"""Real-tmux integration checks for native Omnigent project workspaces.

Run: python3 -m unittest discover -s tools/omnigent -p 'test_*.py' -v
"""

from __future__ import annotations

import importlib.util
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch
import uuid


WRAPPER = Path(__file__).with_name("omnigent-tmux.py")
TMUX = shutil.which("tmux")
SERVER = "https://fixture.invalid"
OTHER_SERVER = "https://other-fixture.invalid"
OWNER = "omnigent-tmux-v2"


@unittest.skipUnless(TMUX, "tmux is required for workspace integration checks")
class ProjectWorkspaceIntegrationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.fixture = tempfile.TemporaryDirectory(prefix="omnigent-workspace-test-")
        self.root = Path(self.fixture.name)
        self.native_fixture = tempfile.TemporaryDirectory(
            prefix="omnigent-terminal-test-", dir="/private/tmp" if sys.platform == "darwin" else "/tmp"
        )
        self.native_socket = str(Path(self.native_fixture.name) / "tmux.sock")
        self.runtime_fixture = tempfile.TemporaryDirectory(
            prefix="omt-", dir="/private/tmp" if sys.platform == "darwin" else "/tmp"
        )
        self.socket_name = "omnigent-test-" + uuid.uuid4().hex[:12]
        self.use_default_backend = False
        self.family = "test-projects"
        self.status_file = self.root / "status.json"
        self.fail_file = self.root / "fail"
        self.fake = self.root / "omnigent"
        self.fake.write_text(
            "#!/usr/bin/env python3\n"
            "import os, pathlib, sys, time\n"
            "if sys.argv[1:3] == ['host', 'status']:\n"
            "    if pathlib.Path(os.environ['OMNIGENT_TEST_FAIL_FILE']).exists():\n"
            "        print('Temporary host error', file=sys.stderr); sys.exit(2)\n"
            "    print(pathlib.Path(os.environ['OMNIGENT_TEST_STATUS_FILE']).read_text())\n"
            "elif sys.argv[1:2] == ['attach']:\n"
            "    print('FAKE_ATTACHED ' + sys.argv[2], flush=True)\n"
            "    while True: time.sleep(1)\n"
            "else:\n"
            "    print('Unexpected fake omnigent command: ' + repr(sys.argv), file=sys.stderr)\n"
            "    sys.exit(2)\n",
            encoding="utf-8",
        )
        self.fake.chmod(0o755)
        # The multi-server case must not attach to real Omnigent sockets on
        # this host. Delegate all outer operations and only our native socket.
        fake_tmux = self.root / "tmux"
        fake_tmux.write_text(
            "#!/usr/bin/env python3\n"
            "import os, sys\n"
            f"native_socket = {self.native_socket!r}\n"
            f"real_tmux = {TMUX!r}\n"
            "if '-S' in sys.argv:\n"
            "    requested = sys.argv[sys.argv.index('-S') + 1]\n"
            "    if requested != native_socket: sys.exit(1)\n"
            "os.execv(real_tmux, [real_tmux, *sys.argv[1:]])\n",
            encoding="utf-8",
        )
        fake_tmux.chmod(0o755)
        self.env = os.environ.copy()
        self.env.pop("TMUX", None)
        self.env.pop("TMUX_PANE", None)
        self.env.update(
            PATH=str(self.root) + os.pathsep + self.env.get("PATH", ""),
            OMNIGENT_TEST_STATUS_FILE=str(self.status_file),
            OMNIGENT_TEST_FAIL_FILE=str(self.fail_file),
            TMUX_TMPDIR=self.runtime_fixture.name,
        )
        self.native("new-session", "-d", "-s", "main", "-n", "native", "cat")
        self.native("set-option", "-g", "prefix", "None")
        self.native("set-option", "-g", "status", "off")
        self.native("set-option", "-g", "@omnigent-conversation-link", SERVER + "/c/fixture-native")
        self.projects = {"project-a": "Project Alpha", "project-b": "Project Beta"}
        self.sessions = [
            self.session("fixture-native", "Native Alpha", "codex-native-ui", "project-a"),
            self.session("fixture-chat", "Chat Alpha", "polly", "project-a"),
            self.session("fixture-beta", "Chat Beta", "polly", "project-b"),
            self.session("fixture-unfiled", "Unfiled chat", "polly"),
        ]
        self.extra_daemons = []
        self.write_status()

    def tearDown(self) -> None:
        self.outer("kill-server", check=False)
        self.native("kill-server", check=False)
        self.native_fixture.cleanup()
        self.runtime_fixture.cleanup()
        self.fixture.cleanup()

    def session(self, session_id: str, title: str, agent: str, project_id: str | None = None) -> dict:
        return dict(
            id=session_id, title=title, agent_name=agent, host_id="fixture-host",
            runner_id="runner-" + session_id, runner_online=True, status="idle", archived=False,
            project_id=project_id, project_name=self.projects.get(project_id), workspace=str(self.root),
            labels={"omnigent.wrapper": "codex-native-ui"} if agent == "codex-native-ui" else {},
        )

    @staticmethod
    def daemon(server: str, sessions: list[dict], projects: dict) -> dict:
        return dict(
            target=server, mode="remote", server_url=server, process="online", host_status="online",
            host_id="fixture-host", sessions=sessions, projects=projects, error=None,
        )

    def write_status(self) -> None:
        payload = dict(daemons=[self.daemon(SERVER, self.sessions, self.projects), *self.extra_daemons])
        temporary = self.status_file.with_suffix(".new")
        temporary.write_text(json.dumps(payload), encoding="utf-8")
        temporary.replace(self.status_file)

    def native(self, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [TMUX, "-S", self.native_socket, "-f", os.devnull, *args],
            text=True, capture_output=True, check=check, timeout=10, env=self.env,
        )

    def outer(self, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
        prefix = [TMUX] if self.use_default_backend else [TMUX, "-L", self.socket_name]
        result = subprocess.run(
            [*prefix, "-f", os.devnull, *args],
            text=True, capture_output=True, check=False, timeout=10, env=self.env,
        )
        if check and result.returncode:
            self.fail(f"tmux {args!r} failed: {result.stderr.strip()}")
        return result

    def cli(self, *args: str, check: bool = True, filter_server: bool = True, tmux_config: str = os.devnull) -> subprocess.CompletedProcess[str]:
        argv = [sys.executable, str(WRAPPER), "--workspace", self.family, "--interval", "1", "--tmux-config", tmux_config]
        if not self.use_default_backend:
            argv += ["--socket-name", self.socket_name]
        if filter_server:
            argv += ["--server", SERVER]
        result = subprocess.run(argv + list(args), text=True, capture_output=True, timeout=20, env=self.env)
        if check and result.returncode:
            self.fail(f"CLI {args!r} failed: {result.stderr.strip()}\n{result.stdout.strip()}")
        return result

    def groups(self) -> dict[str, dict]:
        result = self.outer(
            "list-sessions", "-F", "#{session_id}\t#{session_name}\t#{@omnigent-owner}\t#{@omnigent-family}\t#{@omnigent-project-key}", check=False
        )
        groups = {}
        for line in result.stdout.splitlines():
            fields = line.split("\t")
            if len(fields) == 5 and fields[2] == OWNER and fields[3] == self.family:
                groups[fields[4]] = dict(id=fields[0], name=fields[1])
        return groups

    def windows(self) -> dict[str, dict]:
        result = self.outer(
            "list-windows", "-a", "-F", "#{session_id}\t#{window_id}\t#{@omnigent-session}\t#{@omnigent-pane}", check=False
        )
        windows = {}
        for line in result.stdout.splitlines():
            fields = line.split("\t")
            if len(fields) == 4 and fields[2]:
                windows[fields[2]] = dict(group=fields[0], id=fields[1], pane=fields[3])
        return windows

    @staticmethod
    def project_key(project_id: str, server: str = SERVER) -> str:
        return server + "/projects/" + project_id

    @staticmethod
    def session_key(session_id: str, server: str = SERVER) -> str:
        return server + "/c/" + session_id

    def window(self, session_id: str, server: str = SERVER) -> dict | None:
        return self.windows().get(self.session_key(session_id, server))

    def capture(self, pane: str) -> str:
        return self.outer("capture-pane", "-p", "-t", pane, check=False).stdout

    def wait_until(self, predicate, explanation: str, timeout: float = 12) -> None:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if predicate():
                return
            time.sleep(0.1)
        self.fail(explanation + "; groups=" + repr(self.groups()) + "; windows=" + repr(self.windows()))

    def start(self, filter_server: bool = True) -> None:
        self.cli("--detach", filter_server=filter_server)
        self.wait_until(
            lambda: all(self.window(sid) and self.window(sid)["pane"] for sid in ("fixture-native", "fixture-chat", "fixture-beta", "fixture-unfiled"))
            and "FAKE_ATTACHED fixture-chat" in self.capture(self.window("fixture-chat")["pane"]),
            "project chat viewer did not start",
        )

    def split_shell(self, window: dict) -> str:
        return self.outer(
            "split-window", "-h", "-t", window["pane"], "-P", "-F", "#{pane_id}", "cat"
        ).stdout.strip()

    def test_groups_use_actual_project_ids_and_unfiled_membership(self) -> None:
        different = self.root / "different-folder"
        different.mkdir()
        self.sessions[1]["workspace"] = str(different)
        self.start()
        self.wait_until(lambda: len(self.windows()) == 4, "expected agent windows were not discovered")
        groups = self.groups()
        self.assertEqual(set(groups), {self.project_key("project-a"), self.project_key("project-b"), "unfiled"})
        self.assertEqual(self.window("fixture-native")["group"], groups[self.project_key("project-a")]["id"])
        self.assertEqual(self.window("fixture-chat")["group"], groups[self.project_key("project-a")]["id"])
        self.assertEqual(self.window("fixture-beta")["group"], groups[self.project_key("project-b")]["id"])
        self.assertEqual(self.window("fixture-unfiled")["group"], groups["unfiled"]["id"])
        self.assertIn("alpha", groups[self.project_key("project-a")]["name"].lower())

    def test_duplicate_project_names_on_different_servers_are_separate(self) -> None:
        other = self.session("fixture-other", "Other server chat", "polly", "project-a")
        self.extra_daemons.append(self.daemon(OTHER_SERVER, [other], {"project-a": "Project Alpha"}))
        self.write_status()
        self.start(filter_server=False)
        self.wait_until(lambda: self.window("fixture-other", OTHER_SERVER), "other server session was not added")
        groups = self.groups()
        first = groups[self.project_key("project-a")]
        other_group = groups[self.project_key("project-a", OTHER_SERVER)]
        self.assertNotEqual(first["id"], other_group["id"])
        self.assertNotEqual(first["name"], other_group["name"])
        self.assertEqual(self.window("fixture-other", OTHER_SERVER)["group"], other_group["id"])

    def test_default_native_view_is_interactive_and_tracks_split_resize(self) -> None:
        self.start()
        self.wait_until(lambda: self.window("fixture-native"), "native window did not start")
        window = self.window("fixture-native")
        command = self.outer("display-message", "-p", "-t", window["pane"], "#{pane_start_command}").stdout
        self.assertNotIn("_view", command)
        self.assertNotIn("ignore-size", command)
        self.assertNotIn("attach -r", command)
        self.outer("send-keys", "-t", window["pane"], "-l", "NATIVE_USER_INPUT")
        self.outer("send-keys", "-t", window["pane"], "Enter")
        self.wait_until(lambda: "NATIVE_USER_INPUT" in self.native("capture-pane", "-p", "-t", "main").stdout, "default viewer dropped user input")
        shell = self.split_shell(window)
        self.outer("resize-pane", "-t", window["pane"], "-x", "55")
        self.wait_until(
            lambda: self.outer("display-message", "-p", "-t", window["pane"], "#{pane_width}x#{pane_height}").stdout.strip()
            == self.native("display-message", "-p", "-t", "main", "#{pane_width}x#{pane_height}").stdout.strip(),
            "native terminal did not follow outer-pane resize",
        )
        flags = self.native("list-clients", "-F", "#{client_flags}").stdout
        self.assertNotIn("read-only", flags)
        self.assertNotIn("ignore-size", flags)
        self.assertEqual(self.outer("display-message", "-p", "-t", shell, "#{pane_dead}").stdout.strip(), "0")

    def test_project_reassignment_moves_existing_window_and_user_panes(self) -> None:
        self.start()
        self.wait_until(lambda: self.window("fixture-native"), "native window did not start")
        original = self.window("fixture-native")
        shell = self.split_shell(original)
        pid = self.outer("display-message", "-p", "-t", original["pane"], "#{pane_pid}").stdout.strip()
        self.sessions[0]["project_id"] = "project-b"
        self.sessions[0]["project_name"] = "Project Beta"
        self.write_status()
        self.wait_until(
            lambda: self.window("fixture-native") and self.window("fixture-native")["group"] == self.groups()[self.project_key("project-b")]["id"],
            "native session did not move to its new project",
        )
        moved = self.window("fixture-native")
        self.assertEqual((original["id"], original["pane"]), (moved["id"], moved["pane"]))
        self.assertEqual(pid, self.outer("display-message", "-p", "-t", moved["pane"], "#{pane_pid}").stdout.strip())
        self.assertEqual(self.outer("display-message", "-p", "-t", shell, "#{window_id}").stdout.strip(), moved["id"])

    def test_ended_agent_preserves_unrelated_shell_panes(self) -> None:
        self.start()
        self.wait_until(lambda: self.window("fixture-native"), "native window did not start")
        original = self.window("fixture-native")
        shell = self.split_shell(original)
        self.sessions[0]["runner_online"] = False
        self.write_status()
        self.native("kill-session", "-t", "main")
        self.wait_until(lambda: self.window("fixture-native") is None, "ended agent remained managed")
        self.assertEqual(self.outer("display-message", "-p", "-t", shell, "#{pane_dead}").stdout.strip(), "0")
        self.assertEqual(self.outer("display-message", "-p", "-t", shell, "#{window_id}").stdout.strip(), original["id"])

    def test_failed_inventory_and_unknown_connectivity_preserve_groups(self) -> None:
        self.start()
        self.wait_until(lambda: self.window("fixture-native"), "native window did not start")
        original = {key: value.copy() for key, value in self.windows().items()}
        self.fail_file.touch()
        time.sleep(2)
        self.assertEqual(original, self.windows(), "query failure changed project groups or windows")
        self.fail_file.unlink()
        self.sessions[1]["runner_online"] = None
        self.write_status()
        time.sleep(2)
        self.assertEqual(original, self.windows(), "unknown connectivity ended a live agent")
        self.sessions[1]["runner_online"] = True
        self.sessions.append(self.session("fixture-recovered", "Recovered chat", "polly", "project-a"))
        self.write_status()
        self.wait_until(lambda: self.window("fixture-recovered"), "discovery did not recover after inventory failure")

    def test_lifecycle_idempotency_title_safety_and_unmanaged_windows(self) -> None:
        marker = self.root / "shell-was-executed"
        self.sessions[1]["title"] = f"$(touch {marker}) `touch {marker}`; touch {marker}"
        offline = self.session("fixture-offline", "Offline", "polly", "project-a")
        offline["runner_online"] = False
        archived = self.session("fixture-archived", "Archived", "polly", "project-a")
        archived["archived"] = True
        self.sessions.extend([offline, archived])
        self.write_status()
        self.start()
        self.wait_until(lambda: len(self.windows()) == 4, "expected windows were not discovered")
        before = self.windows()
        self.cli("--detach")
        self.assertEqual(before, self.windows())
        group = self.groups()[self.project_key("project-a")]["id"]
        manual = self.outer("new-window", "-d", "-t", group + ":", "-n", "personal", "-P", "-F", "#{window_id}", "cat").stdout.strip()
        self.sessions.append(self.session("fixture-added", "Added", "polly", "project-a"))
        self.write_status()
        self.wait_until(lambda: self.window("fixture-added"), "new agent was not added")
        self.sessions = [row for row in self.sessions if row["id"] != "fixture-chat"]
        self.write_status()
        self.wait_until(lambda: self.window("fixture-chat") is None, "ended chat window was not removed")
        self.assertEqual(self.outer("display-message", "-p", "-t", manual, "#{pane_dead}").stdout.strip(), "0")
        self.assertFalse(marker.exists())
        self.assertIsNone(self.window("fixture-offline"))
        self.assertIsNone(self.window("fixture-archived"))

    def test_stop_is_scoped_to_family_and_leaves_native_agents_running(self) -> None:
        sibling = self.family + "-extra"
        self.outer("new-session", "-d", "-s", sibling, "-n", "personal", "cat")
        self.outer("set-option", "-t", "=" + sibling + ":", "@omnigent-owner", OWNER)
        self.outer("set-option", "-t", "=" + sibling + ":", "@omnigent-family", self.family + "-other")
        self.cli("--stop")
        self.assertIn(sibling, self.outer("list-sessions", "-F", "#{session_name}").stdout.splitlines())
        self.start()
        self.cli("--stop")
        self.assertFalse(self.groups())
        self.assertIn(sibling, self.outer("list-sessions", "-F", "#{session_name}").stdout.splitlines())
        self.assertEqual(self.native("has-session", "-t", "main").returncode, 0)

    def test_overview_can_be_renamed_or_closed_and_reopened(self) -> None:
        self.start()
        before = self.window("fixture-chat")
        unfiled = self.groups()["unfiled"]["id"]
        controller = self.outer("show-options", "-qv", "-t", unfiled + ":", "@omnigent-controller").stdout.strip()
        self.assertTrue(controller)
        self.outer("rename-window", "-t", controller, "renamed-overview")
        self.cli("--detach")
        self.assertEqual(controller, self.outer("show-options", "-qv", "-t", unfiled + ":", "@omnigent-controller").stdout.strip())
        self.outer("kill-window", "-t", controller)
        self.cli("--detach")
        restored = self.outer("show-options", "-qv", "-t", unfiled + ":", "@omnigent-controller").stdout.strip()
        self.assertNotEqual(controller, restored)
        self.sessions.append(self.session("fixture-reopened", "Reopened", "polly", "project-b"))
        self.write_status()
        self.wait_until(lambda: self.window("fixture-reopened"), "reopened controller did not resume discovery")
        self.assertEqual(before, self.window("fixture-chat"))

    def test_active_overview_shell_survives_reopen_without_duplicate_watchers(self) -> None:
        self.start()
        root = self.groups()["unfiled"]["id"]
        controller = self.outer("show-options", "-qv", "-t", root + ":", "@omnigent-controller").stdout.strip()
        watcher = self.outer("show-options", "-qv", "-t", root + ":", "@omnigent-controller-pane").stdout.strip()
        self.assertTrue(watcher)
        shell = self.split_shell(dict(pane=watcher))
        self.outer("select-pane", "-t", shell)
        shell_pid = self.outer("display-message", "-p", "-t", shell, "#{pane_pid}").stdout.strip()
        watcher_pid = self.outer("display-message", "-p", "-t", watcher, "#{pane_pid}").stdout.strip()
        self.cli("--detach")
        self.assertEqual(shell_pid, self.outer("display-message", "-p", "-t", shell, "#{pane_pid}").stdout.strip())
        self.assertEqual(watcher_pid, self.outer("display-message", "-p", "-t", watcher, "#{pane_pid}").stdout.strip())
        commands = self.outer("list-panes", "-a", "-F", "#{pane_start_command}").stdout.splitlines()
        self.assertEqual(sum("_watch" in command for command in commands), 1)
        self.outer("kill-pane", "-t", watcher)
        self.cli("--detach")
        restored = self.outer("show-options", "-qv", "-t", root + ":", "@omnigent-controller-pane").stdout.strip()
        self.assertNotEqual(watcher, restored)
        self.assertEqual(shell_pid, self.outer("display-message", "-p", "-t", shell, "#{pane_pid}").stdout.strip())
        self.assertEqual(controller, self.outer("display-message", "-p", "-t", restored, "#{window_id}").stdout.strip())
        commands = self.outer("list-panes", "-a", "-F", "#{pane_start_command}").stdout.splitlines()
        self.assertEqual(sum("_watch" in command for command in commands), 1)

    def test_sole_shell_survives_when_managed_pane_was_closed_before_agent_ended(self) -> None:
        self.start()
        original = self.window("fixture-native")
        shell = self.split_shell(original)
        shell_pid = self.outer("display-message", "-p", "-t", shell, "#{pane_pid}").stdout.strip()
        root = self.groups()["unfiled"]["id"]
        watcher = self.outer("show-options", "-qv", "-t", root + ":", "@omnigent-controller-pane").stdout.strip()
        # Pause discovery using an ordinary tmux respawn, then reopen after
        # the fixture is offline so no stale live snapshot can restore it.
        self.outer("respawn-pane", "-k", "-t", watcher, "sleep 60")
        self.outer("kill-pane", "-t", original["pane"])
        self.sessions[0]["runner_online"] = False
        self.write_status()
        self.native("kill-session", "-t", "main")
        self.cli("--detach")
        self.wait_until(lambda: self.window("fixture-native") is None, "offline agent window remained managed")
        self.assertEqual(shell_pid, self.outer("display-message", "-p", "-t", shell, "#{pane_pid}").stdout.strip())
        self.assertEqual(original["id"], self.outer("display-message", "-p", "-t", shell, "#{window_id}").stdout.strip())
        self.assertEqual("0", self.outer("display-message", "-p", "-t", shell, "#{pane_dead}").stdout.strip())

    def test_project_selector_accepts_spaces_and_prefers_exact_labels(self) -> None:
        self.projects.update({"project-a": "My Project", "project-b": "My Project Archive"})
        for row in self.sessions:
            row["project_name"] = self.projects.get(row["project_id"])
        self.write_status()
        self.start()
        spec = importlib.util.spec_from_file_location("omnigent_tmux_under_test", WRAPPER)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)

        def choose(selector: str) -> dict:
            options = module.parse([
                "--workspace", self.family, "--socket-name", self.socket_name,
                "--tmux-config", os.devnull, "--server", SERVER, "--interval", "1", "--project", selector,
            ])
            with patch.dict(os.environ, self.env, clear=True):
                return module.Workspace(options).select_project()

        groups = self.groups()
        alpha = groups[self.project_key("project-a")]
        beta = groups[self.project_key("project-b")]
        self.assertEqual(alpha["id"], choose("My Project")["session"])
        self.assertEqual(alpha["id"], choose("my project")["session"])
        self.assertEqual(beta["id"], choose("My Project Archive")["session"])
        self.assertEqual(alpha["id"], choose("project-a")["session"])
        self.assertEqual(alpha["id"], choose(alpha["name"])["session"])
        with self.assertRaises(ValueError):
            choose("Project")

    def test_list_and_missing_native_terminal_do_not_start_agents(self) -> None:
        self.sessions.append(self.session("fixture-waiting", "Waiting native", "codex-native-ui", "project-a"))
        self.write_status()
        listed = self.cli("--list")
        self.assertIn("Waiting native", listed.stdout)
        self.assertFalse(self.groups())
        self.start()
        time.sleep(1.2)
        self.assertIsNone(self.window("fixture-waiting"))

    def test_legacy_migration_preserves_custom_windows_and_panes(self) -> None:
        proxy = shlex.join([sys.executable, "-c", "import time; time.sleep(60)", "_view"])
        controller = self.outer("new-session", "-d", "-s", self.family, "-n", "overview", "-P", "-F", "#{window_id}", "sleep 60").stdout.strip()
        legacy = "=" + self.family + ":"
        self.outer("set-option", "-t", legacy, "@omnigent-owner", "omnigent-tmux-v1")
        self.outer("set-option", "-t", legacy, "@omnigent-controller", controller)
        self.outer("set-option", "-t", legacy, "@omnigent-config", json.dumps(dict(server=SERVER, interval=1.0, interactive=False), sort_keys=True))
        managed = self.outer("new-window", "-d", "-t", legacy, "-n", "legacy-native", "-P", "-F", "#{window_id}", proxy).stdout.strip()
        self.outer("set-option", "-w", "-t", managed, "@omnigent-session", self.session_key("fixture-native"))
        self.outer("set-option", "-w", "-t", managed, "@omnigent-mode", "terminal")
        self.outer("set-option", "-w", "-t", managed, "@omnigent-source", self.native_socket)
        pane = self.outer("list-panes", "-t", managed, "-F", "#{pane_id}").stdout.strip()
        shell = self.split_shell(dict(pane=pane))
        personal = self.outer("new-window", "-d", "-t", legacy, "-n", "personal", "-P", "-F", "#{window_id}", "cat").stdout.strip()
        self.start()
        self.wait_until(lambda: self.window("fixture-native"), "legacy native window was not migrated")
        migrated = self.window("fixture-native")
        self.assertEqual(managed, migrated["id"])
        self.assertEqual(pane, migrated["pane"])
        self.assertEqual(self.outer("display-message", "-p", "-t", shell, "#{pane_dead}").stdout.strip(), "0")
        self.assertEqual(self.outer("display-message", "-p", "-t", personal, "#{pane_dead}").stdout.strip(), "0")
        command = self.outer("display-message", "-p", "-t", migrated["pane"], "#{pane_start_command}").stdout
        self.assertNotIn("_view", command)
        self.outer("send-keys", "-t", migrated["pane"], "-l", "MIGRATED_NATIVE_INPUT")
        self.outer("send-keys", "-t", migrated["pane"], "Enter")
        self.wait_until(lambda: "MIGRATED_NATIVE_INPUT" in self.native("capture-pane", "-p", "-t", "main").stdout, "migration left the native viewer output-only")

    def test_legacy_migration_refuses_an_incompatible_server_filter(self) -> None:
        controller = self.outer(
            "new-session", "-d", "-s", self.family, "-n", "overview", "-P", "-F", "#{window_id}", "sleep 60"
        ).stdout.strip()
        target = "=" + self.family + ":"
        self.outer("set-option", "-t", target, "@omnigent-owner", "omnigent-tmux-v1")
        self.outer("set-option", "-t", target, "@omnigent-controller", controller)
        self.outer("set-option", "-t", target, "@omnigent-config", json.dumps(dict(server=OTHER_SERVER, interval=1.0, interactive=False)))
        panes = self.outer("list-panes", "-t", controller, "-F", "#{pane_id}\t#{pane_pid}").stdout
        result = self.cli("--detach", check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("different server filter", result.stderr)
        self.assertFalse(self.groups())
        self.assertEqual(panes, self.outer("list-panes", "-t", controller, "-F", "#{pane_id}\t#{pane_pid}").stdout)
        self.assertEqual("omnigent-tmux-v1", self.outer("show-options", "-qv", "-t", target, "@omnigent-owner").stdout.strip())

    def test_explicit_tmux_config_is_loaded_and_preserved(self) -> None:
        config = self.root / "tmux.conf"
        config.write_text(
            'set -g prefix C-a\n'
            'unbind C-b\n'
            'bind-key C-a send-prefix\n'
            'set -g status-style "bg=colour17,fg=colour231"\n'
            'set -g status-left "MY CONFIG"\n'
            'set -g status-right "MY RIGHT"\n'
            'set -g base-index 4\n'
            'setw -g pane-base-index 2\n'
            'setw -g mode-keys vi\n'
            'set -g history-limit 12345\n'
            'set -g mouse off\n'
            'bind-key X display-message "custom configuration binding"\n',
            encoding="utf-8",
        )
        self.cli("--detach", tmux_config=str(config))
        self.wait_until(lambda: self.window("fixture-chat"), "custom-config workspace was not populated")
        self.assertEqual("C-a", self.outer("show-options", "-gv", "prefix").stdout.strip())
        style = self.outer("show-options", "-gv", "status-style").stdout
        self.assertIn("bg=colour17", style)
        self.assertIn("fg=colour231", style)
        self.assertEqual("MY CONFIG", self.outer("show-options", "-gv", "status-left").stdout.strip())
        self.assertEqual("MY RIGHT", self.outer("show-options", "-gv", "status-right").stdout.strip())
        self.assertEqual("4", self.outer("show-options", "-gv", "base-index").stdout.strip())
        self.assertEqual("2", self.outer("show-window-options", "-gv", "pane-base-index").stdout.strip())
        self.assertEqual("vi", self.outer("show-window-options", "-gv", "mode-keys").stdout.strip())
        self.assertEqual("12345", self.outer("show-options", "-gv", "history-limit").stdout.strip())
        self.assertEqual("off", self.outer("show-options", "-gv", "mouse").stdout.strip())
        self.assertIn("custom configuration binding", self.outer("list-keys", "-T", "prefix", "X").stdout)
        window = self.window("fixture-chat")
        self.assertEqual("C-a", self.outer("show-options", "-Av", "-t", window["group"] + ":", "prefix").stdout.strip())
        self.assertEqual("vi", self.outer("show-options", "-wAv", "-t", window["id"], "mode-keys").stdout.strip())

    def test_default_backend_uses_the_existing_tmux_server_and_config(self) -> None:
        spec = importlib.util.spec_from_file_location("omnigent_tmux_default_backend", WRAPPER)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        options = module.parse(["--workspace", self.family])
        workspace = module.Workspace(options)
        self.assertEqual([TMUX], workspace.prefix)
        isolated = module.Workspace(module.parse([
            "--workspace", self.family, "--socket-name", self.socket_name, "--tmux-config", os.devnull,
        ]))
        self.assertEqual([TMUX, "-L", self.socket_name, "-f", os.devnull], isolated.prefix)

    def test_synchronous_plugin_startup_can_exceed_five_seconds(self) -> None:
        config = self.root / "slow-plugin.conf"
        config.write_text(
            'run-shell "sleep 6"\n'
            'set -g @plugin-loaded yes\n'
            'set -g prefix C-a\n',
            encoding="utf-8",
        )
        started = time.monotonic()
        self.cli("--detach", tmux_config=str(config))
        self.assertGreaterEqual(time.monotonic() - started, 6)
        self.assertEqual("yes", self.outer("show-options", "-gv", "@plugin-loaded").stdout.strip())
        self.assertEqual("C-a", self.outer("show-options", "-gv", "prefix").stdout.strip())
        self.wait_until(
            lambda: all(self.window(sid) and self.window(sid)["pane"] for sid in ("fixture-native", "fixture-chat", "fixture-beta", "fixture-unfiled")),
            "project windows did not populate after slow plugin startup",
        )
        root = self.groups()["unfiled"]["id"]
        watcher = self.outer("show-options", "-qv", "-t", root + ":", "@omnigent-controller-pane").stdout.strip()
        command = self.outer("display-message", "-p", "-t", watcher, "#{pane_start_command}").stdout
        self.assertIn("--tmux-config", command)
        self.assertIn(str(config), command)

    def test_default_server_stop_preserves_an_existing_unmanaged_session(self) -> None:
        self.use_default_backend = True
        personal = self.outer(
            "new-session", "-d", "-s", "personal-default", "-n", "shell", "-P", "-F", "#{session_id}\t#{pane_id}\t#{pane_pid}", "cat"
        ).stdout.strip().split("\t")
        session, pane, pid = personal
        self.start()
        self.assertTrue(self.groups())
        self.cli("--stop")
        self.assertFalse(self.groups())
        self.assertEqual("personal-default", self.outer("display-message", "-p", "-t", pane, "#{session_name}").stdout.strip())
        self.assertEqual(pid, self.outer("display-message", "-p", "-t", pane, "#{pane_pid}").stdout.strip())
        self.assertEqual(session, self.outer("display-message", "-p", "-t", pane, "#{session_id}").stdout.strip())
        self.assertEqual(self.native("has-session", "-t", "main").returncode, 0)


if __name__ == "__main__":
    unittest.main()

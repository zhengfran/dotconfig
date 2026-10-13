#!/usr/bin/env python3
"""Native tmux sessions grouped by this host's Omnigent projects."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import socket
import stat
import subprocess
import sys
import tempfile
import time
from urllib.parse import urlsplit, urlunsplit

SCRIPT = str(Path(__file__).resolve())
OWNER = "omnigent-tmux-v2"
LEGACY_OWNER = "omnigent-tmux-v1"
RETRY_SECONDS = 30
TMUX_STARTUP_TIMEOUT = 120
IDLE = shlex.join([sys.executable, "-c", "import time; time.sleep(86400)"])

# Use the CLI's own interpreter and authenticated GET helper. Credentials stay
# inside Omnigent; stdout contains only the metadata needed for grouping.
METADATA_HELPER = r'''
import concurrent.futures, json, sys
from omnigent.cli import _host_http_json
request = json.load(sys.stdin)
def read_server(pair):
    server, wanted = pair
    output = {"projects": {}, "sessions": {}}
    errors = []
    if wanted["projects"]:
        try:
            result = _host_http_json(base_url=server, method="GET", path="/v1/projects")
            if result.status_code == 200 and isinstance(result.body, dict):
                output["projects"] = {p["id"]: p["name"] for p in result.body.get("data", [])}
            else:
                errors.append("Project names unavailable on " + server)
        except Exception:
            errors.append("Project names unavailable on " + server)
    for sid in wanted["sessions"]:
        try:
            result = _host_http_json(base_url=server, method="GET", path="/v1/sessions/" + sid)
            if result.status_code == 200 and isinstance(result.body, dict):
                row = result.body
                output["sessions"][sid] = {k: row.get(k) for k in ("id", "title", "project_id", "workspace")}
            else:
                errors.append("Terminal project membership unavailable: " + sid[:8])
        except Exception:
            errors.append("Terminal project membership unavailable: " + sid[:8])
    return server, output, errors
with concurrent.futures.ThreadPoolExecutor() as pool:
    results = list(pool.map(read_server, request.items()))
print(json.dumps({"servers": {s: d for s, d, _ in results}, "errors": [e for _, _, es in results for e in es]}))
'''


def clean(value: object, limit: int = 120) -> str:
    return " ".join("".join(c for c in str(value or "") if c.isprintable()).split()).replace("#", "_")[:limit]


def run(argv: list[str], timeout: float = 5, input_text: str | None = None) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env.pop("TMUX", None)
    env.pop("TMUX_PANE", None)
    return subprocess.run(argv, capture_output=True, text=True, input=input_text, env=env, timeout=timeout)


def native_terminals(tmux: str, server_filter: str | None) -> tuple[dict[str, dict], set[str]]:
    roots = {Path(p).resolve() for p in (tempfile.gettempdir(), "/tmp", "/private/tmp") if Path(p).is_dir()}
    sessions: dict[str, dict] = {}
    uncertain: set[str] = set()
    for path in sorted({p.resolve() for root in roots for p in root.glob("omnigent-terminal-*/tmux.sock")}):
        try:
            info = path.stat()
            if info.st_uid != os.getuid() or not stat.S_ISSOCK(info.st_mode):
                continue
            panes = run([tmux, "-S", str(path), "list-panes", "-t", "main", "-F", "#{pane_dead}"], 2)
            if panes.returncode or "0" not in panes.stdout.splitlines():
                continue
            link = run([tmux, "-S", str(path), "show-options", "-gqv", "@omnigent-conversation-link"], 2)
            parsed = urlsplit(link.stdout.strip())
            match = re.fullmatch(r"(.*)/c/([A-Za-z0-9_-]+)", parsed.path)
            if link.returncode or parsed.scheme not in ("http", "https") or not match:
                continue
            server = urlunsplit((parsed.scheme, parsed.netloc, match[1], parsed.query, "")).rstrip("/")
            if server_filter and server != server_filter:
                continue
            sid = match[2]
            key = server + "/c/" + sid
            sessions[key] = dict(key=key, id=sid, server=server, title=sid[:8], agent="terminal", status="live", mode="terminal", source=str(path), project_known=False, project_id=None, project_name=None, workspace=None)
        except (OSError, subprocess.TimeoutExpired):
            uncertain.add(str(path))
    return sessions, uncertain


def apply_metadata(item: dict, row: dict) -> None:
    item.update(project_known=True, project_id=row.get("project_id") or None, project_name=clean(row["project_name"]) if row.get("project_name") else None, workspace=row.get("workspace"))
    if row.get("title"):
        item["title"] = clean(row["title"])


def remote_metadata(omnigent: str, requests: dict) -> dict:
    if not requests:
        return dict(servers={}, errors=[])
    try:
        first_line = Path(omnigent).resolve().open(encoding="utf-8").readline().strip()
        if not first_line.startswith("#!"):
            raise ValueError("Omnigent CLI has no Python interpreter shebang")
        interpreter = shlex.split(first_line[2:])
        result = run(interpreter + ["-c", METADATA_HELPER], 45, json.dumps(requests))
        if result.returncode:
            raise ValueError("Omnigent project lookup failed")
        data = json.loads(result.stdout)
        if not isinstance(data.get("servers"), dict):
            raise ValueError("Omnigent project lookup returned an invalid response")
        return data
    except (OSError, subprocess.TimeoutExpired, ValueError, UnicodeError):
        return dict(servers={}, errors=["Project metadata unavailable; existing project groups are retained."])


def discover(tmux: str, omnigent: str, server_filter: str | None) -> dict:
    terminals, uncertain = native_terminals(tmux, server_filter)
    sessions = dict(terminals)
    confirmed: set[str] = set()
    errors: list[str] = []
    projects: dict[str, dict] = {}
    servers = {item["server"] for item in terminals.values()}
    args = [omnigent, "host", "status", "--sessions", "--json"]
    args += ["--server", server_filter] if server_filter else ["--all"]
    try:
        result = run(args, 45)
        if result.returncode:
            raise ValueError("Omnigent host inventory unavailable")
        payload = json.loads(result.stdout)
        if not isinstance(payload.get("daemons"), list):
            raise ValueError("Omnigent host inventory has no daemon list")
        for daemon in payload["daemons"]:
            server = str(daemon.get("server_url") or "").rstrip("/")
            if not server.startswith(("http://", "https://")) or (server_filter and server != server_filter):
                continue
            servers.add(server)
            if isinstance(daemon.get("projects"), dict):
                projects[server] = daemon["projects"]
            if daemon.get("error") or not isinstance(daemon.get("sessions"), list):
                errors.append("Session inventory unavailable on " + clean(server))
                continue
            rows = daemon["sessions"]
            if not any(row.get("runner_online") is None and row.get("runner_id") for row in rows):
                confirmed.add(server)
            for row in rows:
                sid = str(row.get("id") or "")
                if not re.fullmatch(r"[A-Za-z0-9_-]+", sid):
                    continue
                key = server + "/c/" + sid
                # A standalone native terminal can have metadata even when
                # it is absent from the daemon's connected runner list.
                if key in terminals:
                    apply_metadata(sessions[key], row)
                    sessions[key]["agent"] = clean(row.get("agent_name") or "terminal")
                    sessions[key]["status"] = clean(row.get("status") or "live")
                if row.get("archived") or row.get("runner_online") is not True:
                    continue
                if key in terminals:
                    continue
                agent = clean(row.get("agent_name") or "agent")
                native = "native" in str((row.get("labels") or {}).get("omnigent.wrapper") or "") or agent.endswith("native-ui")
                item = dict(key=key, id=sid, server=server, title=clean(row.get("title") or sid[:8]), agent=agent, status=clean(row.get("status") or "live"), mode="waiting" if native else "repl", source=key)
                apply_metadata(item, row)
                sessions[key] = item
    except (OSError, subprocess.TimeoutExpired, ValueError, TypeError) as exc:
        errors.append(clean(exc, 180))
    requests = {}
    for server in servers:
        missing = [item["id"] for item in sessions.values() if item["server"] == server and not item["project_known"]]
        needs_names = server not in projects and any(item["server"] == server and (not item["project_known"] or (item["project_id"] and not item["project_name"])) for item in sessions.values())
        if missing or needs_names:
            requests[server] = dict(projects=needs_names, sessions=missing)
    metadata = remote_metadata(omnigent, requests)
    errors += metadata.get("errors", [])
    for server, data in metadata["servers"].items():
        projects.setdefault(server, {}).update(data.get("projects", {}))
        for sid, row in data.get("sessions", {}).items():
            key = server + "/c/" + sid
            if key in sessions:
                apply_metadata(sessions[key], row)
    for item in sessions.values():
        if item["project_known"] and item["project_id"]:
            item["project_name"] = clean(projects.get(item["server"], {}).get(item["project_id"]) or item["project_name"]) or None
    return dict(sessions=sessions, confirmed=confirmed, uncertain=uncertain, errors=errors)


def group_for(item: dict) -> tuple[str, str | None]:
    if not item["project_known"]:
        return "pending", "Pending project lookup"
    if not item["project_id"]:
        return "unfiled", "Unfiled"
    return item["server"] + "/projects/" + str(item["project_id"]), item["project_name"]


class Workspace:
    def __init__(self, options: argparse.Namespace):
        self.options = options
        self.tmux = shutil.which("tmux") or ""
        self.omnigent = shutil.which("omnigent") or ""
        if not self.tmux or (not self.omnigent and not options.stop):
            raise ValueError("Install tmux, omnigent, and Python 3 before using this wrapper.")
        # Default to the user's regular server/config so native session
        # pickers, bindings, themes, and plugins see these project sessions.
        self.prefix = [self.tmux]
        if options.socket_name:
            self.prefix += ["-L", options.socket_name]
        if options.tmux_config:
            self.prefix += ["-f", options.tmux_config]

    def call(self, *args: str, check: bool = True) -> str:
        # A user's normal config can synchronously initialize plugins. Killing
        # its first client mid-load can deadlock tmux's configuration queue.
        timeout = TMUX_STARTUP_TIMEOUT if args and args[0] == "new-session" else 5
        result = run(self.prefix + list(args), timeout)
        if check and result.returncode:
            raise ValueError(clean(result.stderr or "tmux command failed", 180))
        return result.stdout.rstrip("\n")

    def all_sessions(self) -> list[dict]:
        output = self.call("list-sessions", "-F", "#{session_id}\t#{session_name}\t#{@omnigent-owner}\t#{@omnigent-family}\t#{@omnigent-project-key}\t#{@omnigent-auto-name}\t#{@omnigent-project-name}", check=False)
        rows = []
        for line in output.splitlines():
            parts = line.split("\t")
            if len(parts) == 7:
                sid, name, owner, family, group, auto, project_name = parts
                rows.append(dict(session=sid, name=name, owner=owner, family=family, group=group, auto=auto, project_name=project_name))
        return rows

    def groups(self) -> dict[str, dict]:
        return {row["group"]: row for row in self.all_sessions() if row["owner"] == OWNER and row["family"] == self.options.workspace}

    def option(self, session: str, name: str) -> str:
        return self.call("show-options", "-qv", "-t", session + ":", "@omnigent-" + name)

    def set_option(self, session: str, name: str, value: str) -> None:
        self.call("set-option", "-t", session + ":", "@omnigent-" + name, value)

    def configuration(self) -> str:
        return json.dumps(dict(server=self.options.server, interval=self.options.interval), sort_keys=True)

    def watch_command(self) -> str:
        args = [sys.executable, SCRIPT, "_watch", "--workspace", self.options.workspace, "--interval", str(self.options.interval)]
        if self.options.socket_name:
            args += ["--socket-name", self.options.socket_name]
        if self.options.tmux_config:
            args += ["--tmux-config", self.options.tmux_config]
        if self.options.server:
            args += ["--server", self.options.server]
        return shlex.join(args)

    def session_name(self, group: str, label: str | None) -> str:
        if group in ("unfiled", "pending"):
            return self.options.workspace + "-" + group
        label = label or "project-" + group.rsplit("/", 1)[-1][:8]
        slug = re.sub(r"[^A-Za-z0-9_-]+", "-", label).strip("-")[:40] or "project"
        return self.options.workspace + "-" + slug + "-" + hashlib.sha256(group.encode()).hexdigest()[:6]

    def mark_group(self, sid: str, group: str, name: str, project_name: str | None = None) -> None:
        for key, value in (("owner", OWNER), ("family", self.options.workspace), ("project-key", group), ("auto-name", name)):
            self.set_option(sid, key, value)
        self.set_option(sid, "project-name", project_name or ("Unfiled" if group == "unfiled" else ""))

    def create_group(self, group: str, label: str | None, cwd: str | None = None) -> dict:
        name = self.session_name(group, label)
        if any(row["name"] == name for row in self.all_sessions()):
            raise ValueError("A tmux session already uses " + name + "; choose another --workspace prefix.")
        location = cwd if isinstance(cwd, str) and Path(cwd).is_dir() else str(Path.home())
        result = self.call("new-session", "-d", "-P", "-F", "#{session_id}\t#{window_id}\t#{pane_id}", "-s", name, "-n", "overview" if group == "unfiled" else "__bootstrap", "-c", location, IDLE)
        sid, wid, pane = result.split("\t")
        self.call("set-option", "-p", "-t", pane, "remain-on-exit", "on")
        self.mark_group(sid, group, name, label)
        self.set_option(sid, "controller" if group == "unfiled" else "bootstrap", wid)
        if group == "unfiled":
            self.set_option(sid, "controller-pane", pane)
        return dict(session=sid, name=name, group=group, auto=name, project_name=label or "")

    def controller_pane(self, sid: str, wid: str) -> str:
        stored = self.option(sid, "controller-pane")
        rows = self.call("list-panes", "-t", wid, "-F", "#{pane_id}\t#{pane_start_command}").splitlines()
        panes = dict(row.split("\t", 1) for row in rows)
        if stored in panes:
            return stored
        # v1 did not record its controller pane. Identify the watcher rather
        # than assuming the active pane belongs to us.
        return next((pane for pane, command in panes.items() if "_watch" in command or command == IDLE), "")

    def legacy(self) -> dict | None:
        names = {self.options.workspace}
        if self.options.workspace == "omnigent":
            names.add("omnigent-host")
        return next((s for s in self.all_sessions() if s["owner"] == LEGACY_OWNER and s["name"] in names), None)

    def ensure(self) -> None:
        groups = self.groups()
        root = groups.get("unfiled")
        if root and self.option(root["session"], "config") != self.configuration():
            raise ValueError("Workspace options differ. Use --stop first, or choose another --workspace prefix.")
        if not root:
            legacy = self.legacy()
            if legacy:
                old_config = json.loads(self.option(legacy["session"], "config") or "{}")
                if old_config.get("server") != self.options.server:
                    raise ValueError("Legacy workspace uses a different server filter; reopen with the same --server before upgrading.")
                target_name = self.session_name("unfiled", "Unfiled")
                if any(s["name"] == target_name for s in self.all_sessions()):
                    raise ValueError("The unfiled tmux session name is already in use.")
                sid = legacy["session"]
                controller = self.option(sid, "controller")
                rows = self.call("list-windows", "-t", sid, "-F", "#{window_id}\t#{window_name}").splitlines()
                if not controller:
                    controller = next((row.split("\t")[0] for row in rows if row.endswith("\toverview")), "")
                if controller and controller in {row.split("\t")[0] for row in rows}:
                    # Stop the old polling loop before migrating any windows.
                    pane = self.controller_pane(sid, controller)
                    if pane:
                        self.call("respawn-pane", "-k", "-t", pane, IDLE)
                        self.set_option(sid, "controller-pane", pane)
                self.call("rename-session", "-t", sid, target_name)
                self.mark_group(sid, "unfiled", target_name)
                self.set_option(sid, "config", self.configuration())
                root = dict(session=sid, name=target_name, group="unfiled", auto=target_name)
            else:
                root = self.create_group("unfiled", "Unfiled")
                self.set_option(root["session"], "config", self.configuration())
        sid = root["session"]
        controller = self.option(sid, "controller")
        ids = self.call("list-windows", "-t", sid, "-F", "#{window_id}").splitlines()
        if controller not in ids:
            result = self.call("new-window", "-d", "-P", "-F", "#{window_id}\t#{pane_id}", "-t", sid + ":", "-n", "overview", self.watch_command())
            controller, pane = result.split("\t")
        else:
            pane = self.controller_pane(sid, controller)
            if not pane:
                pane = self.call("split-window", "-d", "-P", "-F", "#{pane_id}", "-t", controller, self.watch_command())
            else:
                pane_command = self.call("display-message", "-p", "-t", pane, "#{pane_start_command}")
                dead = self.call("display-message", "-p", "-t", pane, "#{pane_dead}")
                if dead == "1" or "_watch" not in pane_command:
                    self.call("respawn-pane", "-k", "-t", pane, self.watch_command())
        self.set_option(sid, "controller", controller)
        self.set_option(sid, "controller-pane", pane)
        self.call("set-option", "-p", "-t", pane, "remain-on-exit", "on")

    def windows(self) -> dict[str, dict]:
        windows = {}
        for group, row in self.groups().items():
            output = self.call("list-windows", "-t", row["session"], "-F", "#{window_id}\t#{window_name}\t#{@omnigent-session}\t#{@omnigent-mode}\t#{@omnigent-source}\t#{@omnigent-retry}\t#{@omnigent-pane}\t#{@omnigent-attachment}\t#{@omnigent-label}")
            for line in output.splitlines():
                parts = line.split("\t")
                if len(parts) == 9 and parts[2]:
                    wid, name, key, mode, source, retry, pane, version, label = parts
                    panes = self.panes(wid)
                    pane = pane or next(iter(panes), "")
                    windows[key] = dict(window=wid, name=name, key=key, mode=mode, source=source, retry=float(retry or 0), pane=pane, version=version, label=label, panes=panes, group=group, session=row["session"])
        return windows

    def panes(self, wid: str) -> dict[str, bool]:
        output = self.call("list-panes", "-t", wid, "-F", "#{pane_id}\t#{pane_dead}")
        return {line.split("\t")[0]: line.split("\t")[1] == "1" for line in output.splitlines()}

    def command(self, item: dict) -> str:
        if item["mode"] == "terminal":
            command = [self.tmux, "-S", item["source"], "attach", "-t", "main"]
        else:
            command = [self.omnigent, "attach", item["id"], "--server", item["server"]]
        return shlex.join(["env", "-u", "TMUX", "-u", "TMUX_PANE", *command])

    def tag(self, wid: str, pane: str, item: dict, label: str) -> None:
        # These settings belong only to managed clients, never to the user's
        # server defaults, existing sessions, theme, or keyboard bindings.
        self.call("set-option", "-p", "-t", pane, "remain-on-exit", "on")
        self.call("set-option", "-w", "-t", wid, "automatic-rename", "off")
        for key, value in (("session", item["key"]), ("mode", item["mode"]), ("source", item["source"]), ("retry", str(time.monotonic() + RETRY_SECONDS)), ("pane", pane), ("attachment", "native-v2"), ("label", label)):
            self.call("set-option", "-w", "-t", wid, "@omnigent-" + key, value)

    def reconcile(self, snapshot: dict) -> None:
        windows = self.windows()
        groups = self.groups()
        for key, item in snapshot["sessions"].items():
            prior = windows.get(key)
            group, project_name = group_for(item)
            if not item["project_known"] and prior:
                group = prior["group"]
            if item["mode"] == "waiting":
                continue
            if group not in groups:
                groups[group] = self.create_group(group, project_name, item.get("workspace"))
            target = groups[group]
            if project_name:
                self.set_option(target["session"], "project-name", project_name)
                target["project_name"] = project_name
            expected_name = self.session_name(group, project_name)
            if project_name and target["name"] == target["auto"] and target["name"] != expected_name:
                if not any(s["name"] == expected_name for s in self.all_sessions()):
                    self.call("rename-session", "-t", target["session"], expected_name)
                    self.set_option(target["session"], "auto-name", expected_name)
                    target.update(name=expected_name, auto=expected_name)
            label = clean(item["title"], 60) + " " + item["id"][:6]
            cwd = item.get("workspace")
            location = cwd if isinstance(cwd, str) and Path(cwd).is_dir() else str(Path.home())
            if not prior:
                result = self.call("new-window", "-d", "-P", "-F", "#{window_id}\t#{pane_id}", "-t", target["session"] + ":", "-n", label, "-c", location, self.command(item))
                wid, pane = result.split("\t")
                self.tag(wid, pane, item, label)
            else:
                wid, pane = prior["window"], prior["pane"]
                if prior["session"] != target["session"]:
                    self.call("move-window", "-d", "-s", wid, "-t", target["session"] + ":")
                # Respect window names the user changed using native tmux.
                if prior["name"] == (prior["label"] or prior["name"]):
                    self.call("rename-window", "-t", wid, label)
                changed = prior["version"] != "native-v2" or (prior["mode"], prior["source"]) != (item["mode"], item["source"])
                if pane not in prior["panes"]:
                    pane = self.call("split-window", "-d", "-P", "-F", "#{pane_id}", "-t", wid, "-c", location, self.command(item))
                    changed = False
                    self.tag(wid, pane, item, label)
                elif changed or (prior["panes"][pane] and time.monotonic() >= prior["retry"]):
                    # Respawn only the agent client; user-created split panes
                    # keep their processes and layout throughout migration.
                    self.call("respawn-pane", "-k", "-t", pane, "-c", location, self.command(item))
                    self.tag(wid, pane, item, label)
                else:
                    self.call("set-option", "-w", "-t", wid, "@omnigent-label", label)
        for key, prior in windows.items():
            if key in snapshot["sessions"]:
                continue
            closed = prior["source"] not in snapshot["uncertain"] if prior["mode"] == "terminal" else key.rsplit("/c/", 1)[0] in snapshot["confirmed"]
            if not closed:
                continue
            if len(prior["panes"]) > 1 or prior["pane"] not in prior["panes"]:
                if prior["pane"] in prior["panes"]:
                    self.call("kill-pane", "-t", prior["pane"])
                self.call("set-option", "-wu", "-t", prior["window"], "@omnigent-session")
            else:
                self.call("kill-window", "-t", prior["window"])
        for group, row in self.groups().items():
            if group == "unfiled":
                continue
            bootstrap = self.option(row["session"], "bootstrap")
            ids = self.call("list-windows", "-t", row["session"], "-F", "#{window_id}").splitlines()
            if bootstrap in ids and len(ids) > 1:
                self.call("kill-window", "-t", bootstrap)

    def stop(self) -> None:
        groups = self.groups()
        root = groups.get("unfiled")
        if root:
            controller = self.option(root["session"], "controller")
            if controller in self.call("list-windows", "-t", root["session"], "-F", "#{window_id}").splitlines():
                pane = self.controller_pane(root["session"], controller)
                if pane:
                    self.call("respawn-pane", "-k", "-t", pane, IDLE)
        for row in groups.values():
            self.call("kill-session", "-t", row["session"])
        legacy = self.legacy()
        if legacy:
            self.call("kill-session", "-t", legacy["session"])

    def select_project(self) -> dict:
        groups = self.groups()
        choices = [row for group, row in groups.items() if group != "unfiled"]
        if self.options.project:
            selector = self.options.project.casefold()
            exact = [row for group, row in groups.items() if selector in (group.rsplit("/", 1)[-1].casefold(), row["name"].casefold(), row.get("project_name", "").casefold())]
            candidates = exact or [row for row in groups.values() if selector in row["name"].casefold() or selector in row.get("project_name", "").casefold()]
            if len(candidates) != 1:
                raise ValueError("Project selection is missing or ambiguous; use the tmux session name or project id.")
            return candidates[0]
        else:
            return sorted(choices, key=lambda row: row["name"])[0] if choices else groups["unfiled"]

    def attach(self) -> None:
        target = self.select_project()
        caller_socket = os.environ.get("TMUX", "").split(",", 1)[0]
        if caller_socket:
            selected_socket = self.call("display-message", "-p", "#{socket_path}")
            if os.path.realpath(caller_socket) == os.path.realpath(selected_socket):
                # Already in this server: switch the native client instead of
                # nesting a second UI inside the user's existing tmux pane.
                result = subprocess.run(self.prefix + ["switch-client", "-t", target["session"]], env=os.environ.copy())
                if result.returncode:
                    raise ValueError("Could not switch the current tmux client to the project session.")
                return
        command = self.prefix + ["attach", "-t", target["session"]]
        env = os.environ.copy()
        env.pop("TMUX", None)
        env.pop("TMUX_PANE", None)
        os.execve(self.tmux, command, env)


def render(snapshot: dict, options: argparse.Namespace) -> None:
    print(f"Omnigent projects on {clean(socket.gethostname())} — native interactive tmux", flush=True)
    for item in sorted(snapshot["sessions"].values(), key=lambda s: (group_for(s)[0], s["title"])):
        group, name = group_for(item)
        name = name or "Project " + group.rsplit("/", 1)[-1][:8]
        print(clean(f"{name:<24} {item['id'][:8]}  {item['agent']:<18} {item['title']}"), flush=True)
    if not snapshot["sessions"]:
        print("No live sessions found.", flush=True)
    for error in snapshot["errors"]:
        print(clean(error, 180), flush=True)


def watch(workspace: Workspace) -> None:
    root = workspace.groups().get("unfiled")
    if not root:
        return
    root_id = root["session"]
    while workspace.groups().get("unfiled", {}).get("session") == root_id:
        snapshot = discover(workspace.tmux, workspace.omnigent, workspace.options.server)
        if workspace.groups().get("unfiled", {}).get("session") != root_id:
            break
        try:
            workspace.reconcile(snapshot)
        except (ValueError, OSError, subprocess.TimeoutExpired) as exc:
            snapshot["errors"].append(clean(exc, 180))
        # This is a log in an ordinary tmux window, not an input-filtering TUI.
        print(f"\n{time.strftime('%H:%M:%S')} — refreshed", flush=True)
        render(snapshot, workspace.options)
        time.sleep(workspace.options.interval)


def parse(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__, epilog="Uses your normal tmux config and bindings. All agent windows accept input normally.")
    action = parser.add_mutually_exclusive_group()
    action.add_argument("--detach", action="store_true", help="Start or reuse project sessions without attaching.")
    action.add_argument("--list", action="store_true", help="List sessions and their actual projects without starting tmux.")
    action.add_argument("--stop", action="store_true", help="Close this family's tmux sessions; Omnigent agents keep running.")
    parser.add_argument("--server", help="Show only sessions on this server URL.")
    parser.add_argument("--interval", type=float, default=10, help="Refresh interval in seconds (default: 10).")
    parser.add_argument("--interactive", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--workspace", default="omnigent", help="Prefix for project sessions (default: omnigent).")
    parser.add_argument("--socket-name", help="Use a named tmux server instead of your regular server.")
    parser.add_argument("--tmux-config", help="Explicit tmux config path for a new server (default: your normal config).")
    parser.add_argument("--project", help="Open a specific project by name, id, or tmux session name.")
    options = parser.parse_args(argv)
    if options.interval <= 0 or not options.interval < float("inf"):
        parser.error("--interval must be a finite positive number")
    for value in (options.workspace, options.socket_name):
        if value is None:
            continue
        if not re.fullmatch(r"[A-Za-z0-9_-]+", value):
            parser.error("Workspace and socket names may contain letters, digits, underscores, and hyphens.")
    if options.server:
        options.server = options.server.rstrip("/")
        if urlsplit(options.server).scheme not in ("http", "https"):
            parser.error("--server must be an HTTP(S) URL")
    return options


def main(argv: list[str]) -> int:
    watching = bool(argv and argv[0] == "_watch")
    options = parse(argv[1:] if watching else argv)
    workspace = Workspace(options)
    if watching:
        watch(workspace)
    elif options.list:
        render(discover(workspace.tmux, workspace.omnigent, options.server), options)
    elif options.stop:
        workspace.stop()
        print("Project tmux sessions closed. Omnigent agents keep running.")
    else:
        if not options.detach and not sys.stdin.isatty():
            raise ValueError("Run in an interactive terminal, or use --detach.")
        workspace.ensure()
        if options.detach:
            print("Project tmux sessions are refreshing automatically. Open them with omnigent-tmux.")
        else:
            # Let the controller populate project sessions before choosing.
            deadline = time.monotonic() + 8
            while len(workspace.groups()) <= 1 and time.monotonic() < deadline:
                time.sleep(0.1)
            workspace.attach()
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main(sys.argv[1:]))
    except (ValueError, OSError, subprocess.TimeoutExpired) as error:
        print("omnigent-tmux: " + clean(error, 200), file=sys.stderr)
        raise SystemExit(1)
    except KeyboardInterrupt:
        raise SystemExit(130)

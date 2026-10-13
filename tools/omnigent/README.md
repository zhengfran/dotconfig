# Omnigent project workspaces

`omnigent-tmux` groups this host's live Omnigent conversations into ordinary
interactive tmux sessions. Each Omnigent project gets its own tmux session;
conversations without a project share `omnigent-unfiled`.

```sh
omnigent-tmux                     # Start or reopen the project sessions
omnigent-tmux --project 'Career Ops' # Open a particular project
omnigent-tmux --list              # Inspect project membership without starting tmux
omnigent-tmux --detach            # Start discovery in the background
omnigent-tmux --stop              # Close this family's tmux sessions; agents keep running
```

The wrapper uses your regular tmux server and `~/.tmux.conf`, including your
theme, plugins, prefix, copy mode, and session picker. With this dotfiles
configuration, use:

- **Ctrl-a s** opens your tmux-fzf project/session picker.
- **Ctrl-a w** chooses a conversation window.
- **Ctrl-a -** or **Ctrl-a v** splits a window and opens a shell.
- **Ctrl-a [** enters your vi copy mode.
- **Ctrl-a d** detaches.

Running `omnigent-tmux` from inside this server switches the existing client
to the selected project. It does not open a second tmux UI inside your pane.
When starting a server, it waits for your config and plugins to initialize.

Agent panes are directly interactive. Native Codex and Claude terminals use
ordinary attachments to their existing private tmux servers, with keyboard
input and terminal resizing enabled. There is no filtering PTY, read-only
client flag, or disabled resizing. Agents whose backend has no native terminal,
such as Polly, use Omnigent's regular interactive `attach` REPL.

Omnigent gives each native terminal its own tmux server. Those windows cannot
be moved across tmux servers, so project windows run direct native attach
clients. The outer project sessions are ordinary tmux: you can split panes,
resize, copy, rename windows, and open additional shells. Project reassignment
moves the existing outer window rather than restarting the agent or its panes.

Grouping uses the actual `project_id` in Omnigent, not the working directory
or Git repository. Names come from authenticated read-only project metadata
requests using the installed Omnigent CLI's interpreter and credentials.
Projects on different servers stay separate even if their names match. The
short hash in a generated tmux session name keeps those identities distinct.
An unavailable name lookup retains the project's ID-based group; unknown
membership retains the current group until it can be resolved.

Live standalone Omnigent terminals are included too. Their conversation link
identifies the server and session, which provides their project membership.
Missing native terminals are reported without launching or resuming an agent.

The `overview` window in `omnigent-unfiled` runs one background discovery loop
and prints a refresh log. The normal entrypoint opens a project session rather
than that log. It checks for new, ended, renamed, and reassigned conversations
every ten seconds after each inventory request. Detaching leaves discovery
running. Reopening recovers a closed controller and preserves user-created
panes and windows. Inventory failures preserve existing groups and clients.

Only tagged agent client panes are replaced or removed by refresh. If an agent
ends in a window where you added a shell pane, your shell survives. Manual
window and tmux session names are respected. `--stop` explicitly closes all
managed project sessions in that family, including shells you opened there;
it does not stop the underlying Omnigent agents.

```sh
omnigent-tmux --server https://omnigent.example.com --workspace work
omnigent-tmux --interval 5 --workspace fast

tmux list-sessions
tmux attach-session -t SESSION_NAME
```

`--workspace` selects a session-name prefix and an independent managed family.
`--socket-name` optionally selects a named tmux server instead of the regular
server. `--tmux-config PATH` optionally supplies a config when starting a new
server; tests use `/dev/null` explicitly. Existing servers keep their current
configuration. The wrapper preserves global settings and user sessions;
client retry settings are applied only to managed panes. The old
`--interactive` flag is accepted for compatibility; interaction is always
enabled. The previous `omnigent-host` read-only workspace is upgraded in place,
preserving existing windows and additional shell panes.

Requirements: Python 3, tmux, and Omnigent on macOS or Linux. The setup script
links `omnigent-tmux.py` to `~/.local/bin/omnigent-tmux`. Run it on the host that
owns the terminals, including over SSH when working remotely.

Integration checks use disposable tmux servers and a fake Omnigent CLI:

```sh
python3 -m unittest discover -s tools/omnigent -p 'test_*.py' -v
```

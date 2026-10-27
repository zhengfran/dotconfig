# billion-context

Shared context-compression policy for Pi, Claude Code, Codex, and DeepSeek Harness.
`billion-context.json` is linked to `~/.config/billion-context/billion-context.json`
by `scripts/setup-config.sh`. Provider credentials, session archives, logs, and
machine-specific MCP registrations stay outside this repository.

## Install

```sh
npm install -g billion-context --prefix="$HOME/.local"
# Ensure ~/.local/bin is on PATH, and link the policy before starting clients.
mkdir -p ~/.config/billion-context
# On an existing machine, back up the destination before replacing it.
ln -s "$HOME/dotconfig/tools/ai/billion-context/billion-context.json" \
  "$HOME/.config/billion-context/billion-context.json"
bili plugin install pi
pi update npm:billion-context
bili plugin install claude
bili plugin install codex
bili plugin install dsh
bili doctor
```

The dsh installer adds the bundle to ordinary profile directories. Run it again
after creating a profile. Symlinked profiles (such as Multica's externally owned
profile) are not discovered; install explicitly with
`dsh plugin --profile multica add billion-context` if needed.
`scripts/setup-config.sh` also registers the integrations when `bili` and the
respective clients are on PATH. Native installers regenerate machine-specific
bootstrap/MCP paths and back up existing client configs to `.bili-bak`; review
local configuration changes before running them.

## Usage

| Client | Start | Integration |
| --- | --- | --- |
| Pi | `pi` | Native package; starts/attaches its own proxy |
| Claude Code CLI | `claude` | SessionStart bootstrap + bili MCP tools |
| Codex | **`bili codex`** | Launcher routes model traffic and exposes bili tools |
| dsh | `dsh --profile <name>` | Native bundle in each installed profile |

Restart existing clients and use a new session after installation. The shared
policy disables bili's Pi delegate tools so the existing `pi-subagents` package
remains the sole subagent implementation.

**Plain `codex` is not enough:** its installed MCP entry exposes context tools,
but does not route its model traffic through the proxy. Use `bili codex`, including
for non-interactive commands (`bili codex exec ...`). The launcher preserves the
existing authentication mode and manages a session-scoped proxy.

An exported `ANTHROPIC_API_KEY` takes precedence over Claude's subscription login.
If that key is invalid and you intend to use the subscription, launch with
`env -u ANTHROPIC_API_KEY claude`; this does not change your shell configuration.

Claude Desktop's Code tab is not covered by this integration: upstream documents
that it overrides the managed API URL while auto-compaction remains disabled.
Use the Claude Code terminal CLI instead.

## Verify / maintain

```sh
bili plugin list
bili doctor
pi update npm:billion-context
bili plugin update dsh
claude mcp get bili
codex mcp get bili
```

`bili doctor` checks installation/version state, not successful model requests or
compression quality. After a real conversation, inspect `/acp-cache` where the
client supports it, or the session dashboard at the proxy URL shown in its logs.
Compression events only appear once enough history has accumulated.

Docs: https://github.com/ranxianglei/billion-context

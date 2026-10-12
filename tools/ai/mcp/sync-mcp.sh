#!/usr/bin/env bash
# sync-mcp.sh [pi|claude|kiro ...] — render servers.json into each harness's
# user-level MCP config. Only the servers named in servers.json are touched;
# every other entry in the target file is kept. Defaults to every harness
# that is installed. Re-run after editing servers.json.
set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
secrets=${MCP_SECRETS_FILE:-${XDG_CONFIG_HOME:-$HOME/.config}/mcp/secrets.env}

# Resolve "mcp-run" to this checkout's launcher (GUI-launched harnesses may not
# have ~/.local/bin on PATH) and drop secrets-backed servers on machines
# without a secrets file.
servers=$(jq --arg run "$here/bin/mcp-run" --argjson have_secrets "$([[ -r $secrets ]] && echo true || echo false)" '
    .mcpServers
    | with_entries(select(.value.secrets != true or $have_secrets))
    | map_values(if .command == "mcp-run" then .command = $run else . end)
' "$here/servers.json")

if [[ $servers == "{}" ]]; then
    echo "[SKIP] mcp: no servers to sync (secrets-backed servers need $secrets)"
    exit 0
fi

# merge_into FILE ENTRIES — overwrite in place so symlinks and modes survive.
merge_into() {
    local file=$1 entries=$2 tmp
    mkdir -p "$(dirname "$file")"
    [[ -s $file ]] || echo '{}' >"$file"
    tmp=$(mktemp)
    jq --argjson s "$entries" '.mcpServers = ((.mcpServers // {}) + $s)' "$file" >"$tmp"
    cat "$tmp" >"$file"
    rm -f "$tmp"
}

sync_pi() {
    merge_into "${PI_CODING_AGENT_DIR:-$HOME/.pi/agent}/mcp.json" \
        "$(jq 'map_values({command, args, env, description} | with_entries(select(.value != null)))' <<<"$servers")"
}

sync_kiro() {
    merge_into "$HOME/.kiro/settings/mcp.json" \
        "$(jq 'map_values({command, args, env: (.env // {}), autoApprove: (.autoApprove // [])})' <<<"$servers")"
}

sync_claude() {
    local name
    for name in $(jq -r 'keys[]' <<<"$servers"); do
        claude mcp remove -s user "$name" >/dev/null 2>&1 || true
        claude mcp add-json -s user "$name" \
            "$(jq -c --arg n "$name" '.[$n] | {type: "stdio", command, args, env: (.env // {})}' <<<"$servers")" >/dev/null
    done
}

targets=("$@")
if [[ ${#targets[@]} -eq 0 ]]; then
    command -v pi >/dev/null 2>&1 && targets+=(pi)
    command -v claude >/dev/null 2>&1 && targets+=(claude)
    { command -v kiro-cli >/dev/null 2>&1 || [[ -d $HOME/.kiro ]]; } && targets+=(kiro)
fi

for t in "${targets[@]}"; do
    case $t in
        pi | claude | kiro) "sync_$t" && echo "[OK] mcp -> $t: $(jq -r 'keys | join(", ")' <<<"$servers")" ;;
        *) echo "[WARN] mcp: unknown harness '$t'" >&2 ;;
    esac
done

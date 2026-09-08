#!/usr/bin/env bash
# Refresh every coding-agent quota herdr tracks, into herdr's own cache.
#
# Bound to a herdr key (see config.toml) and also called in the background by
# status-quota.sh when the cache goes stale. Nothing here touches pi.
set -uo pipefail

root="$(cd "$(dirname "$0")" && pwd)"
cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/herdr-status"
lock="$cache_dir/quota-refresh.lock"
mkdir -p "$cache_dir" 2>/dev/null || true

command -v node >/dev/null 2>&1 || { echo "node is not installed" >&2; exit 1; }

# --wait blocks until any in-flight refresh finishes instead of skipping; used
# for manual CLI runs that need to report a result.
args=()
wait_for_lock=0
for a in "$@"; do
    case "$a" in
        --wait) wait_for_lock=1 ;;
        *)      args+=("$a") ;;
    esac
done

# One refresh at a time. --wait when a human pressed the key (so the popup
# reports the result of the run already in flight rather than exiting mute),
# non-blocking when called from the status bar.
exec 9>"$lock" || exit 1
if [ "$wait_for_lock" = 1 ]; then
    flock 9
else
    flock -n 9 || exit 0
fi

# This network sits behind a TLS-inspecting proxy, so Node has to trust the
# system CA store; its bundled one fails with SELF_SIGNED_CERT_IN_CHAIN on
# every host except the few the proxy passes through. --use-system-ca needs
# Node >= 22.15, so older runtimes fall back to the bundle path.
flags=(--experimental-strip-types)
if node --use-system-ca -e '' >/dev/null 2>&1; then
    flags+=(--use-system-ca)
elif [ -r /etc/ssl/certs/ca-certificates.crt ]; then
    export NODE_EXTRA_CA_CERTS=/etc/ssl/certs/ca-certificates.crt
fi

exec node "${flags[@]}" "$root/quota/main.ts" ${args[@]+"${args[@]}"}

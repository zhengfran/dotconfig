#!/usr/bin/env bash
# Refresh every coding-agent quota herdr tracks, into herdr's own cache.
#
# Bound to a herdr key (see config.toml) and also called in the background by
# status-quota.sh when the cache goes stale. Nothing here touches pi.
set -uo pipefail

root="$(cd "$(dirname "$0")" && pwd)"
# flock is util-linux and absent on macOS; the lock helpers come from here.
. "$root/portable.sh"

cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/herdr-status"
lock="$cache_dir/quota-refresh.lock.d"
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
if [ "$wait_for_lock" = 1 ]; then
    herdr_lock_acquire "$lock" wait 60 || exit 1
else
    herdr_lock_acquire "$lock" nowait || exit 0
fi
# The mkdir lock, unlike flock's file descriptor, does not ride through an
# exec, so the node run below stays a child and the lock is dropped here.
trap 'herdr_lock_release "$lock"' EXIT INT TERM

# This network sits behind a TLS-inspecting proxy, so Node has to trust the
# system CA store; its bundled one fails with SELF_SIGNED_CERT_IN_CHAIN on
# every host except the few the proxy passes through. --use-system-ca needs
# Node >= 22.15, so older runtimes fall back to a CA bundle on disk -- whose
# path is distro- and platform-specific.
flags=(--experimental-strip-types)
if node --use-system-ca -e '' >/dev/null 2>&1; then
    flags+=(--use-system-ca)
else
    for bundle in \
        /etc/ssl/certs/ca-certificates.crt \
        /etc/pki/tls/certs/ca-bundle.crt \
        /opt/homebrew/etc/ca-certificates/cert.pem \
        /usr/local/etc/ca-certificates/cert.pem
    do
        if [ -r "$bundle" ]; then
            export NODE_EXTRA_CA_CERTS="$bundle"
            break
        fi
    done
fi

node "${flags[@]}" "$root/quota/main.ts" ${args[@]+"${args[@]}"}

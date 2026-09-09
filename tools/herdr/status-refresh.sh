#!/usr/bin/env bash
# What prefix+q runs: refresh every agent quota, then repaint the status area
# immediately instead of waiting out the entry's interval_seconds tick.
#
# Bound in config.toml ([[keys.command]] prefix+q). Herdr runs it detached with
# no window, so nothing here may write to a terminal: the repainted bar and the
# toast are the only feedback a keypress gets.
set -uo pipefail

root="$(cd "$(dirname "$0")" && pwd)"

# --wait rather than the status bar's non-blocking mode. status-quota.sh kicks
# off its own refresh whenever the cache goes stale, and quota-refresh.sh takes
# a lock; without --wait a keypress landing on top of that run would hit
# `flock -n`, exit 0, and look identical to a refresh that never happened.
if "$root/quota-refresh.sh" --wait --quiet; then
    title="Quota refreshed"
    sound=()
else
    title="Quota refresh failed"
    sound=(--sound request)
fi

# Tab bar command entries re-run on config reload — measured at ~18ms, against
# the entry's own 30s tick. This is the only way to repaint on demand: herdr
# 0.9.0 exposes no per-entry refresh. It applies reloadable settings only and
# leaves panes running, same as prefix+r.
herdr server reload-config >/dev/null 2>&1

# The bar has already repainted by now; the toast is what confirms the keypress
# actually did something when the numbers happen to be unchanged.
herdr notification show "$title" --body "$("$root/status-quota.sh")" \
    ${sound[@]+"${sound[@]}"} >/dev/null 2>&1 || true

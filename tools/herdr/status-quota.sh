#!/usr/bin/env bash
# Coding-agent quota segment for herdr's ui.tab_bar_right.
#
# Single source of truth: ~/.cache/herdr-status/quota.json, written by
# quota-refresh.sh (herdr's own refresher). Nothing here reads pi's cache and
# pi need not be installed.
#
#   CL 28%/85%   five-hour / weekly remaining
#   KI 93%       monthly quota remaining
#   CP 99%       AI credits remaining against the configured budget
#   !            at or below the low-quota threshold
#   ?            cache older than its staleness threshold (refresh failing)
set -uo pipefail

root="$(cd "$(dirname "$0")" && pwd)"
# stat, ping and setsid all differ or go missing between Linux and macOS.
. "$root/portable.sh"

cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/herdr-status"
quota_cache="$cache_dir/quota.json"
mkdir -p "$cache_dir" 2>/dev/null || true

command -v jq >/dev/null 2>&1 || exit 0

LOW_PERCENT=${HERDR_STATUS_LOW_PERCENT:-10}
# How old the cache may get before a background refresh is kicked off.
REFRESH_TTL=${HERDR_STATUS_REFRESH_TTL:-600}
# Five-hour windows move fast, so they age out sooner than monthly quotas.
PAIR_STALE_AFTER=${HERDR_STATUS_PAIR_STALE_AFTER:-1800}
MONTHLY_STALE_AFTER=${HERDR_STATUS_MONTHLY_STALE_AFTER:-43200}
CORP_IP=${HERDR_STATUS_CORP_IP:-10.221.249.249}
CORP_TTL=${HERDR_STATUS_CORP_TTL:-300}
# GitHub does not report the credit entitlement for this seat, so it is config.
COPILOT_CREDITS=${HERDR_STATUS_COPILOT_CREDITS:-20000}

now=$(date +%s)

# The providers this bar knows how to render, in display order. Also the only
# values ever spliced into a seg_* variable name below.
PROVIDERS="claude codex copilot kiro"

# macOS ships bash 3.2, which has no associative arrays, so each provider's
# rendered segment lives in a seg_<provider> variable reached by indirect
# expansion.
seg_set() { printf -v "seg_$1" '%s' "$2"; }
seg_get() { local name="seg_$1"; printf '%s' "${!name-}"; }

label_for() {
    case "$1" in
        claude)  echo "CL" ;;
        codex)   echo "CX" ;;
        copilot) echo "CP" ;;
        kiro)    echo "KI" ;;
        *)       echo "${1:0:2}" | tr '[:lower:]' '[:upper:]' ;;
    esac
}

# "-" means the provider did not report the window; "inf" means it reported the
# quota as unlimited. Neither takes a percent sign.
as_percent() {
    case "$1" in
        -)   echo "-" ;;
        inf) echo "∞" ;;
        *)   echo "$1%" ;;
    esac
}

# Company-granted providers are only meaningful on the corporate network. The
# probe is cached: an off-network miss costs a full ping timeout, which would
# otherwise blow the entry's timeout_seconds budget on every tick.
is_corporate() {
    local f="$cache_dir/corp.check" result
    if [ -r "$f" ] && [ "$(herdr_file_age "$f")" -lt "$CORP_TTL" ]; then
        [ "$(cat "$f" 2>/dev/null)" = "1" ]
        return
    fi
    result=0
    herdr_host_reachable "$CORP_IP" && result=1
    printf '%s' "$result" > "$f" 2>/dev/null || true
    [ "$result" = "1" ]
}

# Refresh detached: the status bar must never wait on the network.
maybe_refresh() {
    if [ ! -r "$quota_cache" ] || [ "$(herdr_file_age "$quota_cache")" -ge "$REFRESH_TTL" ]; then
        herdr_spawn_detached "$root/quota-refresh.sh" --quiet
    fi
}

# The cache stores usedPercent; the bar shows what is left.
read_provider() {
    local provider=$1 kind=$2 want=$3 row observed a b flag seg
    [ -r "$quota_cache" ] || return 0

    row=$(jq -r --arg p "$provider" --arg w "$want" --arg k "$kind" '
        def show:
            if . == null then "-"
            elif (.unlimited // false) then "inf"
            else ((100 - (.usedPercent // 0)) | floor | tostring) end;
        # Strict for the pair: five_hour and seven_day must match by id, or one
        # window would be reported as both halves.
        def rem($id): (.windows // []) | map(select(.id == $id)) | .[0] | show;
        # The monthly providers carry a single window, so an id the provider has
        # renamed can safely fall back to it -- but only when it is unambiguous.
        def mono($id):
            (.windows // []) as $ws
            | (($ws | map(select(.id == $id)) | .[0])
               // (if ($ws | length) == 1 then $ws[0] else null end))
            | show;
        (.snapshots[$p] // empty) as $s
        | if $s == null then empty
          else [ (($s.observedAt // "")
                  | if . == "" then 0
                    else (sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601? // 0) end),
                 ($s | rem("five_hour")),
                 ($s | if $k == "pair" then rem($w) else mono($w) end) ] | @tsv
          end
    ' "$quota_cache" 2>/dev/null) || return 0
    [ -n "$row" ] || return 0

    IFS=$'\t' read -r observed a b <<< "$row"
    : "${observed:=0}"
    flag=""

    if [ "$kind" = "pair" ]; then
        [ "$a" != "-" ] && [ "$a" != "inf" ] && [ "$a" -le "$LOW_PERCENT" ] 2>/dev/null && flag="!"
        seg="$(label_for "$provider") $(as_percent "$a")/$(as_percent "$b")"
        [ "$observed" -gt 0 ] 2>/dev/null && [ $((now - observed)) -gt "$PAIR_STALE_AFTER" ] && flag="${flag}?"
    else
        [ "$b" != "-" ] && [ "$b" != "inf" ] && [ "$b" -le "$LOW_PERCENT" ] 2>/dev/null && flag="!"
        seg="$(label_for "$provider") $(as_percent "$b")"
        [ "$observed" -gt 0 ] 2>/dev/null && [ $((now - observed)) -gt "$MONTHLY_STALE_AFTER" ] && flag="${flag}?"
    fi
    seg_set "$provider" "${seg}${flag}"
}

# Copilot reports AI credits used with no entitlement, so the percentage is
# derived against the configured budget rather than read from the provider.
read_copilot() {
    local row used observed pct flag seg
    [ -r "$quota_cache" ] || return 0

    row=$(jq -r '
        (.snapshots.copilot // empty) as $s
        | if $s == null then empty
          else [ (($s.observedAt // "")
                  | if . == "" then 0
                    else (sub("\\.[0-9]+Z$"; "Z") | fromdateiso8601? // 0) end),
                 ((($s.windows // []) | map(select(.id == "ai_credits")) | .[0].used) // "") ] | @tsv
          end
    ' "$quota_cache" 2>/dev/null) || return 0
    [ -n "$row" ] || return 0

    IFS=$'\t' read -r observed used <<< "$row"
    [ -n "$used" ] || return 0
    used=${used%.*}
    flag=""

    if [ "$COPILOT_CREDITS" -gt 0 ] 2>/dev/null; then
        pct=$(( (COPILOT_CREDITS - used) * 100 / COPILOT_CREDITS ))
        [ "$pct" -lt 0 ] && pct=0
        [ "$pct" -le "$LOW_PERCENT" ] && flag="!"
        seg="$(label_for copilot) ${pct}%"
    else
        # No budget configured, so a percentage is not derivable; show the count.
        seg="$(label_for copilot) ${used}cr"
    fi
    [ "$observed" -gt 0 ] 2>/dev/null && [ $((now - observed)) -gt "$MONTHLY_STALE_AFTER" ] && flag="${flag}?"
    seg_set copilot "${seg}${flag}"
}

maybe_refresh
read_provider claude pair seven_day
read_provider codex  pair seven_day
if is_corporate; then
    read_copilot
    read_provider kiro monthly CREDIT
fi

out=""
for p in $PROVIDERS; do
    seg=$(seg_get "$p")
    [ -n "$seg" ] && out="${out:+$out }$seg"
done

[ -n "$out" ] || exit 0
printf '%s\n' "$out"

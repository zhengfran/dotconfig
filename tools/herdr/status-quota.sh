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
declare -A seg_of

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
    local f="$cache_dir/corp.check" age result
    if [ -r "$f" ]; then
        age=$(( now - $(stat -c %Y "$f" 2>/dev/null || echo 0) ))
        if [ "$age" -ge 0 ] && [ "$age" -lt "$CORP_TTL" ]; then
            [ "$(cat "$f" 2>/dev/null)" = "1" ]
            return
        fi
    fi
    result=0
    if command -v ping >/dev/null 2>&1 && ping -c 1 -W 1 -n "$CORP_IP" >/dev/null 2>&1; then
        result=1
    fi
    printf '%s' "$result" > "$f" 2>/dev/null || true
    [ "$result" = "1" ]
}

# Refresh detached: the status bar must never wait on the network.
maybe_refresh() {
    local age
    age=$(( now - $(stat -c %Y "$quota_cache" 2>/dev/null || echo 0) ))
    if [ ! -r "$quota_cache" ] || [ "$age" -ge "$REFRESH_TTL" ]; then
        ( setsid "$(dirname "$0")/quota-refresh.sh" --quiet >/dev/null 2>&1 & ) 2>/dev/null || true
    fi
}

# The cache stores usedPercent; the bar shows what is left.
read_provider() {
    local provider=$1 kind=$2 want=$3 row observed a b flag
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
        seg_of[$provider]="$(label_for "$provider") $(as_percent "$a")/$(as_percent "$b")"
        [ "$observed" -gt 0 ] 2>/dev/null && [ $((now - observed)) -gt "$PAIR_STALE_AFTER" ] && flag="${flag}?"
    else
        [ "$b" != "-" ] && [ "$b" != "inf" ] && [ "$b" -le "$LOW_PERCENT" ] 2>/dev/null && flag="!"
        seg_of[$provider]="$(label_for "$provider") $(as_percent "$b")"
        [ "$observed" -gt 0 ] 2>/dev/null && [ $((now - observed)) -gt "$MONTHLY_STALE_AFTER" ] && flag="${flag}?"
    fi
    seg_of[$provider]="${seg_of[$provider]}${flag}"
}

# Copilot reports AI credits used with no entitlement, so the percentage is
# derived against the configured budget rather than read from the provider.
read_copilot() {
    local row used observed pct flag
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
        seg_of[copilot]="$(label_for copilot) ${pct}%"
    else
        # No budget configured, so a percentage is not derivable; show the count.
        seg_of[copilot]="$(label_for copilot) ${used}cr"
    fi
    [ "$observed" -gt 0 ] 2>/dev/null && [ $((now - observed)) -gt "$MONTHLY_STALE_AFTER" ] && flag="${flag}?"
    seg_of[copilot]="${seg_of[copilot]}${flag}"
}

maybe_refresh
read_provider claude pair seven_day
read_provider codex  pair seven_day
if is_corporate; then
    read_copilot
    read_provider kiro monthly CREDIT
fi

out=()
for p in claude codex copilot kiro; do
    [ -n "${seg_of[$p]:-}" ] && out+=("${seg_of[$p]}")
done

[ ${#out[@]} -gt 0 ] || exit 0
printf '%s\n' "$(IFS=' '; echo "${out[*]}")"

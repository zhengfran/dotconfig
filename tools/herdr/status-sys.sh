#!/usr/bin/env bash
# CPU + memory segment for herdr's ui.tab_bar_right.
#
# herdr re-executes this on every interval tick, so CPU has to be sampled as a
# delta across invocations: /proc/stat is cumulative since boot and a single
# read only yields the average since boot, which never moves. The previous
# sample is kept in the cache dir and the first run reports "--".
set -uo pipefail

state_dir="${XDG_CACHE_HOME:-$HOME/.cache}/herdr-status"
prev_file="$state_dir/cpu.prev"
mkdir -p "$state_dir" 2>/dev/null || true

cpu="--"
if read -r _ user nice system idle iowait irq softirq steal _ < /proc/stat 2>/dev/null; then
    total=$((user + nice + system + idle + iowait + irq + softirq + steal))
    busy=$((total - idle - iowait))

    if [ -r "$prev_file" ]; then
        read -r prev_busy prev_total < "$prev_file" 2>/dev/null || true
        : "${prev_busy:=0}" "${prev_total:=0}"
        d_total=$((total - prev_total))
        d_busy=$((busy - prev_busy))
        # A negative delta means the counters were reset (reboot, or a WSL VM
        # restart); fall through to "--" and reseed from this sample.
        if [ "$d_total" -gt 0 ] && [ "$d_busy" -ge 0 ]; then
            cpu=$(( (d_busy * 100 + d_total / 2) / d_total ))
        fi
    fi

    printf '%s %s\n' "$busy" "$total" > "$prev_file" 2>/dev/null || true
fi

mem=$(awk '
    /^MemTotal:/     { total = $2 }
    /^MemAvailable:/ { avail = $2 }
    END {
        if (total > 0) printf "%d", (total - avail) * 100 / total
        else printf "--"
    }
' /proc/meminfo 2>/dev/null)
[ -n "$mem" ] || mem="--"

printf 'CPU %s%% MEM %s%%\n' "$cpu" "$mem"

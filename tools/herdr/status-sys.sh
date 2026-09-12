#!/usr/bin/env bash
# CPU + memory segment for herdr's ui.tab_bar_right.
#
# The two platforms expose these completely differently, so there are two
# implementations rather than one with a shim:
#
#   Linux  /proc/stat is cumulative since boot, so CPU has to be sampled as a
#          delta across herdr's interval ticks. The previous sample is kept in
#          the cache dir and the first run reports "--".
#   macOS  has no /proc. top does its own delta internally, so no state is
#          carried between ticks; memory comes from vm_stat page counts.
set -uo pipefail

root="$(cd "$(dirname "$0")" && pwd)"
. "$root/portable.sh"

state_dir="${XDG_CACHE_HOME:-$HOME/.cache}/herdr-status"
prev_file="$state_dir/cpu.prev"
mkdir -p "$state_dir" 2>/dev/null || true

cpu_linux() {
    local user nice system idle iowait irq softirq steal total busy
    local prev_busy prev_total d_total d_busy
    read -r _ user nice system idle iowait irq softirq steal _ < /proc/stat 2>/dev/null || return 0
    total=$((user + nice + system + idle + iowait + irq + softirq + steal))
    busy=$((total - idle - iowait))

    if [ -r "$prev_file" ]; then
        read -r prev_busy prev_total < "$prev_file" 2>/dev/null || true
        : "${prev_busy:=0}" "${prev_total:=0}"
        d_total=$((total - prev_total))
        d_busy=$((busy - prev_busy))
        # A negative delta means the counters were reset (reboot, or a WSL VM
        # restart); print nothing and reseed from this sample.
        if [ "$d_total" -gt 0 ] && [ "$d_busy" -ge 0 ]; then
            printf '%s' $(( (d_busy * 100 + d_total / 2) / d_total ))
        fi
    fi

    printf '%s %s\n' "$busy" "$total" > "$prev_file" 2>/dev/null || true
}

mem_linux() {
    awk '
        /^MemTotal:/     { total = $2 }
        /^MemAvailable:/ { avail = $2 }
        END { if (total > 0) printf "%d", (total - avail) * 100 / total }
    ' /proc/meminfo 2>/dev/null
}

cpu_darwin() {
    # top's first sample is the average since boot, which never moves, so two
    # are needed. -s 0 makes the second one immediate (~0.5s total) instead of
    # top's default 1s delay, which would eat the entry's timeout budget.
    top -l 2 -s 0 -n 0 2>/dev/null | awk '
        /^CPU usage/ { idle = $(NF - 1); sub(/%/, "", idle); seen = 1 }
        END { if (seen) printf "%d", 100 - idle + 0.5 }
    '
}

mem_darwin() {
    # macOS has no MemAvailable, so it is approximated the way the platform
    # itself does: pages that can be handed to a new allocation without
    # swapping are free + inactive + speculative + purgeable.
    vm_stat 2>/dev/null | awk -v total="$(sysctl -n hw.memsize 2>/dev/null || echo 0)" '
        function pages(v) { gsub(/[^0-9]/, "", v); return v + 0 }
        /page size of/ {
            for (i = 1; i <= NF; i++) if ($i ~ /^[0-9]+$/) { page = $i + 0; break }
        }
        /^Pages free:/        { avail += pages($3) }
        /^Pages inactive:/    { avail += pages($3) }
        /^Pages speculative:/ { avail += pages($3) }
        /^Pages purgeable:/   { avail += pages($3) }
        END {
            if (total <= 0 || page <= 0) exit
            used = (total - avail * page) * 100 / total
            if (used < 0) used = 0
            if (used > 100) used = 100
            printf "%d", used
        }
    '
}

if [ "$HERDR_OS" = Darwin ]; then
    cpu=$(cpu_darwin)
    mem=$(mem_darwin)
else
    cpu=$(cpu_linux)
    mem=$(mem_linux)
fi

# Either probe reports "--" rather than a stale or invented number: the first
# Linux tick has no previous sample to diff against, and a platform that
# answers neither /proc nor top should say so instead of showing 0%.
[ -n "$cpu" ] || cpu="--"
[ -n "$mem" ] || mem="--"

printf 'CPU %s%% MEM %s%%\n' "$cpu" "$mem"

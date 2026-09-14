#!/usr/bin/env bash
# Cross-platform primitives shared by the herdr status scripts.
#
# These configs run on WSL2/Linux and on macOS. Everything the status scripts
# reached for that differs between the two lives here, so the scripts
# themselves stay readable:
#
#   stat     GNU spells mtime -c %Y, BSD spells it -f %m, and neither accepts
#            the other's flag.
#   ping     -W is seconds on iputils and milliseconds on the BSD ping macOS
#            ships, so one value cannot mean the same thing on both.
#   setsid   util-linux; absent on macOS.
#   flock    util-linux; absent on macOS.
#
# Sourced, not executed. Bash 3.2 clean: macOS still ships 3.2, so nothing here
# may use associative arrays, `declare -n`, or `${var^^}`.

HERDR_OS=$(uname -s 2>/dev/null || echo unknown)

# Herdr command hooks can be launched by a GUI-started server on macOS, where
# PATH often lacks Homebrew even though interactive shells have it. Add the
# common package-manager bins here so status scripts find node/herdr/jq without
# depending on shell startup files.
case "$HERDR_OS" in
    Darwin)
        for herdr_path_dir in \
            "$HOME/.local/bin" \
            /opt/homebrew/bin /opt/homebrew/sbin \
            /usr/local/bin /usr/local/sbin
        do
            [ -d "$herdr_path_dir" ] || continue
            case ":$PATH:" in
                *":$herdr_path_dir:"*) ;;
                *) PATH="$herdr_path_dir:$PATH" ;;
            esac
        done
        export PATH
        unset herdr_path_dir
        ;;
esac

# Epoch seconds of a file's mtime, or 0 when it cannot be read. Always prints
# an integer: callers feed this straight into $(( )), where an empty string is
# a syntax error rather than a zero.
herdr_file_mtime() {
    local path=$1 mtime
    if [ "$HERDR_OS" = Darwin ]; then
        mtime=$(stat -f %m "$path" 2>/dev/null)
    else
        mtime=$(stat -c %Y "$path" 2>/dev/null)
    fi
    case "$mtime" in
        '' | *[!0-9]*) echo 0 ;;
        *) echo "$mtime" ;;
    esac
}

# Seconds since a file was last written, or a very large number when it is
# missing -- which reads naturally at every call site as "long overdue".
herdr_file_age() {
    local mtime
    mtime=$(herdr_file_mtime "$1")
    [ "$mtime" -gt 0 ] || { echo 999999999; return; }
    echo $(( $(date +%s) - mtime ))
}

# Run a command detached: the status bar must never wait on it, and it has to
# outlive the tick that spawned it. nohup in a subshell is the portable
# equivalent of setsid for that purpose -- the extra fork keeps the job off
# this shell's table, and nohup detaches it from the terminal.
herdr_spawn_detached() {
    if command -v setsid >/dev/null 2>&1; then
        ( setsid "$@" >/dev/null 2>&1 & ) 2>/dev/null || true
    else
        ( nohup "$@" >/dev/null 2>&1 & ) 2>/dev/null || true
    fi
}

# One ICMP probe with a hard 1 second budget on both platforms. The per-reply
# wait (-W) is not enough on its own: BSD ping ignores it when the address has
# no route and still sits there for ~2s, which alone would blow the tab bar
# entry's timeout_seconds. The overall deadline is what actually bounds this --
# -t on BSD, -w on iputils, and each spells the other's flag differently.
herdr_host_reachable() {
    command -v ping >/dev/null 2>&1 || return 1
    if [ "$HERDR_OS" = Darwin ]; then
        ping -c 1 -t 1 -W 500 -n "$1" >/dev/null 2>&1
    else
        ping -c 1 -w 1 -W 1 -n "$1" >/dev/null 2>&1
    fi
}

# Mutual exclusion without flock. mkdir is atomic on every filesystem herdr
# runs on; the holder's pid goes inside so a lock orphaned by SIGKILL can be
# broken instead of wedging the refresher until the next reboot.
#
#   herdr_lock_acquire <dir> [wait|nowait] [timeout_seconds]
#
# Returns 0 holding the lock, 1 otherwise. Unlike flock's file descriptor, this
# lock does NOT survive `exec`, so the caller must release it from a trap.
herdr_lock_acquire() {
    local dir=$1 mode=${2:-nowait} timeout=${3:-60}
    local deadline=$(( $(date +%s) + timeout )) holder reaped=0

    while :; do
        if mkdir "$dir" 2>/dev/null; then
            printf '%s\n' "$$" > "$dir/pid" 2>/dev/null || true
            return 0
        fi

        holder=$(cat "$dir/pid" 2>/dev/null)
        if [ "$reaped" -eq 0 ]; then
            case "$holder" in
                '' | *[!0-9]*)
                    # The owner writes pid immediately after mkdir. Give that
                    # tiny race a moment before treating a missing/corrupt pid
                    # as a stale lock left by an interrupted refresher.
                    sleep 0.1
                    holder=$(cat "$dir/pid" 2>/dev/null)
                    case "$holder" in
                        '' | *[!0-9]*)
                            reaped=1
                            rm -rf "$dir" 2>/dev/null || true
                            continue
                            ;;
                    esac
                    ;;
            esac
        fi
        # A pid we cannot signal is gone, so its lock is garbage. Reap it at
        # most once per call: two racing reapers that each retried forever
        # would take turns deleting the other's freshly taken lock.
        if [ "$reaped" -eq 0 ] && ! kill -0 "$holder" 2>/dev/null; then
            reaped=1
            rm -rf "$dir" 2>/dev/null || true
            continue
        fi

        [ "$mode" = wait ] || return 1
        [ "$(date +%s)" -lt "$deadline" ] || return 1
        sleep 0.2
    done
}

herdr_lock_release() {
    rm -rf "$1" 2>/dev/null || true
}

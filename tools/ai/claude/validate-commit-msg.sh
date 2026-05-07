#!/usr/bin/env bash
# validate-commit-msg.sh — Claude Code PreToolUse hook for Bash.
#
# Reads the hook JSON payload on stdin. If the Bash command is a
# `git commit -m ...` (plain or HEREDOC), validates the first line of
# the message against Conventional Commits format. Exits 2 (block) on
# failure with stderr feedback Claude can act on; exits 0 (allow) on
# success or for any non-`git commit` Bash call.
#
# Allowed types: feat | fix | docs | style | refactor | perf | test
#                | build | ci | chore | revert
#
# Requires `jq` to parse the JSON payload. If `jq` is missing, the hook
# becomes a no-op (better to allow than to block all Bash globally).

set -euo pipefail

# ── Read hook payload ────────────────────────────────────────────────────────
payload=$(cat)

if ! command -v jq >/dev/null 2>&1; then
    exit 0
fi

cmd=$(printf '%s' "$payload" | jq -r '.tool_input.command // empty' 2>/dev/null || true)
[ -z "$cmd" ] && exit 0

# ── Quick exits for non-commit Bash calls ────────────────────────────────────
# Match `git commit` as a whole token (not e.g. `git commits` or `git--commit`).
if ! printf '%s' "$cmd" | grep -qE '(^|[^[:alnum:]_-])git[[:space:]]+commit([[:space:]]|$)'; then
    exit 0
fi

# `--amend --no-edit` reuses the existing (already valid) message; allow.
if printf '%s' "$cmd" | grep -qE -- '--no-edit'; then
    exit 0
fi

# ── Extract the commit message ───────────────────────────────────────────────
msg=""

# Pattern A: HEREDOC — `-m "$(cat <<'EOF'\n...\nEOF\n)"` or unquoted variant.
if printf '%s' "$cmd" | grep -qE "<<['\"]?[A-Za-z_][A-Za-z0-9_]*['\"]?"; then
    tag=$(printf '%s' "$cmd" \
        | grep -oE "<<['\"]?[A-Za-z_][A-Za-z0-9_]*['\"]?" \
        | head -n 1 \
        | sed -E "s/<<['\"]?//; s/['\"]?$//")
    if [ -n "$tag" ]; then
        msg=$(printf '%s' "$cmd" | awk -v tag="$tag" '
            $0 ~ "<<['\''\"]?"tag"['\''\"]?" { capture=1; next }
            capture && $0 == tag { exit }
            capture { print }
        ')
    fi
fi

# Pattern B: plain `-m "subject"` or `-m 'subject'`.
if [ -z "$msg" ]; then
    msg=$(printf '%s' "$cmd" | sed -nE 's/.*-m[[:space:]]+"([^"]+)".*/\1/p' | head -n 1)
fi
if [ -z "$msg" ]; then
    msg=$(printf '%s' "$cmd" | sed -nE "s/.*-m[[:space:]]+'([^']+)'.*/\\1/p" | head -n 1)
fi

# Couldn't extract a message — likely the user opened the editor (no -m).
# Don't block; we can't validate what we can't see.
[ -z "$msg" ] && exit 0

# ── Validate first non-blank line ────────────────────────────────────────────
first=$(printf '%s' "$msg" | grep -v '^[[:space:]]*$' | head -n 1)

re='^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert)(\([^)]+\))?!?:[[:space:]].+'

if printf '%s' "$first" | grep -qE "$re"; then
    exit 0
fi

# ── Reject with actionable feedback ──────────────────────────────────────────
cat >&2 <<MSG
✗ Commit message does not match Conventional Commits format
  (enforced by validate-commit-msg.sh PreToolUse hook).

Got first line:
    $first

Required format:
    type(scope): description

Allowed types:
    feat | fix | docs | style | refactor | perf | test | build | ci | chore | revert

Examples:
    feat(ahk): add hyper key layer
    fix(garmin): correct timezone in sleep parsing
    docs(readme): update setup instructions
    chore(skills): bump conventional-commit version
    feat!: drop legacy yank fallback (BREAKING CHANGE)

Invoke the 'conventional-commit' skill if you need a template, then retry.
MSG

exit 2

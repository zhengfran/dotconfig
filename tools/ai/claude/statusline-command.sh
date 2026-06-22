#!/bin/sh
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir // empty')
[ -z "$cwd" ] && cwd=$(pwd)
cwd="${cwd/#$HOME/\~}"
model=$(echo "$input" | jq -r '.model.display_name // empty')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Build base prompt: user@host:cwd
base=$(printf '\033[1;32m%s@%s\033[0m:\033[1;34m%s\033[0m' "$(whoami)" "$(hostname -s)" "$cwd")

# Append model name if available
if [ -n "$model" ]; then
  base="$base $model"
fi

# Append context usage if available
if [ -n "$used" ]; then
  used_int=$(printf '%.0f' "$used")
  base="$base $(printf '\033[1;31mctx:%s%%\033[0m' "$used_int")"
fi

printf '%s' "$base"

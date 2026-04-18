#!/bin/sh
input=$(cat)
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name // empty')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Build base prompt: [user@host dir]
base=$(printf '[%s@%s %s]' "$(whoami)" "$(hostname -s)" "$(basename "$cwd")")

# Append model name if available
if [ -n "$model" ]; then
  base="$base $model"
fi

# Append context usage if available
if [ -n "$used" ]; then
  used_int=$(printf '%.0f' "$used")
  base="$base ctx:${used_int}%"
fi

printf '%s' "$base"

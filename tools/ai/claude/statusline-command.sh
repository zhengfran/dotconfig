#!/usr/bin/env bash
# Claude Code statusLine command — mirrors the magicmace zsh theme style.
# Receives JSON on stdin with session context.

input=$(cat)

# Extract fields from JSON
cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // empty')
model=$(echo "$input" | jq -r '.model.display_name // empty')
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')

# --- Directory display (fish-style abbreviation: shorten each component to 1 char except last) ---
if [ -n "$cwd" ]; then
  # Replace $HOME with ~
  home_escaped=$(printf '%s\n' "$HOME" | sed 's/[[\.*^$()+?{}|]/\\&/g')
  display_dir=$(echo "$cwd" | sed "s|^$HOME|~|")

  # Fish-style: abbreviate each path component except the last to its first character
  last_part=$(basename "$display_dir")
  dir_part=$(dirname "$display_dir")

  if [ "$dir_part" = "." ] || [ "$dir_part" = "/" ]; then
    short_dir="$display_dir"
  else
    # Abbreviate each component of the prefix
    short_prefix=$(echo "$dir_part" | awk -F'/' 'BEGIN{OFS="/"}{for(i=1;i<=NF;i++) if($i!="" && $i!="~") $i=substr($i,1,1); print}')
    short_dir="${short_prefix}/${last_part}"
  fi
else
  short_dir=$(pwd | sed "s|^$HOME|~|")
fi

# --- Git branch ---
git_info=""
if git_branch=$(git -C "${cwd:-$(pwd)}" rev-parse --abbrev-ref HEAD 2>/dev/null); then
  git_dirty=""
  if ! git -C "${cwd:-$(pwd)}" diff --quiet 2>/dev/null || ! git -C "${cwd:-$(pwd)}" diff --cached --quiet 2>/dev/null; then
    git_dirty="*"
  fi
  git_ahead=$(git -C "${cwd:-$(pwd)}" rev-list --count @{u}..HEAD 2>/dev/null || echo "")
  git_behind=$(git -C "${cwd:-$(pwd)}" rev-list --count HEAD..@{u} 2>/dev/null || echo "")
  arrows=""
  [ "${git_ahead:-0}" -gt 0 ] 2>/dev/null && arrows="${arrows}↑"
  [ "${git_behind:-0}" -gt 0 ] 2>/dev/null && arrows="${arrows}↓"
  git_info="─[${git_branch}${git_dirty}${arrows}]"
fi

# --- Context window ---
ctx_info=""
if [ -n "$remaining" ]; then
  ctx_info=" ctx:${remaining}%"
fi

# --- Model ---
model_info=""
if [ -n "$model" ]; then
  model_info=" ${model}"
fi

# --- Assemble (ANSI colors: cyan = \033[36m, reset = \033[0m) ---
CYAN='\033[36m'
RESET='\033[0m'

printf "${CYAN}[%s]%s${RESET}%s%s" \
  "$short_dir" \
  "$git_info" \
  "$model_info" \
  "$ctx_info"

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ -f ~/dotconfig/bash/.bash_aliases ]; then
    source ~/dotconfig/bash/.bash_aliases
fi

if [ -f ~/dotconfig/bash/env.sh ]; then
    source ~/dotconfig/bash/env.sh
fi

if [ -f ~/dotconfig/bash/general.sh ]; then
    source ~/dotconfig/bash/general.sh
fi

if [ -f ~/dotconfig/bash/python.sh ]; then
    source ~/dotconfig/bash/python.sh
fi

if [ -f ~/dotconfig/bash/work_func.sh ]; then
    source ~/dotconfig/bash/work_func.sh
fi

eval "$(starship init bash)"
. "$HOME/.cargo/env"

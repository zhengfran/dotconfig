export PATH=$HOME/.local/bin:$PATH
export PATH=/usr/local/bin:$PATH
export EDITOR=nvim
export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_MANUAL_REBIND=1
export TERM=screen-256color-bce
if [[ $(hostname) == "IGAS116X" ]]; then
    export PATH=$HOME/tooling/klocwork/user64/bin:$PATH
fi


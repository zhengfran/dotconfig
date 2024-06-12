export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.cargo/bin:$PATH
export EDITOR=nvim
export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_MANUAL_REBIND=1
export OPENAI_API_KEY="$(cat ~/.netrc | awk '{print $NF}')"
export TERM=screen-256color-bce
if [[ $(hostname) == "IGAS116X" ]]; then
    export PATH=$HOME/tooling/klocwork/user64/bin:$PATH
fi
if [[ $(uname) == "Darwin" ]]; then
    export PATH=/opt/homebrew/Cellar/universal-ctags/HEAD-4ab3954/bin:$PATH
fi

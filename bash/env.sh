export LC_ALL=en_US.UTF-8 
export LANG=en_US.UTF-8
export PATH=$HOME/bin:$PATH
export PATH=$HOME/bin/nvim-linux64/bin:$PATH
export PATH=$HOME/bin/node/node/bin:$PATH
export PATH=$HOME/bin/clang-14/bin:$PATH
export PATH=$HOME/.local/bin:$PATH

export EDITOR=nvim
export OPENAI_API_KEY="$(cat ~/.netrc | awk '{print $NF}')"
export TERM=screen-256color-bce

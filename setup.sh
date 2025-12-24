
# Fetch config and set them up
git clone -b Rebirth git@github.com:zhengfran/dotconfig.git
##installtion
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
brew install zsh fzf fd ripgrep yazi lazygit
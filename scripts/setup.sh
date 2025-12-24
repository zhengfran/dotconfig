
# Fetch config if not present and set them up
if [ ! -d ~/dotconfig ]; then
    git clone -b Rebirth git@github.com:zhengfran/dotconfig.git ~/dotconfig
fi
# Symlink config files

##installtions
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
brew install zsh fzf fd ripgrep yazi lazygit
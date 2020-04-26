#!/bin/bash

WARNINGS="### WARNINGS"

echo -e "### Installing packages"
brew install fzf ripgrep vim neovim tmux python nvm zsh fd bat direnv kakoune || WARNINGS="${WARNINGS}\nCheck errors with brew"
$(brew --prefix)/opt/fzf/install --completion --key-bindings --update-rc --no-bash --no-fish || WARNINGS="${WARNINGS}\nCheck errors when installing fzf"
pip3 install --upgrade powerline-status
pip3 install --user --upgrade pynvim

mkdir -p $HOME/.nvm
nvm install node || WARNINGS="${WARNINGS}\nnvm could not install node"
npm install -g typescript
npm install -g typescript-language-server
brew install yarn

if [[ "$OSTYPE" == "darwin"* ]]; then
  brew cask install amethyst
  brew install ul/kak-lsp/kak-lsp
else
  WARNINGS="${WARNINGS}\nYou need to manually install kak-lsp"
fi

echo -e "### Installing (Neo)Vim plugin managers"
# vim-plug to manage plugins in vim
curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# same for neovim
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo -e "### Symlinking config files"
# symlink dotfiles into repository
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
ln -s $SCRIPTPATH/.vimrc  $HOME/.vimrc || WARNINGS="${WARNINGS}\nCheck symlinking vimrc"
ln -s $SCRIPTPATH/.tmux.conf  $HOME/.tmux.conf || WARNINGS="${WARNINGS}\nCheck symlinking tmux conf"
ln -s $SCRIPTPATH/.zshrc  $HOME/.zshrc || WARNINGS="${WARNINGS}\nCheck symlinking zshrc"
ln -s $SCRIPTPATH/.gitconfig  $HOME/.gitconfig || WARNINGS="${WARNINGS}\nCheck symlinking gitconfig"
mkdir -p $HOME/.config
ln -s $SCRIPTPATH/.config/nvim $HOME/.config/nvim || WARNINGS="${WARNINGS}\nCheck symlinking nvim config"
ln -s $SCRIPTPATH/.config/neomutt $HOME/.config/neomutt || WARNINGS="${WARNINGS}\nCheck symlinking neomutt config"
ln -s $SCRIPTPATH/.config/kak $HOME/.config/kak || WARNINGS="${WARNINGS}\nCheck symlinking kakoune rc"

if [[ "$OSTYPE" == "darwin"* ]]; then
  # Rust under MacOS uses another config path
  ln -s $SCRIPTPATH/.config/kak-lsp $HOME/Library/Preferences/kak-lsp || WARNINGS="${WARNINGS}\nCheck symlinking kak-lsp conf"
else
  ln -s $SCRIPTPATH/.config/kak-lsp $HOME/.config/kak-lsp || WARNINGS="${WARNINGS}\nCheck symlinking kak-lsp conf"
fi

echo -e "### Installing kakoune plugin manager"
mkdir -p $HOME/.config/kak/plugins
git clone https://github.com/andreyorst/plug.kak.git $HOME/.config/kak/plugins/plug.kak

echo -e ""
echo -e "### INFO"
echo -e "To finsh setup:"
echo -e " * Change the default shell \`chsh -s <path/to/brew/zsh>\`"
echo -e " * Install a patched font from https://github.com/ryanoasis/nerd-fonts"
echo -e "   and set it in your terminal emulator"
echo -e " * Install oh-my-zsh in zsh"
echo -e " * Run :PlugInstall in vim"
echo -e " * Run :plug-install in kakoune"
echo -e " * Add .~/.gitconfigemail and set your email address"
echo -e " * Don't forget to map capslock to <ESC>"
echo -e ""
echo -e "${WARNINGS}"

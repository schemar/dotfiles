#!/bin/bash

# install packages
brew install fzf ripgrep vim tmux python nvm zsh fd bat direnv
$(brew --prefix)/opt/fzf/install
pip3 install powerline-status

mkdir $HOME/.nvm
nvm install node
npm install -g typescript
brew install yarn

if [[ "$OSTYPE" == "darwin"* ]]; then
  brew cask install amethyst
fi

# vim-plug to manage plugins in vim
curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# symlink dotfiles into repository
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
ln -s $SCRIPTPATH/.vimrc  $HOME/.vimrc
ln -s $SCRIPTPATH/.tmux.conf  $HOME/.tmux.conf
ln -s $SCRIPTPATH/.zshrc  $HOME/.zshrc

echo ""
echo "To finsh setup:"
echo " * Change the default shell `chsh -s <path/to/brew/zsh>`"
echo " * Install a patched font from https://github.com/powerline/fonts"
echo "   and set it in your terminal emulator"
echo " * Install oh-my-zsh in zsh"
echo " * run :PlugInstall in vim"

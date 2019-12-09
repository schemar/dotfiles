#!/bin/zsh

# install packages
if [[ "$OSTYPE" == "darwin"* ]]; then
  brew install fzf ripgrep vim tmux python nvm yarn zsh fd bat direnv
  brew cask install amethyst
  pip install powerline-status
  nvm install node
  npm install -g typescript
else
  echo "Sorry! Mac only."
fi

# vim-plug to manage plugins in vim
curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# symlink dotfiles into repository
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
ln -s $SCRIPTPATH/.vimrc  $HOME/.vimrc
ln -s $SCRIPTPATH/.tmux.conf  $HOME/.tmux.conf
ln -s $SCRIPTPATH/.zshrc  $HOME/.zshrc

source $HOME/.zshrc

echo ""
echo "To finsh setup:"
echo " * Install a patched fron from https://github.com/powerline/fonts"
echo "   and set it in your terminal emulator"
echo " * run :PlugInstall in vim"

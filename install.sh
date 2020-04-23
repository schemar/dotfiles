#!/bin/bash

echo "### Installing packages"
brew install fzf ripgrep vim neovim tmux python nvm zsh fd bat direnv kakoune ul/kak-lsp/kak-lsp
$(brew --prefix)/opt/fzf/install
pip3 install --upgrade powerline-status
pip3 install --user --upgrade pynvim

mkdir -p $HOME/.nvm
nvm install node
npm install -g typescript
npm install -g typescript-language-server
brew install yarn

if [[ "$OSTYPE" == "darwin"* ]]; then
  brew cask install amethyst
fi

echo "### Installing (Neo)Vim plugin managers"
# vim-plug to manage plugins in vim
curl -fLo $HOME/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
# same for neovim
curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo "### Symlinking config files"
# symlink dotfiles into repository
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
ln -s $SCRIPTPATH/.vimrc  $HOME/.vimrc
ln -s $SCRIPTPATH/.tmux.conf  $HOME/.tmux.conf
ln -s $SCRIPTPATH/.zshrc  $HOME/.zshrc
ln -s $SCRIPTPATH/.gitconfig  $HOME/.gitconfig
mkdir -p $HOME/.config
ln -s $SCRIPTPATH/.config/nvim $HOME/.config/nvim
ln -s $SCRIPTPATH/.config/neomutt $HOME/.config/neomutt
ln -s $SCRIPTPATH/.config/kak $HOME/.config/kak
ln -s $SCRIPTPATH/.config/kak-lsp $HOME/.config/kak-lsp

echo "### Installing kakoune plugin manager"
mkdir -p $HOME/.config/kak/plugins
git clone https://github.com/andreyorst/plug.kak.git $HOME/.config/kak/plugins/plug.kak

echo ""
echo "To finsh setup:"
echo " * Change the default shell \`chsh -s <path/to/brew/zsh>\`"
echo " * Install a patched font from https://github.com/powerline/fonts"
echo "   and set it in your terminal emulator"
echo " * Install oh-my-zsh in zsh"
echo " * Run :PlugInstall in vim"
echo " * Run :plug-install in kakoune"
echo " * Add .~/.gitconfigemail and set your email address"
echo " * Don't forget to map capslock to <ESC>"

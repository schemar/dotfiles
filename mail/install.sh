#!/bin/bash

WARNINGS="### WARNINGS"

echo -e "### Installing packages"
brew install isync msmtp neomutt ripmime w3m python lbdb
pip3 install -I urlscan gcalcli vobject

echo -e "### Creating directories to store data"
mkdir -p $HOME/.mail/futurice
mkdir -p $HOME/.mutt
mkdir -p $HOME/.contacts

echo -e "### Symlinking config files"
# symlink dotfiles into repository
SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
[ ! -L $HOME/.mbsyncrc ] && ln -s $SCRIPTPATH/.mbsyncrc  $HOME/.mbsyncrc || WARNINGS="${WARNINGS}\nCheck symlinking mbsyncrc"
[ ! -L $HOME/.mutt ] && ln -s $SCRIPTPATH/.mutt  $HOME/.mutt || WARNINGS="${WARNINGS}\nCheck symlinking mutt"
[ ! -L $HOME/.config/certificates.pem ] && ln -s $SCRIPTPATH/.config/certificates.pem  $HOME/.config/certificates.pem || WARNINGS="${WARNINGS}\nCheck symlinking config/certificates.pem"

echo -e ""
echo -e "### INFO"
echo -e "To finsh setup:"
echo -e " * Add your mail password to the macOS keychain:"
echo -e "   Add a new login item"
echo -e "   Item name: http://imap.gmail.com"
echo -e "   Add a new login item"
echo -e "   Item name: smtp://smtp.gmail.com"
echo -e " * After syncing for the first time, fill your LBDB database:"
echo -e "  `./fill-lbdb.sh`"
echo -e ""
echo -e "${WARNINGS}"

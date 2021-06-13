#!/bin/bash

username="$(whoami)"
dotfiles_dir="$(cd "$(dirname "$0")" || exit; pwd -P)"
dotfiles_dir=$(echo "${dotfiles_dir}" | sed "s/\/ansible//")

ansible-playbook --ask-become-pass ./fedora.yml  --extra-vars "username=${username} dotfiles_dir=${dotfiles_dir}" "$@"

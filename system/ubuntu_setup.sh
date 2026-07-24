#!/usr/bin/env bash

set -exuo pipefail

echo "#"
echo "# This setup script is meant to be used with Ubuntu 26.04 LTS"
echo "#"

# Add extra sources:
sudo add-apt-repository ppa:linrunner/tlp

sudo apt update
sudo apt upgrade -y

# Conflicts with TLP:
sudo apt uninstall -y tuned tuned-ppd

sudo apt install -y \
  intel-media-driver \
  intel-vpl-gpu-rt \
  thermald \
  tlp \
  tlp-pd \
  fprintd \
  fprintd-pam \
  authselect \
  virt-manager \
  libvirt \
  podman \
  podman-compose \
  podman-docker \
  pipewire-pulseaudio \
  pavucontrol \
  blueman \
  imv \
  grim \
  slurp \
  swappy \
  firefox \
  fuzzel \
  mako \
  sway \
  swaylock \
  swayidle \
  swaybg \
  waybar \
  wl-copy \
  wtype \
  playerctl \
  fuse-libs \
  keepassxc \
  ghostty \
  zsh

chsh -s /usr/bin/zsh

sudo systemctl enable --now fstrim.timer
sudo systemctl enable --now tlp.service
sudo systemctl enable --now tlp-pd.service

# Add to libvirt group for access to libvirt/virt-manager/qmk:
sudo usermod -a -G libvirt "$USER"

echo ""
echo "## Nix"
echo ""

echo "Installing Nix"
echo "⚠️ You may want to select a newer version instead ⚠️"
curl https://nix-community.github.io/nix-installers/nix/x86_64/nix-multi-user-2.24.10.deb -o $HOME/Downloads/nix-multi-user-2.24.10.deb
sudo apt install $HOME/Downloads/nix-multi-user-2.24.10.deb

echo "Adding nixpkgs nix-channel. Currently 26.05."
nix-channel --add https://nixos.org/channels/nixos-26.05 nixpkgs
echo "Adding home-manager nix-channel. Currently 26.05."
nix-channel --add https://github.com/nix-community/home-manager/archive/release-26.05.tar.gz home-manager

nix-channel --update
nix-env -u '*'
nix-shell '<home-manager>' -A install
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

echo ""
echo "## Home Manager"
echo ""
echo "Running home-manager switch --flake ."
home-manager switch --flake .

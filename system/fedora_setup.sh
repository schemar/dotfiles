#!/usr/bin/env bash

set -exuo pipefail

echo "#"
echo "# This setup script is meant to be used with Fedora Sway Spin"
echo "#"

# Add extra source:
sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1
sudo dnf copr enable scottames/ghostty

sudo dnf upgrade --refresh

# Conflicts with TLP:
sudo dnf remove tuned tuned-ppd

sudo dnf install intel-media-driver \
  intel-vpl-gpu-rt \
  thermald \
  tlp \
  tlp-pd \
  nix \
  nix-daemon \
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

sudo dnf remove dunst

chsh -s /usr/bin/zsh

sudo systemctl enable --now fstrim.timer
sudo systemctl enable --now tlp.service
sudo systemctl enable --now tlp-pd.service

sudo systemctl enable --now nix-daemon
systemctl --user enable --now mako

echo "Adding nixpkgs nix-channel. Currently 26.05."
nix-channel --add https://nixos.org/channels/nixos-26.05 nixpkgs
echo "Adding home-manager nix-channel. Currently 26.05."
nix-channel --add https://github.com/nix-community/home-manager/archive/release-26.05.tar.gz home-manager

nix-channel --update
nix-env -u '*'
nix-shell '<home-manager>' -A install
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

echo "Running home-manager switch --flake ."
home-manager switch --flake .

#!/usr/bin/env bash

set -exuo pipefail

echo "#"
echo "# This setup script is meant to be used with Fedora Sway Spin"
echo "#"

# Add extra source:
sudo dnf install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1
sudo dnf copr enable scottames/ghostty

sudo dnf upgrade -y --refresh

# Conflicts with TLP:
sudo dnf remove -y tuned tuned-ppd

sudo dnf install -y \
  intel-media-driver \
  intel-vpl-gpu-rt \
  thermald \
  tlp \
  tlp-pd \
  fprintd \
  fprintd-pam \
  authselect \
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

sudo dnf remove dunst rofi

chsh -s /usr/bin/zsh

sudo systemctl enable --now fstrim.timer
sudo systemctl enable --now tlp.service
sudo systemctl enable --now tlp-pd.service

# Add to libvirt group for access to libvirt/virt-manager/qmk:
sudo usermod -a -G libvirt "$USER"

sudo systemctl enable --now nix-daemon
systemctl --user enable --now mako

echo ""
echo "## Nix"
echo ""
echo "Adding nixpkgs nix-channel. Currently 26.05."
nix-channel --add https://nixos.org/channels/nixos-26.05 nixpkgs
echo "Adding home-manager nix-channel. Currently 26.05."
nix-channel --add https://github.com/nix-community/home-manager/archive/release-26.05.tar.gz home-manager

nix-channel --update
nix-env -u '*'
nix-shell '<home-manager>' -A install
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

echo ""
echo "## Enrolling fingerprints"
echo ""
echo "Enrolling right index finger"
sudo fprintd-enroll -f right-index-finger "${USER}"
echo "Enrolling left index finger"
sudo fprintd-enroll -f left-index-finger "${USER}"
echo "Verifying fingerprints"
sudo fprintd-verify -f right-index-finger "${USER}"
sudo fprintd-verify -f left-index-finger "${USER}"
echo "Enabling fingerprint auth"
sudo authselect enable-feature with-fingerprint
sudo authselect apply-changes
echo "Verify 'with-fingerprint' is available"
sudo authselect current
echo "## Done with enrolling fingerprints"

echo ""
echo "## Home Manager"
echo ""
echo "Running home-manager switch --flake ."
home-manager switch --flake .

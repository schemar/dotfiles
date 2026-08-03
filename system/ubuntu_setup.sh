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
sudo apt remove -y tuned tuned-ppd

sudo apt install -y \
  intel-media-va-driver \
  thermald \
  tlp \
  tlp-pd \
  mate-polkit \
  authselect \
  kanshi \
  virt-manager \
  podman \
  podman-compose \
  podman-docker \
  pavucontrol \
  network-manager-gnome \
  blueman \
  curl \
  imv \
  grim \
  slurp \
  swappy \
  firefox \
  fuzzel \
  mako-notifier \
  sway \
  swaylock \
  swayidle \
  swaybg \
  waybar \
  wl-clipboard \
  wtype \
  playerctl \
  keepassxc \
  ghostty \
  zsh

snap install gtk-theme-breeze \
  gtk-theme-adw-gtk3 \
  vivaldi

chsh -s /usr/bin/zsh
sudo chsh -s /usr/bin/zsh

sudo systemctl enable --now fstrim.timer
sudo systemctl enable --now tlp.service
sudo systemctl enable --now tlp-pd.service
# Waybar lifecycle is managed by sway config:
systemctl --user disable --now waybar.service

# Disable ubuntu splash screen:
sudo sed -i 's/GRUB_CMDLINE_LINUX_DEFAULT=.*/GRUB_CMDLINE_LINUX_DEFAULT=""/' /etc/default/grub
sudo update-grub2

# Ubuntu is doing shenanigans with locales:
sudo localectl set-locale LANG=en_US.utf8 LC_TIME=de_DE.UTF-8

# Add to libvirt group for access to libvirt/virt-manager/qmk:
sudo usermod -a -G libvirt "$USER"

# Disable file transfer to stop the startup error for blueman:
gsettings set org.blueman.general plugin-list "['\!TransferService']"

echo ""
echo "## Nix"
echo ""

echo "Installing Nix"
echo "⚠️ You may want to select a newer version instead ⚠️"
curl https://nix-community.github.io/nix-installers/nix/x86_64/nix-multi-user-2.24.10.deb -o $HOME/Downloads/nix-multi-user-2.24.10.deb
sudo apt install $HOME/Downloads/nix-multi-user-2.24.10.deb

echo "Adding nixpkgs nix-channel. Currently 26.05."
/usr/bin/nix-channel --add https://nixos.org/channels/nixos-26.05 nixpkgs
echo "Adding home-manager nix-channel. Currently 26.05."
/usr/bin/nix-channel --add https://github.com/nix-community/home-manager/archive/release-26.05.tar.gz home-manager

/usr/bin/nix-channel --update
/usr/bin/nix-env -u '*'
/usr/bin/nix-shell '<home-manager>' -A install
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

echo ""
echo "## Home Manager"
echo ""
echo "Restart and run: home-manager switch --flake ."

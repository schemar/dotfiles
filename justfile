rebuild := if os() == 'linux' { 'nixos-rebuild' } else { 'darwin-rebuild' } 

default: help

# List available recipes
help:
  just --list --unsorted

# Update all installed homebrew packages (incl. casks)
brew-upgrade:
  brew update
  brew upgrade
  brew upgrade --casks --greedy

# Configure the system with this flake
nix-switch:
  sudo {{rebuild}} switch --flake .

nix-clean:
  nix-collect-garbage -d

nix-flake-update:
  nix flake update

nix-upgrade: nix-flake-update nix-switch

upgrade: brew-upgrade nix-upgrade

theme-dark:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'
  printf "dark" > ~/.config/current_theme_store
  /etc/profiles/per-user/$USER/bin/tmux source-file ~/.config/tmux/tmux.conf
  # See trap in ZSH config:
  pkill -USR1 zsh
  # Slack: #24273A,#C6A0F6,#C6A0F6,#91D7E3

theme-light:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to false'
  printf "light" > ~/.config/current_theme_store
  /etc/profiles/per-user/$USER/bin/tmux source-file ~/.config/tmux/tmux.conf
  # See trap in ZSH config:
  pkill -USR1 zsh
  # Slack: #FAF4ED,#6A67B4,#6A67B4,#007E7D

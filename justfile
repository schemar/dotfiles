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
  darwin-rebuild switch --flake .

nix-clean:
  nix-collect-garbage -d

nix-flake-update:
  nix flake update

nix-upgrade: nix-flake-update nix-switch

upgrade: nix-upgrade brew-upgrade

theme-dark:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'
  printf "dark" > ~/.config/current_theme_store
  /etc/profiles/per-user/$USER/bin/tmux source-file ~/.config/tmux/tmux.conf
  # Slack: #24273A,#C6A0F6,#C6A0F6,#91D7E3
  # ⚠️ Remember to reload zsh sessions to apply the changes.

theme-light:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to false'
  printf "light" > ~/.config/current_theme_store
  /etc/profiles/per-user/$USER/bin/tmux source-file ~/.config/tmux/tmux.conf
  # Slack: #FAF4ED,#6A67B4,#6A67B4,#007E7D
  # ⚠️ Remember to reload zsh sessions to apply the changes.

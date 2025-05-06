default: help

# List available recipes
help:
  just --list --unsorted

# Update brew's DB
brew-update:
  brew update

# Update all installed homebrew packages (incl. casks)
brew-upgrade: brew-update
  brew upgrade
  brew upgrade --casks --greedy

theme-dark:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'
  printf "dark" > ~/.config/current_theme_store
  /run/current-system/sw/bin/tmux source-file ~/.config/tmux/tmux.conf
  # Slack: #24273A,#C6A0F6,#C6A0F6,#91D7E3
  # ⚠️ Remember to reload zsh sessions to apply the changes.

theme-light:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to false'
  printf "light" > ~/.config/current_theme_store
  /run/current-system/sw/bin/tmux source-file ~/.config/tmux/tmux.conf
  # Slack: #FAF4ED,#6A67B4,#6A67B4,#007E7D
  # ⚠️ Remember to reload zsh sessions to apply the changes.

theme-blueberry-peach-update:
  ./tools/themes/update_blueberry_peach.lua

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
    sudo {{ rebuild }} switch --flake .

hm-switch:
    home-manager switch --flake .#$(whoami)@$(hostname)

nix-clean:
    nix-collect-garbage -d

nix-flake-update:
    nix flake update

nix-upgrade: nix-flake-update nix-switch

upgrade: brew-upgrade nix-upgrade

theme-dark:
    #!/usr/bin/env bash
    if [ "{{ os() }}" = "macos" ]; then
      osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'
      printf "#191724,#A19DD4,#A19DD4,#5EB1AF" | pbcopy
      echo "Copied slack colors to clipboard"
    fi

    printf "dark" > ~/.config/current_theme_store
    /etc/profiles/per-user/$USER/bin/tmux source-file ~/.config/tmux/tmux.conf
    # See trap in ZSH config:
    pkill -USR1 zsh

theme-light:
    #!/usr/bin/env bash
    if [ "{{ os() }}" = "macos" ]; then
      osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to false'
      printf "#FAF4ED,#6A67B4,#6A67B4,#007E7D" | pbcopy
      echo "Copied slack colors to clipboard"
    fi

    printf "light" > ~/.config/current_theme_store
    /etc/profiles/per-user/$USER/bin/tmux source-file ~/.config/tmux/tmux.conf
    # See trap in ZSH config:
    pkill -USR1 zsh

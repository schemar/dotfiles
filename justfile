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
    nh os switch .

# Configure the system with this flake after reboot
nix-boot:
    nh os boot .

darwin-switch:
    nh darwin switch .

darwin-upgrade: brew-upgrade flake-update darwin-switch

hm-switch:
    nh home switch . -c $(whoami)@$(hostname)

remote-switch user="schemar" host="klabautermann":
    nh os switch --target-host ssh://{{ user }}@{{ host }} --build-host ssh://{{ user }}@{{ host }} . -H {{ host }}

remote-boot user="schemar" host="klabautermann":
    nh os boot --target-host ssh://{{ user }}@{{ host }} --build-host ssh://{{ user }}@{{ host }} . -H {{ host }}

flake-update:
    nix flake update

nix-clean:
    nh clean all --keep 5 --optimise --ask

firmware-upgrade:
    #!/usr/bin/env bash
    fwupdmgr get-updates

    if [ $? == 2 ]; then
      # No updates available
      exit 0
    fi

    fwupdmgr update

theme-dark:
    #!/usr/bin/env bash
    if [ "{{ os() }}" = "macos" ]; then
      osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'
      osascript -e 'tell app "System Events" to tell every desktop to set picture to "{{ join(justfile_directory(), "assets", "images", "marc-linnemann-wDx3q0yb7fk-unsplash_darker.jpg") }}"'
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
      osascript -e 'tell app "System Events" to tell every desktop to set picture to "{{ join(justfile_directory(), "assets", "images", "neil-rosenstech-1o4Z1EwCkaY-unsplash.jpg") }}"'
      printf "#FAF4ED,#6A67B4,#6A67B4,#007E7D" | pbcopy
      echo "Copied slack colors to clipboard"
    fi

    printf "light" > ~/.config/current_theme_store
    /etc/profiles/per-user/$USER/bin/tmux source-file ~/.config/tmux/tmux.conf
    # See trap in ZSH config:
    pkill -USR1 zsh

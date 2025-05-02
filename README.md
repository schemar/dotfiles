# Dotfiles

This repository contains a nix configuration for my productivity stack.

## Installation

Requirements:

- [Nix](https://nixos.org/download.html)
  ```sh
  sh <(curl -L https://nixos.org/nix/install)
  ```
- [Homebrew](https://brew.sh/) (macOS only)
  ```sh
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
  ```

Install initially with:

```sh
# Errors will guide you on what to fix:
nix --extra-experimental-features "nix-command flakes" run nix-darwin/nix-darwin-24.11#darwin-rebuild -- switch --flake .
```

Change shell (not sure why nix doesn't do this):

```sh
chsh -s /run/current-system/sw/bin/zsh
```

Update config:

```sh
darwin-rebuild switch --flake .
```

## Included Configurations

See `./configs` for the full list of configurations.

### Remote

- [Q1 Max QMK Firmware](https://github.com/schemar/qmk_firmware/blob/wireless_playground/keyboards/keychron/q1_max/ansi_encoder/keymaps/schemar/keymap.c)

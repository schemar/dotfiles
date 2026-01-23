# Dotfiles

This repository contains a nix configuration for my productivity stack.

## Installation

Requirements:

> [!NOTE]
> Consider installing [lix](https://lix.systems/) instead of nix:

- [Nix](https://nixos.org/download.html)
  ```sh
  # macOS:
  sh <(curl -L https://nixos.org/nix/install)

  # Linux with SELinux disabled (multi-user):
  sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --daemon
  ```
- [Homebrew](https://brew.sh/) (macOS only)
  ```sh
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
  ```

Install initially with:

Generate an SSH keypair and upload the public key to GitHub (auth and signing):

```sh
# Use the default keyname (~/.ssh/id_ed25519) as it is used by the git config:
ssh-keygen -t ed25519 -C "martinschenck@fastmail.com"
```

Clone this repo (on a machine without git)

```sh
nix-shell -p git
git clone ...
```

Initial installation

```sh
# Errors will guide you on what to fix
# macOS:
nix --extra-experimental-features "nix-command flakes" run nix-darwin/master#darwin-rebuild -- switch --flake .

# Linux with home-manager only:
# (if necessary, add 'experimental-features = nix-command flakes' to /etc/nix/nix.conf
nix --extra-experimental-features "nix-command flakes" run home-manager/master -- switch --flake .#$(whoami)@$(hostname)
```

Change shell (not sure why nix doesn't do this):

```sh
chsh -s /run/current-system/sw/bin/zsh
```

Apply config:

```sh
# macOS:
darwin-rebuild switch --flake .

# Linux with home-manager only:
home-manager switch --flake .#$(whoami)@$(hostname)
```

Remaining steps:

- Set up Night Shift (System Preferences > Displays > Night Shift)

## Included Configurations

See `./configs` for the full list of configurations.

### Directory Structure

- **`system/`** - System-level configurations for NixOS and nix-darwin
  - `common.nix` - Shared settings across all systems
  - `darwin/` - macOS-specific system settings
  - `nixos/` - NixOS-specific system settings
- **`home/`** - User-level configurations (home-manager)
  - `schemar/` - My user configuration
    - `default.nix` - Common home-manager config
    - `standalone.nix` - Standalone home-manager entry point
- **`hosts/`** - Host-specific configurations
  - Each subdirectory represents a specific machine
- **`configs/`** - Application configurations (reusable home-manager modules)

### Remote

- [🫐 BlueberryPeach 🍑 Theme](https://github.com/schemar/blueberry-peach)
- [Q1 Max QMK Firmware](https://github.com/schemar/qmk_firmware/blob/wireless_playground/keyboards/keychron/q1_max/ansi_encoder/keymaps/schemar/keymap.c)

## Themeing

> Noting this down for my own memory ...

You can switch between light and dark mode with the just commands `just theme-light` and `just theme-dark`.
`just` will tell macOS to switch to light or dark mode, update the theme store at `~/.config/current_theme_store`, and reload `tmux`.

The theme files in `~/.config` are managaed in `./configs/theme/`.
The following programs use the theme store or its accompanying script to change their appearance:

- `bat`: There is a zsh-alias configured in `./configs/zsh/` which selects the theme.
- `delta`: There are both themes configured in `./configs/git/`. `git`'s pager is set tot a script which selects the theme.
- `fzf`: The zsh-configuration sources the fzf-theme based on the store value.
- `lazygit`: There is a zsh-alias configured in `./configs/zsh/` which selects the theme.
- `nvim`: `plugins/theme.lua` loads the theme from the store.
- `tmux`: `./configs/tmux/` sources the theme based on an `if-shell` in its config.
- `wezterm`: `./configs/wezterm/` loads the theme from the store.
- `zsh-syntax`: The zsh-configuration sources the theme based on the store value.

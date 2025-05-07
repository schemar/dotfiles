# Dotfiles

This repository contains a nix configuration for my productivity stack.

> [!WARNING]
> I am still in the process of switching from a simple dotfiles setup to an overcomplicated nix setup.
> Some files from ./configs are simply symlinked.
>
> See flake.nix as entry point.
>
> Uses nix-darwin and home-manager.

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

Generate an SSH keypair and upload the public key to GitHub (auth and signing):

```sh
# Use the default keyname (~/.ssh/id_ed25519) as it is used by the git config:
ssh-keygen -t ed25519 -C "martinschenck@fastmail.com"
```

## Included Configurations

See `./configs` for the full list of configurations.

### Remote

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
- `nvim`: `plugins/theme.lua` loads the theme from the store.
- `tmux`: `./configs/tmux/` sources the theme based on an `if-shell` in its config.
- `wezterm`: `./configs/wezterm/` loads the theme from the store.
- `zsh-syntax`: The zsh-configuration sources the theme based on the store value.

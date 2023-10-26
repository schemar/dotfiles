# Dotfiles

This repository contains configurations for my productivity stack and some basic visual improvements.

[![Screenshot NeoVim 2023](./resources/2023_neovim.jpg)](https://github.com/schemar/dotfiles/raw/main/resources/2023_neovim.png)

## Included Configurations

### Terminal

- [asdf](https://github.com/asdf-vm/asdf)
- [bat](https://github.com/sharkdp/bat)
- [bottom](https://github.com/ClementTsang/bottom)
- [direnv](https://github.com/direnv/direnv)
- [efm](https://github.com/mattn/efm-langserver)
- [fd](https://github.com/sharkdp/fd)
- [fzf](https://github.com/junegunn/fzf)
- [kmonad](https://github.com/david-janssen/kmonad)
- [lsd](https://github.com/lsd-rs/lsd)
- [NeoVim](https://github.com/neovim/neovim)❤️
- [procs](https://github.com/dalance/procs)
- [ranger](https://github.com/ranger/ranger)
- [ripgrep](https://github.com/BurntSushi/ripgrep)
- [starship](https://github.com/starship/starship)
- [tealdeer](https://github.com/dbrgn/tealdeer)
- [tig](https://github.com/jonas/tig)
- [tmux](https://github.com/tmux/tmux)❤️
- [Zsh](https://www.zsh.org/)

### Graphical System

- [autorandr](https://github.com/phillipberndt/autorandr)
- [dunst](https://github.com/dunst-project/dunst)
- [feh](https://github.com/derf/feh)
- [i3](https://github.com/i3/i3)❤️
- [picom](https://github.com/yshui/picom)
- [redshift](https://github.com/jonls/redshift)
- [rofi](https://github.com/davatorium/rofi)
- [wezterm](https://github.com/wez/wezterm)❤️

## Usage

An overview over the most common tasks follows below,
especially if they differ from the default.
Check the files' contents for more details.

### Command Line

- `<ctrl>+r` to fuzzy find in the shell's history.
- `<ctrl>+t` to fuzzy find paths within current directory.

### Tmux

Tmux sessions will be stored every 15 minutes and auto-restored when starting tmux the next time.
Persists across restarts.

Prefix is `<ctrl>-a`.

- `<prefix> {c,n,p}` create/next/previous window.
- `<prefix> {v,s}` split pane vertically/horizontally.
- `<ctrl>+{h,j,k,l}` to move cursor across panes (works also with vim windows).
- `<prefix> z` to (un)zoom the current pane.
- `<prefix> {<,>,+,-}` resize panes by fixed amount left/right/up/down.
- `<prefix> [` to enable copy mode. Use e.g. to scroll up to see older output.
  Movement in copy mode is vim-like (e.g. `<ctrl>+{d,u}` to move down/up a page).
- `<prefix> u` fuzzy find and open a URL from the buffer.
- `<prefix> tab` fuzzy find any previous tmux output (words, lines, URLs, paths).
- `<prefix> <alt-1>` split all windows evenly horizontally (`<alt-2>` for vertically).

### NeoVim

Prerequisites:

- `npm install -g tree-sitter`
- And possibly language tools which weren't automatically installed via mason.
  - For example `npm install -g eslint_d` after setup of `asdf` is complete.

Usage:

- When you first run NeoVim, `lazy.nvim` will install itself.
  After, run: `:Lazy` and `I` to `install` all plugins.
  Restart NeoVim.
- When you re-start NeoVim, `treesitter` should install its packages and `mason`
  should install all kinds of language servers and tools.
  Restart NeoVim.

### i3

Installation is based on Fedora i3 spin with i3 replaced with i3-gaps.

See the i3 config for all keybindings. Most relevant:

- `$mod` is by default "super"/"windows".
- `$mod+d` to launch application.
- `$mod+m` to see configuration options.
- `$mod+Shift+e` to exit, lock, etc.
- `$mod+r` to resize.
- `$mod+p` to take a screenshot.
- `$mod+Shift+c` to reload config.

#### Monitor layouts

The system includes [autorandr](https://github.com/phillipberndt/autorandr) to automatically detect and apply screen layouts based on the connected monitors.
If you have a new layout set up using for example arandr, run `autorandr --save <name>` to save the layout with the given name.
Autorandr should automatically apply this layout the next time the monitors are connected identically.
You can manually load a profile using `autorandr --load <name>`.

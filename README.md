# Dotfiles

This repository contains configurations for:

* Terminal in Linux (Ubuntu and Fedora), WSL (Ubuntu), and macOS
* Plasma with i3 on Fedora KDE

## Installation

You can use the `./installer` helper. It will install the packages and symlink the configs.
Run `./installer symlinks` if you only want to symlink all configs.

Requirements macOS (Linux and WSL don't have any special requirements):
* [Homebrew](https://docs.brew.sh/Installation)
* [Homebrew's requirements](https://docs.brew.sh/Installation)

## Included Configurations

### Terminal

* [bat](https://github.com/sharkdp/bat)
* [direnv](https://github.com/direnv/direnv)
* [fd](https://github.com/sharkdp/fd)
* [fzf](https://github.com/junegunn/fzf)
* [kakoune editor](https://github.com/mawww/kakoune)❤️
* [nb](https://github.com/xwmx/nb)
* [NeoVim](https://github.com/neovim/neovim)
* [nvm](https://github.com/nvm-sh/nvm)
* [ranger](https://github.com/ranger/ranger)
* [ripgrep](https://github.com/BurntSushi/ripgrep)
* [starship](https://github.com/starship/starship)
* [tig](https://github.com/jonas/tig)
* [tmux](https://github.com/tmux/tmux)❤️
* [Zsh](https://www.zsh.org/)

### X Window System

* [dunst](https://github.com/dunst-project/dunst)
* [feh](https://github.com/derf/feh)
* [i3](https://github.com/i3/i3)❤️
  * Integrated as [kwin](https://github.com/KDE/kwin) replacement in [plasma](https://kde.org/plasma-desktop/)
* [kitty](https://github.com/kovidgoyal/kitty)❤️
* [kmonad](https://github.com/david-janssen/kmonad)
* [picom](https://github.com/yshui/picom)
* [redshift](https://github.com/jonls/redshift)
* [rofi](https://github.com/davatorium/rofi)

## Usage

An overview over the most common tasks follows below, especially if they differ from the default. Check the files' contents for more details.

### Command Line

* `<ctrl>+r` to fuzzy find in the shell's history.
* `<ctrl>+t` to fuzzy find paths within current directory.
* `.nvmrc` files will automatically trigger `nvm use`.

### Tmux

Tmux sessions will be stored every 15 minutes and auto-restored when starting tmux the next time.
Persists across restarts.

Prefix is `<ctrl>-b`.

* `<prefix> {c,n,p}` create/next/previous window.
* `<prefix> {v,s}` split pane vertically/horizontally.
* `<ctrl>+{h,j,k,l}` to move curser across panes (works also with vim windows).
* `<prefix> z` to (un)zoom the current pane.
* `<prefix> {<,>,+,-}` resize panes by fixed amount left/right/up/down.
* `<prefix> [` to enable copy mode. Use e.g. to scroll up to see older output. Movement in copy mode is vim-like (e.g. `<ctrl>+{d,u}` to move down/up a page).
* `<prefix> u` fuzzy find and open a URL from the buffer.
* `<prefix> tab` fuzzy find any previous tmux output (words, lines, URLs, paths).

### Kakoune

Since kakoune has a helpful info panel, I don't need to list much here.
Check the info panel when entering user mode with `,`.

There are some files in the .config/kak/autoload/ directory in addition to the .config/kak/kakrc file.
Check there if something appears to be missing from kakrc.

You can add custom hooks or configurations to a `.kakrc.local` in your project root for project specific settings.

See https://github.com/schemar/kak-jsts/ for details on TypeScript formatting/linting.

### i3

Linux desktop installs include i3 as the window manager of KDE Plasma.

See the i3 config for all keybindings. Most relevant:

* `$mod` is by default "super"/"windows".
* `$mod+d` to launch application.
* `$mod+Shift+e` to exit, lock, etc.
* `$mod+r` to resize.
* `$mod+Shift+c` to reload config.

### (Neo)Vim

* `<ctrl>+w {v,s}` split window vertically/horizontally.
* `<ctrl>+{h,j,k,l}` to move cursor across vim windows (works also with tmux panes).
* `,h` to enable/disable search results highlighting.

#### Working with files:

* `,f` to fuzzy find files.
* `,b` to fuzzy find buffers.
* `,g` to ripgrep and then fuzzy find on the results.
* `,e` to open/close file explorer

#### For languages that support LSP:

* `,d` to go to definition of symbol under cursor.
* `,r` to list references of symbol under cursor.
* `,2` to rename symbol under cursor.

# dotfiles

dotfiles for Linux, WSL, and macOS.

An opinionated default setup for working on the command line.
Supports Fedora, Ubuntu 20,04+, Ubuntu WSL, and macOS.

## Installation

You can use the `./installer` helper.
Run `./installer symlinks` if you only want to symlink all configs.

Requirements macOS (Linux and WSL don't have any special requirements):
* [Homebrew](https://docs.brew.sh/Installation)
* [Homebrew's requirements](https://docs.brew.sh/Installation)

## Usage

### Included

* [Zsh](https://www.zsh.org/)
* [starship](https://github.com/starship/starship)
* [tmux](https://github.com/tmux/tmux)❤️
* [kakoune editor](https://github.com/mawww/kakoune)❤️
* [fzf](https://github.com/junegunn/fzf)
* [bat](https://github.com/sharkdp/bat)
* [ripgrep](https://github.com/BurntSushi/ripgrep)
* [fd](https://github.com/sharkdp/fd)
* [ranger](https://github.com/ranger/ranger)
* [tig](https://github.com/jonas/tig)
* [NeoVim](https://github.com/neovim/neovim)
* [nvm](https://github.com/nvm-sh/nvm)
* [direnv](https://github.com/direnv/direnv)
* [Amethyst (on macOS)](https://github.com/ianyh/Amethyst)
* [sway (on linux desktop)](https://github.com/swaywm/sway)❤️

An overview over the most common tasks follows below, especially if they differ from the default. Check the files' contents for more details.

### Command Line

* `,,<tab>` to fuzzy find a path. E.g. `cd ./Projects/,,<tab>` will let you fuzzy find directories inside the Projects directory.
* `<ctrl>+r` to fuzzy find in the shell's history.
* `<ctrl>+t` to fuzzy find files.
* `.nvmrc` files will automatically trigger `nvm use`.

### Inside Tmux

Tmux sessions will be stored every 15 minutes and auto-restored when starting tmux the next time.
Persists across restarts.

Prefix is `<ctrl>-space`.

* `<prefix> {c,n,p}` create/next/previous window.
* `<prefix> {v,s}` split pane vertically/horizontally.
* `<ctrl>+{h,j,k,l}` to move curser across panes (works also with vim windows).
* `<prefix> z` to (un)zoom the current pane.
* `<prefix> {<,>,+,-}` resize panes by fixed amount left/right/up/down.
* `<prefix> [` to enable copy mode. Use e.g. to scroll up to see older output. Movement in copy mode is vim-like (e.g. `<ctrl>+{d,u}` to move down/up a page).
* `<prefix> u` fuzzy find and open a URL from the buffer.

### Kakoune

Since kakoune has a helpful info panel, I don't need to list much here.
Check the info panel when entering user mode with `,`.

There are some files in the .config/kak/autoload/ directory in addition to the .config/kak/kakrc file.
Check there if something appears to be missing from kakrc.

In JavaScript/TypeScript files, you can check `,df` to lint and format with prettier and eslint.

You can add these to a `.kakrc.local` in your project for project specific settings.

See https://github.com/schemar/kak-jsts/ for details on TypeScript formatting/linting.

### Vim

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

### Sway/i3

Linux desktop installs include sway and i3 in a somewhat similar setup. Even though I love wayland and sway, we are not quite at wayland yet. I guess once electron apps come with chromium ozone and pipewire support, I won't need i3 anymore.

See the sway/i3 config for all keybindings. Most relevant:

* `$mod` is by default "super"/"windows".
* `$mod+d` to launch application.
* `$mod+Shift+e` to exit, lock, etc.
* `$mod+p` to take screenshots (saved to `~/Pictures/`).
* `$mod+m` to enter "maintenance mode" (display settings, etc.)
* `$mod+r` to resize.
* `$mod+Shift+c` to reload config.

Sway is set up with:

* Full screen sharing with Firefox.
* Red tinted display at night.
* Easy screenshot of screen area.
* Application launcher.
* Auto-lock and auto-sleep.
* Waybar:
  * Click "eye" to stop auto-lock/sleep.
  * Click "brightness" to manage displays.
  * Click "audio" to manage sound settings.
  * Click "network" to connect to WiFi.


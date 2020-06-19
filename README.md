# dotfiles

dotfiles for macOS, Linux, and WSL.

An opinionated default setup for working on the command line.

## Installation

You can use the `install.sh` helper.

Requirements:
* [Homebrew](https://docs.brew.sh/Installation) (on macOS, Linux, or WSL)
* [Homebrew's requirements](https://docs.brew.sh/Installation)

Post-installation:
* You need to change the default shell to zsh `chsh -s <path/to/brew/zsh>`
* You need to set a [Nerd Font](https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Regular/complete) as your terminal emulator's font.
* You need to run `<ctrl>+b I` inside tmux to install the tmux plug-ins.
* You need to run `:plug-install` inside kakoune.
* You need to run `:PlugInstall` inside NeoVim.
* You need to install oh-my-zsh inside zsh.

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

An overview over the most common tasks follows below, especially if they differ from the default. Check the files' contents for more details.

### Command Line

* `,,<tab>` to fuzzy find a path. E.g. `cd ./Projects/,,<tab>` will let you fuzzy find directories inside the Projects directory.
* `<ctrl>+r` to fuzzy find in the shell's history.
* `.nvmrc` files will automatically trigger `nvm use`.

### Inside Tmux

Tmux sessions will be stored every 15 minutes and auto-restored when starting tmux the next time.
Persists across restarts.

* `<ctrl>+b {c,n,p}` create/next/previous window.
* `<ctrl>+b {v,s}` split pane vertically/horizontally.
* `<ctrl>+{h,j,k,l}` to move curser across panes (works also with vim windows).
* `<ctrl>+b z` to (un)zoom the current pane.
* `<ctrl>+b {<,>,+,-}` resize panes by fixed amount left/right/up/down.
* `<ctrl>+b [` to enable copy mode. Use e.g. to scroll up to see older output. Movement in copy mode is vim-like (e.g. `<ctrl>+{d,u}` to move down/up a page).
* `<ctrl>+b u` fuzzy find and open a URL from the buffer.

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

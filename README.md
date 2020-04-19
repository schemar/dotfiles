# dotfiles

dotfiles for macOS, Linux, and WSL.

An opinionated default setup for working on the command line.

## Installation

You can use the `install.sh` helper.

Requirements:
* [Homebrew](https://docs.brew.sh/Installation) (on macOS, Linux, or WSL)
* [Homebrew's requirements](https://docs.brew.sh/Installation)

You need to change the default shell to zsh `chsh -s <path/to/brew/zsh>`

You need to set a [powerline font](https://github.com/powerline/fonts) as your terminal emulator's font.

You need to run `:PlugInstall` inside vim.

You need to run `:plug-install` inside kakoune.

You need to install oh-my-zsh inside zsh.

## Usage

An overview over the most common tasks follows below, especially if they differ from the default. Check the files' contents for more details.

**This is not a replacement for the documentation of the tools! Please read the manuals.**

### Command Line

* `,,<tab>` to fuzzy find a path. E.g. `cd ./Projects/,,<tab>` will let you fuzzy find directories inside the Projects directory.
* `<ctrl>+r` to fuzzy find in the shells history.
* `tmux` to open a new tmux session.
* `bat <filename>` to show a scrollable, syntax-highlighted file output.

### Inside Tmux

* `<ctrl>+b {c,n,p}` create/next/previous window.
* `<ctrl>+b {v,s}` split pane vertically/horizontally.
* `<ctrl>+{h,j,k,l}` to move curser across panes (works also with vim windows).
* `<ctrl>+b z` to (un)zoom the current pane.
* `<ctrl>+b {<,>}` resize panes by fixed amount.
* `<ctrl>+b [` to enable copy mode. Use e.g. to scroll up to see older output. Movement in copy mode is vim-like (e.g. `<ctrl>+{d,u}` to move down/up a page).

### Kakoune

Since kakoune has a helpful info panel, I will only list the main commands here:

* `,e` to show file explorer.
* `,f` to fuzzy find files, buffers, etc.
* `,s` to surround, e.g. with quotes.
* `,l` to interact with the language server, e.g. "show references", "rename", etc.

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

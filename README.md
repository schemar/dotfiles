# dotfiles

dotfiles for macOS.

## Helper

You can use the `install.sh` helper.

Requirements:
* [Homebrew](https://docs.brew.sh/Installation) (on macOS, Linux, or WSL)
* [Homebrew's requirements](https://docs.brew.sh/Installation)

## Linux and WSL
They work on Linux and WSL, but `.zshrc` needs tweaking:

Add brew bin to path:
`export PATH=/home/linuxbrew/.linuxbrew/bin:$PATH`

Update powerline location in `.zshrc` and `.tmux.conf`.

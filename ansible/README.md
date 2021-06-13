# Ansible Config Management

Auto workstation configuration based on a clean installation of [Fedora 34 i3 Spin](https://spins.fedoraproject.org/en/i3/).

1. Run `dnf install git ansible`
2. Run `ansible-galaxy collection install community.general`
3. Clone repo `git clone git@github.com:schemar/dotfiles.git && cd dotfiles/ansible`
4. Run `./install.sh`
  a. You can specify ansible options, e.g. `./install.sh --tags symlinks`
5. Run `systemctl reboot`
6. After reboot:
  a. Run `Ctrl+b I` inside tmux
  b. Run `:plug-install` inside kakoune
  c. Add `~/.gitconfigemail` and set your email address
  d. Run `sh -c "$(curl -fsSL https://starship.rs/install.sh)"` in your shell
  e. Set a wallpaper with `feh --bg-fill <path-to-file>`
  f. Download a kmonad release and make it available as `kmonad` in the path
    - https://github.com/kmonad/kmonad/releases

## TODO

- install asdf latest tag
- kmonad compose key
- autostart kmonad 
- install kmonad with api.GitHub.com and jq 
- monitor config with [autorandr](https://github.com/phillipberndt/autorandr)
  - including option to save and force apply with rofi
- rust
- broot
- [other cli tools](https://zaiste.net/posts/shell-commands-rust/#bonus)?
- npm packages
- based on installer:
 * In a new shell set https://github.com/asdf-vm/asdf-nodejs up
   (and/or any other asdf plugins) if you haven't already
 * Map printscr/menu to <composer>

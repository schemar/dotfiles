# Ansible Config Management

Auto workstation configuration based on a clean installation of [Fedora 34 i3 Spin](https://spins.fedoraproject.org/en/i3/).

1. Run `dnf install git ansible`
2. Run `ansible-galaxy collection install community.general`
3. Clone repo `git clone git@github.com:schemar/dotfiles.git && cd dotfiles/ansible`
3.1. Until ansible 2.10 is available, manually install i3-gaps: `sudo dnf install --allowerasing i3-gaps`
4. Run `./install.sh`
  a. You can specify ansible options, e.g. `./install.sh --tags symlinks`
  b. The `BECOME password` is your user password for `sudo`
5. Run `systemctl reboot`
6. After reboot (first-time set-up):
  a. Run `Ctrl+b I` inside tmux
  b. Run `:plug-install` inside kakoune
  c. Run `:adblock-update` inside qutebrowser
  d. Add `~/.gitconfigemail` and set your email address
  e. Run `sh -c "$(curl -fsSL https://starship.rs/install.sh)"` in your shell
  f. Set a wallpaper with `feh --bg-fill <path-to-file>`
  g. Download a kmonad release and make it available as `kmonad` in the path
    - https://github.com/kmonad/kmonad/releases
  h. Run `lxappearance` to set theme and icons
  i. Install `asdf`:
    - `git clone https://github.com/asdf-vm/asdf.git ~/.asdf`
    - `cd ~/.asdf`
    - `git checkout "$(git describe --abbrev=0 --tags)"`
  j. Install `nodejs` `asdf` plugin and latest:
    - `asdf plugin-add nodejs`
    - `asdf install nodejs`
  k. Reboot one last time

## TODO
- kmonad
  - autostart kmonad 
  - install kmonad with api.GitHub.com and jq?
  - update layers to be less intrusive
- auto-start applications with [dex](https://github.com/jceb/dex)? (fedora package `dex-autostart`)?
- auto-apply autorandr after boot
- fix redshift-gtk geo access (and CPU usage)
- fix `~/.xsession-errors`
- fix lacking i3blocks

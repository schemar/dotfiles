# Ansible Config Management

Auto workstation configuration based on a clean installation of [Fedora 34 i3 Spin](https://spins.fedoraproject.org/en/i3/).

1. Run `dnf install git` and `pip install --user ansible`
2. Run `ansible-galaxy collection install community.general`
3. Clone repo `git clone git@github.com:schemar/dotfiles.git && cd dotfiles/ansible`
4. Run `./install.sh`
  a. You can specify ansible options, e.g. `./install.sh --tags symlinks`
  b. The `BECOME password` is your user password for `sudo`
5. Install kmonad
  a. Install stack: `curl -sSL https://get.haskellstack.org/ | sh`
  b. `git clone https://github.com/kmonad/kmonad`
  c. `cd` into the directory and run `stack install`
  d. Copy the binary where root can access it: `cp ${HOME}/.local/bin/kmonad /usr/local/bin/`
6. Run `systemctl reboot`
7. After reboot (first-time set-up):
  a. Run `Ctrl+b I` inside tmux
  b. Run `:plug-install` inside kakoune
  c. Add `~/.gitconfigemail` and set your email address
  d. Run `sh -c "$(curl -fsSL https://starship.rs/install.sh)"` in your shell
  e. Set a wallpaper with `feh --bg-fill <path-to-file>`
  f. Run `lxappearance` to set theme and icons
  g. Install `asdf`:
    - `git clone https://github.com/asdf-vm/asdf.git ~/.asdf`
    - `cd ~/.asdf`
    - `git checkout "$(git describe --abbrev=0 --tags)"`
  h. Install `nodejs` `asdf` plugin and latest:
    - `asdf plugin-add nodejs`
    - `asdf install nodejs`
  i. Reboot one last time

## TODO
- fix redshift-gtk geo access (and CPU usage)
- fix `~/.xsession-errors`

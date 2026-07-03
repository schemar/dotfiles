{ lib, pkgs, ... }:
{
  services.blueman-applet = {
    enable = true;
  };

  home.packages = with pkgs; [
    swayidle
    swaybg
    glib
    kdePackages.polkit-kde-agent-1

    libnotify
    wl-clipboard
    playerctl

    networkmanager
    networkmanagerapplet

    pulseaudio
    pavucontrol
    blueman

    grim
    slurp
    swappy

    wtype # Type on wayland like xdotool; used by bemoji

    nautilus # gnome file manager

    eog # eye of gnome image viewer
    gimp
    thunderbird
    todoist-electron
  ];

  services.udiskie.enable = true;

  gtk = {
    enable = true;
    gtk4.theme = null;
  };
  qt = {
    enable = true;
  };

  home.file.".local/bin/lightmode.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        gsettings set org.gnome.desktop.interface gtk-theme 'Breeze'
        gsettings set org.gnome.desktop.interface icon-theme 'breeze'
        gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'

        printf "light" > ~/.config/current_theme_store
        tmux source-file ~/.config/tmux/tmux.conf
        pkill -USR1 zsh

        if [ "$XDG_SESSION_DESKTOP" = "sway" ]; then
          pkill swaybg
          swaybg --mode fill --image ${../assets/images/neil-rosenstech-1o4Z1EwCkaY-unsplash.jpg} &
        fi
      '';
  };
  home.file.".local/bin/darkmode.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        gsettings set org.gnome.desktop.interface gtk-theme 'Breeze-Dark'
        gsettings set org.gnome.desktop.interface icon-theme 'breeze-dark'
        gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'

        printf "dark" > ~/.config/current_theme_store
        tmux source-file ~/.config/tmux/tmux.conf
        pkill -USR1 zsh

        if [ "$XDG_SESSION_DESKTOP" = "sway" ]; then
          pkill swaybg
          swaybg --mode fill --image ${../assets/images/marc-linnemann-wDx3q0yb7fk-unsplash_darker.jpg} &
        fi
      '';
  };

  imports = [
    ./linux-desktop.nix
    ../configs/chromium
    ../configs/firefox
    ../configs/qutebrowser
  ];

  wayland.windowManager.sway.config.startup = [
    { command = "${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1"; }
  ];

}

{ lib, pkgs, ... }:
{
  services.blueman-applet = {
    enable = true;
  };

  # Fonts
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      emoji = [
        "Noto Color Emoji"
      ];
      monospace = [
        "MonoLisaCode"
        "Symbols Nerd Font Mono"
      ];
      sansSerif = [ "MonoLisaText" ];
      serif = [ ];
    };
  };

  home.packages = with pkgs; [
    # Configured when started by sway (see sway config).
    swayidle

    swaybg

    glib

    setxkbmap

    kdePackages.polkit-kde-agent-1
    kdePackages.breeze
    kdePackages.breeze-gtk
    kdePackages.breeze-icons

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

    bemoji
    wtype # Type on wayland like xdotool; used by bemoji

    nautilus # gnome file manager
    imv

    eog # eye of gnome image viewer
    gimp
    obsidian
    thunderbird
    todoist-electron
  ];

  services.udiskie.enable = true;

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "image/bmp" = [ "imv.desktop" ];
      "image/gif" = [ "imv.desktop" ];
      "image/jpeg" = [ "imv.desktop" ];
      "image/png" = [ "imv.desktop" ];
      "text/html" = [ "vivaldi-stable.desktop" ];
      "x-scheme-handler/about" = [ "vivaldi-stable.desktop" ];
      "x-scheme-handler/http" = [ "vivaldi-stable.desktop" ];
      "x-scheme-handler/https" = [ "vivaldi-stable.desktop" ];
      "x-scheme-handler/unknown" = [ "vivaldi-stable.desktop" ];
    };
  };

  # Ensure pointer is the right size:
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 32;
    x11 = {
      enable = true;
      defaultCursor = "Adwaita";
    };
    gtk.enable = true;
    sway.enable = true;
  };

  home.file.".local/bin/powermenu.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        menu_cmd=(fuzzel -d --prompt="Power > ")
        lock_cmd="swaylock"

        choice=$(printf " Lock\n󰗽 Logout\n Reboot\n󰐥 Shutdown\n󰒲 Sleep\n󰤄 Hibernate" \
          | "''${menu_cmd[@]}")

        case "$choice" in
          " Lock")
            $lock_cmd
            ;;
          "󰗽 Logout")
            swaymsg exit
            ;;
          " Reboot")
            systemctl reboot
            ;;
          "󰐥 Shutdown")
            systemctl poweroff
            ;;
          "󰒲 Sleep")
            systemctl sleep
            ;;
          "󰤄 Hibernate")
            systemctl hibernate
            ;;
        esac
      '';
  };

  home.file.".local/bin/settingsmenu.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        menu_cmd=(fuzzel -d --prompt="Settings > ")

        choice=$(printf " Audio\n󰛳 Network\n󰂯 Bluetooth\n Light Mode\n Dark Mode" \
          | "''${menu_cmd[@]}")

        case "$choice" in
          " Audio")
            pavucontrol
            ;;
          "󰛳 Network")
            nm-connection-editor
            ;;
          "󰂯 Bluetooth")
            blueman-manager
            ;;
          " Light Mode")
            "$HOME/.local/bin/lightmode.sh"
            ;;
          " Dark Mode")
            "$HOME/.local/bin/darkmode.sh"
            ;;
        esac
      '';
  };

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

  # Scaling in xresources:
  xresources = {
    extraConfig = ''
      *dpi: 192
      Xft.dpi: 192
      Xcursor.size: 32
    '';
  };

  # For some reason, the scaling in wayland makes the fonts way bigger. Adjusting:
  programs.ghostty.settings."font-size" = lib.mkForce 11.0;

  imports = [
    ../configs/avizo
    ../configs/chromium
    ../configs/firefox
    ../configs/fuzzel
    ../configs/sway
    ../configs/mako
    ../configs/qutebrowser
    ../configs/swaylock
    ../configs/waybar
  ];
}

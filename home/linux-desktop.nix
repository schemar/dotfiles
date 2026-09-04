{
  lib,
  pkgs,
  inputs,
  ...
}:
let
  # mode can be light or dark
  switchMode = mode: {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        gsettings set org.gnome.desktop.interface gtk-theme '${
          if mode == "light" then "Breeze" else "Breeze-Dark"
        }'
        gsettings set org.gnome.desktop.interface icon-theme '${
          if mode == "light" then "breeze" else "breeze-dark"
        }'
        gsettings set org.gnome.desktop.interface color-scheme 'prefer-${mode}'

        printf "${mode}" > ~/.config/current_theme_store
        tmux source-file ~/.config/tmux/tmux.conf
        pkill -USR1 -u "$UID" -x zsh
        makoctl mode -a ${mode} -r ${if mode == "light" then "dark" else "light"}
        ln -sf ~/.config/fuzzel/blueberry_peach_${mode}.ini ~/.config/fuzzel/blueberry_peach.ini

        ln -sf ~/.config/helix/themes/blueberry_peach_${mode}.toml ~/.config/helix/themes/blueberry_peach.toml
        pkill -USR1 -u "$UID" hx

        if [ "$XDG_SESSION_DESKTOP" = "sway" ]; then
          # Join the given theme file into a command that can be executed with swaymsg
          # Joins each non-empty line with a semicolon, leading to a number of
          # consecutive commands for swaymsg
          THEME_FILE=~/.config/sway/blueberry_peach_${mode}
          THEME_CMD=$(grep -v '^\s+$' "$THEME_FILE" | paste -sd';' -)
          swaymsg "$THEME_CMD"

          pkill swaybg
          swaybg --mode fill --image ${
            if mode == "light" then
              ../assets/images/rohit-tandon-9wg5jCEPBsw-unsplash_lighter.jpg
            else
              ../assets/images/marc-linnemann-wDx3q0yb7fk-unsplash_darker.jpg
          } &
        fi
      '';
  };
in
{

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

  home.packages = [
    pkgs.nh

    pkgs.setxkbmap
    pkgs.kdePackages.breeze
    pkgs.kdePackages.breeze-gtk
    pkgs.kdePackages.breeze-icons

    pkgs.bemoji
    pkgs.imv

    # Fonts:
    pkgs.lato
    pkgs.nerd-fonts.symbols-only
    pkgs.noto-fonts-color-emoji
    pkgs.open-sans
    pkgs.source-serif

    inputs.private-fonts.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

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
      "application/vnd.keepass" = [ "org.keepassxc.KeePassXC.desktop" ];
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

  home.file.".local/bin/lightmode.sh" = switchMode "light";
  home.file.".local/bin/darkmode.sh" = switchMode "dark";

  # For some reason, the scaling in wayland makes the fonts way bigger. Adjusting:
  programs.ghostty.settings."font-size" = lib.mkForce 11.0;

  imports = [
    ../configs/avizo
    ../configs/fuzzel
    ../configs/mako
    ../configs/sway
    ../configs/swaylock
    ../configs/waybar
  ];
}

{
  inputs,
  lib,
  pkgs,
  ...
}:
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
    pkgs.setxkbmap
    pkgs.kdePackages.breeze
    pkgs.kdePackages.breeze-gtk
    pkgs.kdePackages.breeze-icons

    pkgs.bemoji
    pkgs.imv

    pkgs.obsidian

    # Fonts:
    pkgs.lato
    pkgs.monaspace
    pkgs.nerd-fonts.symbols-only
    pkgs.noto-fonts-color-emoji
    pkgs.open-sans
    pkgs.source-serif

    inputs.private-fonts.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

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
          swaybg --mode fill --image ${../../assets/images/neil-rosenstech-1o4Z1EwCkaY-unsplash.jpg} &
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
          swaybg --mode fill --image ${../../assets/images/marc-linnemann-wDx3q0yb7fk-unsplash_darker.jpg} &
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

  # Get criteria with swaymsg -t get_outputs
  xdg.configFile."kanshi/config".text = ''
    profile laptop_only {
      output eDP-1 enable scale 2.0
    }
    profile {
      output eDP-1 disable 
      output "Dell Inc. DELL S2722QC 5Q7VLD3" enable scale 2.0
    }
    profile entelios_office {
      output eDP-1 disable
      output "Dell Inc. DELL U2412M Y1H5T27508CL" enable scale 1.0
    }
    profile entelios_office_2 {
      output eDP-1 disable
      output "Dell Inc. DELL U2414H 292K477303PL" enable scale 1.0
    }
  '';
  #
  # For some reason, the scaling in wayland makes the fonts way bigger. Adjusting:
  programs.ghostty.settings."font-size" = lib.mkForce 11.0;

  imports = [
    ../../configs/avizo
    ../../configs/fuzzel
    ../../configs/mako
    ../../configs/sway
    ../../configs/swaylock
    ../../configs/waybar
  ];

  wayland.windowManager.sway.config.startup = [
    { command = "blueman-applet"; }
  ];
}

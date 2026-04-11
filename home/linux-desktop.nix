{ config, pkgs, ... }:
{
  services.blueman-applet = {
    enable = true;
  };

  # Fonts
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # Configured when started by sway (see sway config).
    swayidle

    swaybg

    glib

    xautolock

    xset
    setxkbmap
    xbacklight
    feh

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

    bemoji
    wtype # Type on wayland like xdotool; used by bemoji

    # Fonts:
    lato
    monaspace
    nerd-fonts.symbols-only
    openmoji-color
    open-sans
    source-serif

    thunar

    discord-ptb # ptb (beta) as middle ground between stable and canary
    eog # eye of gnome image viewer
    gimp
    obsidian
    thunderbird
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

  home.file.".local/bin/powermenu.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        launcher="$1"

        # Check if argument is fuzzel or rofi
        if [[ "$launcher" != "fuzzel" && "$launcher" != "rofi" ]]; then
            echo "Usage: $0 {fuzzel|rofi}"
            exit 1
        fi

        # Build launcher command
        if [[ "$launcher" == "fuzzel" ]]; then
            menu_cmd=(fuzzel -d --prompt="Power > ")
            lock_cmd="swaylock"
        else
            menu_cmd=(rofi -dmenu -p "Power > ")
            lock_cmd="i3lock -n -c 191724"
        fi

        choice=$(printf " Lock\n󰗽 Logout\n Reboot\n󰐥 Shutdown\n󰒲 Hibernate" \
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
          "󰒲 Hibernate")
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

        launcher="$1"

        # Check if argument is fuzzel or rofi
        if [[ "$launcher" != "fuzzel" && "$launcher" != "rofi" ]]; then
            echo "Usage: $0 {fuzzel|rofi}"
            exit 1
        fi

        # Build launcher command
        if [[ "$launcher" == "fuzzel" ]]; then
            menu_cmd=(fuzzel -d --prompt="Settings > ")
        else
            menu_cmd=(rofi -dmenu -p "Settings > ")
        fi

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
    gtk3.extraConfig.gtk-application-prefer-dark-theme = true; # or false for light
    gtk4.theme = config.gtk.theme;
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

  imports = [
    ../configs/avizo
    ../configs/chromium
    ../configs/dunst
    ../configs/fuzzel
    ../configs/i3-sway/i3.nix
    ../configs/i3-sway/sway.nix
    ../configs/mako
    ../configs/polybar
    ../configs/rofi
    ../configs/swaylock
    ../configs/waybar
  ];
}

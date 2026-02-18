{ pkgs, ... }:
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

    xset
    setxkbmap
    xbacklight

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
    monaspace
    nerd-fonts.symbols-only
    openmoji-color
    open-sans
    source-serif

    discord-ptb # ptb (beta) as middle ground between stable and canary
    eog # eye of gnome image viewer
    gimp
    obsidian
    todoist-electron
    vivaldi
  ];

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
        else
            menu_cmd=(rofi -dmenu -p "Power > ")
        fi

        choice=$(printf "󰗼 Lock\n󰍃 Logout\n󰜉 Reboot\n󰐥 Shutdown\n󰒲 Hibernate" \
          | "''${menu_cmd[@]}")

        case "$choice" in
          "󰗼 Lock")
            swaylock
            ;;
          "󰍃 Logout")
            swaymsg exit
            ;;
          "󰜉 Reboot")
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

  home.file.".local/bin/lightmode.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        printf "light" > ~/.config/current_theme_store

        gsettings set org.gnome.desktop.interface gtk-theme 'Breeze'
        gsettings set org.gnome.desktop.interface icon-theme 'breeze'
        gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'

        tmux source-file ~/.config/tmux/tmux.conf
        pkill -USR1 zsh
      '';
  };
  home.file.".local/bin/darkmode.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        printf "dark" > ~/.config/current_theme_store

        gsettings set org.gnome.desktop.interface gtk-theme 'Breeze-Dark'
        gsettings set org.gnome.desktop.interface icon-theme 'breeze-dark'
        gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'

        tmux source-file ~/.config/tmux/tmux.conf
        pkill -USR1 zsh
      '';
  };

  # Scaling in xsession and xresources:
  xsession = {
    enable = true;

    profileExtra = ''
      # Scaling
      export QT_AUTO_SCREEN_SCALE_FACTOR=1
      export QT_ENABLE_HIGHDPI_SCALING=1
      export GDK_SCALE=2
    '';
  };
  xresources = {
    extraConfig = ''
      Xft.dpi: 192
    '';
  };

  imports = [
    ../configs/avizo
    ../configs/dunst
    ../configs/fuzzel
    ../configs/gtk
    ../configs/i3-sway/i3.nix
    ../configs/i3-sway/sway.nix
    ../configs/mako
    ../configs/polybar
    ../configs/qutebrowser
    ../configs/rofi
    ../configs/swaylock
    ../configs/waybar
  ];
}

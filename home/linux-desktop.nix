{ pkgs, ... }:
{
  services.blueman-applet = {
    enable = true;
  };

  home.packages = with pkgs; [
    # Sway
    # Configured when started by sway (see sway config):
    swayidle
    swaybg

    # i3
    i3status
    feh
    xautolock
    i3lock

    kdePackages.breeze
    kdePackages.breeze-gtk
    kdePackages.breeze-icons

    libnotify
    wl-clipboard
    playerctl

    networkmanager
    networkmanagerapplet

    pavucontrol
    blueman

    bemoji
    wtype # Type on wayland like xdotool; used by bemoji

    discord-ptb # ptb (beta) as middle ground between stable and canary
    eog # eye of gnome image viewer
    gimp
    obsidian
    todoist-electron
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

        choice=$(printf "󰗼 Lock\n󰍃 Logout\n󰜉 Reboot\n󰐥 Shutdown\n󰒲 Hibernate" | "${"$"}{menu_cmd[@]}")

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
            menu_cmd=(fuzzel -d --prompt="Power > ")
        else
            menu_cmd=(rofi -dmenu -p "Power > ")
        fi

        choice=$(printf " Audio\n󰛳 Network\n󰂯 Bluetooth\n Light Mode\n Dark Mode" \
          | "${"$"}{menu_cmd[@]}")

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

  wayland.windowManager.sway.config.bars = [
    {
      command = "${pkgs.waybar}/bin/waybar";
    }
  ];

  imports = [
    ../configs/avizo
    ../configs/dunst
    ../configs/fuzzel
    ../configs/gtk
    ../configs/mako
    ../configs/polybar
    ../configs/qutebrowser
    ../configs/rofi
    ../configs/sway-i3/i3.nix
    ../configs/sway-i3/sway.nix
    ../configs/swaylock
    ../configs/waybar
  ];
}

{ pkgs, ... }:
{
  services.blueman-applet = {
    enable = true;
  };

  home.sessionVariables = {
    NIXOS_OZONE_WL = "1"; # Enable wayland for Chrome (Electron) apps (e.g. Todoist).
  };

  home.packages = with pkgs; [
    # Configured when started by sway (see sway config).
    swayidle

    swaybg

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

        choice=$(printf "󰗼 Lock\n󰍃 Logout\n󰜉 Reboot\n󰐥 Shutdown\n󰒲 Hibernate" \
          | fuzzel -d --prompt="Power > ")

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

        choice=$(printf " Audio\n󰛳 Network\n󰂯 Bluetooth\n Light Mode\n Dark Mode" \
          | fuzzel -d --prompt="Settings > ")

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

        lookandfeeltool -a org.kde.breeze.desktop
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

        lookandfeeltool -a org.kde.breezedark.desktop
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
    ../configs/fuzzel
    ../configs/gtk
    ../configs/mako
    ../configs/qutebrowser
    ../configs/sway-i3/sway.nix
    ../configs/swaylock
    ../configs/waybar
  ];
}

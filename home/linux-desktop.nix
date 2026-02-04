{ pkgs, ... }:
{
  services.blueman-applet = {
    enable = true;
  };

  home.packages = with pkgs; [
    # Configured when started by sway (see sway config).
    swayidle

    swaybg

    libnotify
    wl-clipboard
    playerctl

    networkmanager
    networkmanagerapplet

    pavucontrol
    blueman

    bemoji
    wtype # Type on wayland like xdotool; used by bemoji
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

        choice=$(printf " Audio\n󰛳 Network\n󰂯 Bluetooth" \
          | fuzzel -d --prompt="Power > ")

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
        esac
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
    ../configs/mako
    ../configs/sway
    ../configs/swaylock
    ../configs/waybar
  ];
}

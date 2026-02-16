{ lib, pkgs, ... }:
{
  home.file.".local/bin/start-sway.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        # Session
        export XDG_SESSION_TYPE=wayland
        export XDG_SESSION_DESKTOP=sway
        export XDG_CURRENT_DESKTOP=sway

        # Wayland stuff
        export MOZ_ENABLE_WAYLAND=1
        export QT_QPA_PLATFORM=wayland
        export SDL_VIDEODRIVER=wayland
        export _JAVA_AWT_WM_NONREPARENTING=1
        export NIXOS_OZONE_WL=1

        exec ${pkgs.sway}/bin/sway "$@"
      '';
  };

  wayland.windowManager.sway = {
    enable = true;
    # This also starts a systemd sway-session.target:
    systemd.enable = true;

    wrapperFeatures.gtk = true; # Include fixes for GTK apps under Sway

    config =
      let
        commonConfig = import ./common-config.nix {
          inherit lib pkgs;
          commands = {
            powerCommand = "exec ~/.local/bin/powermenu.sh fuzzel";
            settingsCommand = "exec ~/.local/bin/settingsmenu.sh fuzzel";
            notificationDismissCommand = "exec ${pkgs.mako}/bin/makoctl dismiss";
            applicationCommand = "exec ${pkgs.fuzzel}/bin/fuzzel";
            emojiCommand = "exec ${pkgs.bemoji}/bin/bemoji --type";
            audioUpCommand = "exec ${pkgs.avizo}/bin/volumectl -u up";
            audioDownCommand = "exec ${pkgs.avizo}/bin/volumectl -u down";
            audioMuteCommand = "exec ${pkgs.avizo}/bin/volumectl toggle-mute";
            audioMicMuteCommand = "exec ${pkgs.avizo}/bin/volumectl -m toggle-mute";
            brightnessUpCommand = "exec ${pkgs.avizo}/bin/lightctl up";
            brightnessDownCommand = "exec ${pkgs.avizo}/bin/lightctl down";
          };
        };
      in
      lib.recursiveUpdate commonConfig {
        startup = [
          {
            command = ''
              swayidle -w \
                timeout 300 'swaylock -fF' \
                timeout 600 'swaymsg "output * dpms off"' \
                resume 'swaymsg "output * dpms on"' \
                before-sleep 'swaylock -fF'
            '';
          }

          {
            command = "nm-applet";
          }
        ];

        seat = {
          "*" = {
            hide_cursor = "when-typing enable";
          };
        };

        input = {
          "type:keyboard" = {
            # Map capslock to escape:
            "xkb_options" = "caps:escape";

            "repeat_delay" = "200";
            "repeat_rate" = "50";
          };
        };

        bars = [
          {
            command = "${pkgs.waybar}/bin/waybar";
          }
        ];
      };

    extraConfig = # sway
      ''
        for_window [shell="xwayland"] title_format "[XWayland] %title"

        # Make sure ghostty and tmux-server die when sway exits so that new sessions
        # start with new servers that attach to the correct dbus, etc.
        exec "swaymsg -mt subscribe '[]' || true && ${pkgs.tmux}/bin/tmux kill-server"
        exec "swaymsg -mt subscribe '[]' || true && pkill ghostty"
      '';
  };
}

{ lib, pkgs, ... }:
{
  home.file.".local/bin/start-i3.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        unset MOZ_ENABLE_WAYLAND
        unset QT_QPA_PLATFORM
        unset SDL_VIDEODRIVER
        unset _JAVA_AWT_WM_NONREPARENTING
        unset NIXOS_OZONE_WL

        unset WAYLAND_DISPLAY
        unset XDG_SESSION_TYPE
        unset XDG_SESSION_DESKTOP
        unset XDG_CURRENT_DESKTOP

        export XDG_SESSION_TYPE=x11
        export XDG_SESSION_DESKTOP=i3
        export XDG_CURRENT_DESKTOP=i3

        exec ${pkgs.i3}/bin/i3 "$@"
      '';
  };

  xsession.windowManager.i3 = {
    enable = true;

    config =
      let
        commonConfig = import ./common-config.nix {
          inherit lib pkgs;
          commands = {
            powerCommand = "exec ~/.local/bin/powermenu.sh rofi";
            settingsCommand = "exec ~/.local/bin/settingsmenu.sh rofi";
            # dunst manages its shortcut itself:
            notificationDismissCommand = null;
            applicationCommand = "exec ${pkgs.rofi}/bin/rofi -show drun";
            emojiCommand = null;
            audioUpCommand = "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 +5%";
            audioDownCommand = "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 -5%";
            audioMuteCommand = "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-mute 0 toggle";
            audioMicMuteCommand = null;
            brightnessUpCommand = "exec --no-startup-id ${pkgs.xbacklight}/bin/xbacklight -inc 20";
            brightnessDownCommand = "exec --no-startup-id ${pkgs.xbacklight}/bin/xbacklight -dec 20";
            searchCommand = "exec ${pkgs.rofi}/bin/rofi -show drun";
          };
        };
      in
      lib.recursiveUpdate commonConfig {
        startup = [
          {
            command = "${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${../../assets/images/daniel-leone-v7daTKlZzaw-unsplash.jpg}";
            always = true;
            notification = false;
          }

          {
            # Keyboard repeat rate:
            command = "${pkgs.xset}/bin/xset r rate 200 50";
            notification = false;
          }
          {
            # Map CapsLock to Escape:
            command = "${pkgs.setxkbmap}/bin/setxkbmap -option caps:escape";
            notification = false;
          }

          {
            command = "systemctl --user restart polybar.service";
            always = true;
            notification = false;
          }

          {
            command = "nm-applet";
          }
          {
            command = "blueman-applet";
          }
        ];

        # Polybar is started by start command above using systemctl.
        bars = [ ];
      };

    extraConfig = # i3config
      ''
        # Ensure borders for _all_ windows. Without this, ghostty and vivaldi
        # wouldn't have borders, for example.
        for_window [class="^.*"] border pixel 1

        # Update environment same as in sway module of home-manager:
        exec --no-startup-id "${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY XDG_CURRENT_DESKTOP XDG_SESSION_TYPE XDG_SESSION_DESKTOP XCURSOR_THEME XCURSOR_SIZE; systemctl --user reset-failed"

        # Make sure tmux-server, etc. exit when i3 exits so that new sessions
        # start with new servers that attach to the correct dbus, etc.
        exec --no-startup-id "i3-msg -mt subscribe '[]' || true && ${pkgs.tmux}/bin/tmux kill-server"
        exec --no-startup-id "i3-msg -mt subscribe '[]' || true && systemctl --user stop polybar.service; systemctl --user stop dunst.service"
      '';
  };
}

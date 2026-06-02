{ lib, pkgs, ... }:
{
  home.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    NIXOS_OZONE_WL = "1";
  };
  systemd.user.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    NIXOS_OZONE_WL = "1";
  };

  home.file.".local/bin/swaywait.sh" = {
    executable = true;
    text = # bash
      ''
        #!/usr/bin/env bash

        # Use this command to apply a sway command to an application once.
        # Example:
        # exec firefox
        # exec ~/.local/bin/swaywait.sh firefox 'move workspace 3'
        #
        # The above example would move the next firefox window to workspace 3
        # *once*. Future launches of firefox are unaffected.
        #
        # The script blocks until the provided app is started.
        # See also: https://gist.github.com/iguanajuice/4b34ef2d93a53b7e1e91ddeb70768c4e

        if [ $# -ne 2 ]
        then
            echo "Usage: swaywait.sh [app_id/class] [sway_command]"
            exit 1
        fi

        TARGET=$1
        COMMAND=$2

        swaymsg -t subscribe -m '["window"]' | while read line
        do
            CON=`echo $line | jq -r 'select(.change=="new").container'`
            APPID=`echo $CON | jq -r '.app_id'`
            CLASS=`echo $CON | jq -r '.window_properties.class'`
            CONID=`echo $CON | jq -r '.id'`

            if [ "$APPID" = "$TARGET" ] || [ "$CLASS" = "$TARGET" ]
            then
                swaymsg "[con_id=$CONID] $COMMAND"
                kill -PIPE 0
            fi
        done
      '';
  };

  wayland.windowManager.sway = {
    enable = true;

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
            # `grim -g "$(slurp)"` to capture the given coordinates
            # `grim -` (- as file) to send the result to stdout instead of a file
            # `swappy -f -` to read the file from stdin (-)
            screenshotCommand = "exec grim -g \"$(slurp)\" - | swappy -f -";
            fullScreenshotCommand = "exec grim - | swappy -f -";
            audioUpCommand = "exec ${pkgs.avizo}/bin/volumectl -u up";
            audioDownCommand = "exec ${pkgs.avizo}/bin/volumectl -u down";
            audioMuteCommand = "exec ${pkgs.avizo}/bin/volumectl toggle-mute";
            audioMicMuteCommand = "exec ${pkgs.avizo}/bin/volumectl -m toggle-mute";
            brightnessUpCommand = "exec ${pkgs.avizo}/bin/lightctl up";
            brightnessDownCommand = "exec ${pkgs.avizo}/bin/lightctl down";
            searchCommand = "exec ${pkgs.fuzzel}/bin/fuzzel";
          };
        };
      in
      lib.recursiveUpdate commonConfig {
        startup = [
          {
            command = "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_DATA_DIRS";
          }
          { command = "${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1"; }

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
          { command = "systemctl --user start mako"; }
          { command = "avizo-service"; }

          {
            command = "~/.local/bin/darkmode.sh";
          }
        ];

        seat = {
          "*" = {
            hide_cursor = "when-typing enable";
          };
        };

        input = {
          "type:keyboard" = {
            # Map capslock to escape;
            # Allows Umlauts and  with right alt as compose key;
            "xkb_options" = "caps:escape,compose:ralt";

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

    extraConfig = # swayconfig
      ''
        for_window [shell="xwayland"] title_format "[XWayland] %title"

        # Make sure tmux-server, etc. exit when sway exits so that new sessions
        # start with new servers that attach to the correct dbus, etc.
        exec --no-startup-id swaymsg -mt subscribe '[]' || true && ${pkgs.tmux}/bin/tmux kill-server
        exec --no-startup-id swaymsg -mt subscribe '[]' || true && pkill ghostty
        exec --no-startup-id swaymsg -mt subscribe '[]' || true && systemctl --user stop mako.service
      '';

    extraSessionCommands = ''
      export XDG_SESSION_DESKTOP=sway
      export XDG_CURRENT_DESKTOP=sway
      export XDG_SESSION_TYPE=wayland
      export MOZ_ENABLE_WAYLAND=1
      export QT_QPA_PLATFORM=wayland
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
  };
}

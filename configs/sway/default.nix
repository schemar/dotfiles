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

    config = import ./config.nix {
      inherit lib pkgs;
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

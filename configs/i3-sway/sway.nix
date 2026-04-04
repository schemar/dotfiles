{ lib, pkgs, ... }:
{
  home.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };
  systemd.user.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  wayland.windowManager.sway = {
    enable = true;

    systemd.enable = false;
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

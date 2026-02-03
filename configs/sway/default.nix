{ lib, pkgs, ... }:
{

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # Include fixes for GTK apps under Sway

    config = {
      focus = {
        followMouse = false;
      };

      fonts = {
        names = [
          "Monaspace Neon"
          "Symbols Nerd Font Mono"
        ];
        size = 13.0;
      };

      modifier = "Mod4"; # Use the Super/Windows key as the Mod key
      terminal = "${pkgs.ghostty}/bin/ghostty";

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

      input = {
        "type:keyboard" = {
          # Map capslock to escape:
          "xkb_options" = "caps:escape";
        };
      };

      keybindings = lib.mkOptionDefault {
        "Mod4+Shift+e" = "exec ~/.local/bin/powermenu.sh";

        "Mod4+d" = "exec ${pkgs.fuzzel}/bin/fuzzel";
        "Mod4+Shift+d" = "exec ${pkgs.bemoji}/bin/bemoji --type";
      };
    };
  };
}

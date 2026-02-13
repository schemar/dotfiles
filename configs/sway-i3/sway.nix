{ lib, pkgs, ... }:
{
  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;

    wrapperFeatures.gtk = true; # Include fixes for GTK apps under Sway

    config =
      let
        commonConfig = import ./common.nix {
          inherit lib pkgs;
          manager = "sway";
          launcher = "fuzzel";
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

        input = {
          "type:keyboard" = {
            # Map capslock to escape:
            "xkb_options" = "caps:escape";

            "repeat_delay" = "200";
            "repeat_rate" = "50";
          };
        };

        seat = {
          "*" = {
            hide_cursor = "when-typing enable";
          };
        };
      };

    extraConfig = # sway
      ''
        for_window [shell="xwayland"] title_format "[XWayland] %title"
      '';
  };
}

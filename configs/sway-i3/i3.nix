{ lib, pkgs, ... }:
{
  xsession.windowManager.i3 = {
    enable = true;

    config =
      let
        commonConfig = import ./common.nix {
          inherit lib pkgs;
          manager = "i3";
          launcher = "rofi";
        };
      in
      lib.recursiveUpdate commonConfig {
        # The Polybar service manages itself:
        bars = [ ];

        startup = [
          {
            command = ''
              ${pkgs.xautolock}/bin/xautolock -time 5 -locker "${pkgs.i3lock}/bin/i3lock"
            '';
          }

          {
            command = "--no-startup-id ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${../../assets/images/daniel-leone-v7daTKlZzaw-unsplash.jpg}";
          }
          {
            # Keybaord repeat rate:
            command = "--no-startup-id xset r rate 200 50";
          }
          {
            # Map CapsLock to Escape:
            command = "--no-startup-id setxkbmap -option caps:escape";
          }

          {
            command = "nm-applet";
          }
          {
            command = "blueman-applet";
          }
        ];

      };

    extraConfig = # i3
      ''
        # Ensure borders for _all_ windows. Without this, ghostty and vivaldi
        # wouldn't have borders, for example.
        for_window [class="^.*"] border pixel 1
      '';
  };
}

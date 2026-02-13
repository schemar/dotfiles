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
        startup = [
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
        ];
      };
  };
}

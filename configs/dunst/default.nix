{ lib, pkgs, ... }:
{
  # Make sure we don't start dunst when we're not in i3:
  systemd.user.services.dunst.Service.ExecStart =
    lib.mkForce "${pkgs.bash}/bin/bash -c '[ \"$XDG_CURRENT_DESKTOP\" = \"i3\" ] || exit 0; exec ${pkgs.dunst}/bin/dunst'";

  services.dunst = {
    enable = true;

    settings = {
      global = {
        width = "600";
        height = "(0,150)";
        offset = "(6,34)";
        origin = "top-right";
        gap_size = 6;
        padding = 8;
        transparency = 0;
        frame_color = "#A19DD4";
        frame_width = 1;
        font = "Monaspace Neon 11";
        scale = 2;
        close = "mod4+n";
      };

      urgency_normal = {
        background = "#0B0A0F";
        foreground = "#A2A2A9";
      };
    };
  };
}

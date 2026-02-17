{ ... }:
{
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

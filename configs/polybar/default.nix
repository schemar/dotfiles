{ ... }:
{
  services.polybar = {
    enable = true;

    settings = {
      "bar/top" = {
        width = "100%";
        height = "28px";
        bottom = false;

        dpi-x = 192;
        dpi-y = 192;
        modules = {
          left = [ "i3" ];
          center = [ ];
          right = [ ];
        };
      };

      "module/i3" = { };
    };

    script = "polybar top &";
  };
}

{ ... }:
{
  programs.waybar = {
    enable = true;

    settings = {
      main = {
        position = "top";
        height = 36;
        modules-left = [
          "sway/workspaces"
          "sway/mode"
        ];
        modules-center = [
          "clock"
        ];
        modules-right = [
          "pulseaudio"
          "bluetooth"
          "tray"
          "custom/power"
        ];

        clock = {
          format = "{0:%Y-%m-%d} {0:%H:%M}";
        };

        "custom/power" = {
          format = "";
          tooltip = "Power menu";
          on-click = "~/.local/bin/powermenu.sh";
          on-click-right = "swaylock";
        };
      };
    };
    style = # css
      ''
        * {
          font-family: Monaspace Neon, Symbols Nerd Font Mono;
          font-size: 18px;
        }
      '';
  };
}

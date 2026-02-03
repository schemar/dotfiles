{ ... }:
{
  programs.waybar = {
    enable = true;

    settings = {
      main = {
        position = "top";
        height = 28;
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

        pulseaudio = {
          format = "{icon} {volume}%";
          "format-bluetooth" = "󰂰 {volume}%";
          "format-bluetooth-muted" = "󰂲 {icon}";
          "format-muted" = "󰝟";
          "format-icons" = {
            "headphone" = "󰋋";
            "hands-free" = "󰥰";
            "headset" = "󰋎";
            "phone" = "󰏲";
            "portable" = "󰄝";
            "car" = "󰄋";
            "default" = [
              "󰕿"
              "󰖀"
              "󰕾"
            ];
          };
          "on-click" = "pavucontrol";
          "on-click-right" = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
          "on-scroll-up" = "pactl set-sink-volume @DEFAULT_SINK@ +2%";
          "on-scroll-down" = "pactl set-sink-volume @DEFAULT_SINK@ -2%";
        };

        "custom/power" = {
          format = "";
          tooltip = "Power menu";
          on-click = "~/.local/bin/powermenu.sh";
          on-click-right = "swaylock";
        };
      };
    };
    style = builtins.readFile ./style.css;
  };
}

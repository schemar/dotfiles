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
          "sway/scratchpad"
          "sway/mode"
        ];
        modules-center = [
          "mpd"
        ];
        modules-right = [
          "memory"
          "network"
          "pulseaudio"
          "bluetooth"
          "privacy"
          "idle_inhibitor"
          "tray"
          "clock"
          "custom/power"
        ];

        clock = {
          format = "{0:%Y-%m-%d} {0:%H:%M}";
          locale = "de_DE";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            format = {
              months = "<span color='#A2A2A9'><b>{}</b></span>";
              days = "<span color='#A2A2A9'><b>{}</b></span>";
              weeks = "<span color='#878794'><b>W{}</b></span>";
              weekdays = "<span color='#878794'><b>{}</b></span>";
              today = "<span color='#5EB1AF'><b><u>{}</u></b></span>";
            };
          };
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
  };

  xdg.configFile."waybar/style-light.css" = {
    source = ./style-light.css;
  };
  xdg.configFile."waybar/style-dark.css" = {
    source = ./style-dark.css;
  };
}

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
          "sway/window"
        ];
        modules-right = [
          "pulseaudio"
          "privacy"
          "idle_inhibitor"
          "tray"
          "clock"
          "custom/power"
        ];

        clock = {
          format = "{0:%Y-%m-%d} {0:%H:%M}";
          locale = "de_DE.utf8";
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
              today = "<span color='#7AA8CE'><b><u>{}</u></b></span>";
            };
          };
        };

        cpu = {
          format = "{usage}% ";
        };

        memory = {
          format = "{percentage}% ";
        };

        network = {
          format = "{ifname}";
          format-ethernet = "󰈀";
          format-wifi = "󰖩";
          format-disconnected = "󰖪";
          tooltip-format = "{ifname} via {gwaddr}";
          tooltip-format-wifi = "{essid} ({signalStrength}%)";
          tooltip-format-ethernet = "{ipaddr} ({ifname})";
          tooltip-format-disconnected = "Disconnected";
          max-length = 50;
          on-click = "nm-connection-editor";
        };

        bluetooth = {
          format = "{status} 󰂯";
          format-disabled = "󰂲";
          format-connected = "{num_connections} ";
          tooltip-format = "{controller_alias}\t{controller_address}";
          tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{device_enumerate}";
          tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
          on-click = "blueman-manager";
        };

        pulseaudio = {
          format = "{volume}% {icon}";
          "format-bluetooth" = "{volume}% 󰂰";
          "format-bluetooth-muted" = "{icon} 󰂲";
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

        idle_inhibitor = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };

        "sway/scratchpad" = {
          format = "{count} {icon}";
          format-icons = "󱞞";
        };

        "sway/workspaces" = {
          format = "{icon}";
          format-icons = {
            "1" = "";
            "2" = "";
            "3" = "";
            "4" = "󰲃";
            "5" = "";
            "6" = "";
            "7" = "󱍅";
          };
        };

        "custom/power" = {
          format = "";
          tooltip-format = "Power menu";
          on-click = "~/.local/bin/powermenu.sh fuzzel";
          on-click-right = "swaylock";
        };
      };
    };
  };

  xdg.configFile = {
    "waybar/style-dark.css".source = ./style-dark.css;
    "waybar/style-light.css".source = ./style-light.css;
  };
}

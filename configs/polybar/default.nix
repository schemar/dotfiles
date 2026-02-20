{ pkgs, ... }:
{
  systemd.user.services.polybar.Service.ExecCondition =
    "/bin/bash -c '[ \"$XDG_CURRENT_DESKTOP\" = \"i3\" ]'";

  services.polybar = {
    enable = true;

    package = pkgs.polybar.override {
      i3Support = true;
      mpdSupport = true;
      pulseSupport = true;
    };

    settings = {
      "bar/top" = {
        width = "100%";
        height = "28pt";
        bottom = false;

        dpi-x = 192;
        dpi-y = 192;

        font = [
          "Open Sans:weight=bold:size=11;4"
          "OpenMoji Color:size=11;4"
          "Symbols Nerd Font Mono:size=11;4"
        ];

        background = "#0B0A0F";
        foreground = "#A2A2A9";

        border-bottom-size = "1pt";
        border-color = "#A19DD4";

        # No separate padding, so both in one:
        module-margin = "2pt";

        modules = {
          left = "i3";
          center = "window";
          right = "cpu memory ethernet wifi audio bluetooth privacy idle tray clock power";
        };

      };

      settings = {
        format-padding = "10pt";
      };

      "module/i3" = {
        type = "internal/i3";

        ws-icon = [
          "1;"
          "2;"
          "3;"
          "4;󰲃"
          "5;"
          "6;"
          "7;󱍅"
        ];

        label-mode-background = "#7AA8CE";
        label-mode-foreground = "#0B0A0F";
        label-mode-padding = "10pt";

        label-focused = "%icon%";
        label-unfocused = "%icon%";
        label-visible = "%icon%";
        label-urgent = "%icon%";

        label-focused-padding = "10pt";
        label-unfocused-padding = "10pt";
        label-visible-padding = "10pt";
        label-urgent-padding = "10pt";

        label-focused-foreground = "#0B0A0F";
        label-focused-background = "#A19DD4";
      };

      "module/window" = {
        type = "internal/xwindow";
      };

      "module/cpu" = {
        type = "internal/cpu";
        label = "%percentage%% ";
      };

      "module/memory" = {
        type = "internal/memory";
        label = "%percentage_used%% ";
      };

      "module/ethernet" = {
        type = "internal/network";

        format-connected = "%{A1:${pkgs.networkmanagerapplet}/bin/nm-connection-editor:}<label-connected>%{A}";
        format-disconnected = "%{A1:${pkgs.networkmanagerapplet}/bin/nm-connection-editor:}<label-disconnected>%{A}";

        interface-type = "wired";
        label-connected = "󰈀";
        label-disconnected = "󰈂";
      };

      "module/wifi" = {
        type = "internal/network";

        format-connected = "%{A1:${pkgs.networkmanagerapplet}/bin/nm-connection-editor:}<label-connected>%{A}";
        format-disconnected = "%{A1:${pkgs.networkmanagerapplet}/bin/nm-connection-editor:}<label-disconnected>%{A}";

        interface-type = "wireless";
        label-connected = "󰖩";
        label-disconnected = "󰖪";
      };

      "module/audio" = {
        type = "internal/pulseaudio";

        click-right = "pavucontrol";

        format-volume = "%{A1:${pkgs.pavucontrol}/bin/pavucontrol:}%{A3:#audio.toggle:}<label-volume><ramp-volume>%{A}%{A}";
        format-muted = "%{A1:${pkgs.pavucontrol}/bin/pavucontrol:}%{A3:#audio.toggle:}<label-muted>%{A}%{A}";
        ramp-volume = [
          "󰕿"
          "󰖀"
          "󰖀"
          "󰕾"
          "󰕾"
        ];
        label-volume = "%percentage%%";
        label-muted = "󰝟";
      };

      "module/bluetooth" = {
        type = "custom/bluetooth";
      };

      "module/privacy" = {
        type = "custom/privacy";
      };

      "module/idle" = {
        type = "custom/idle";
      };

      "module/tray" = {
        type = "internal/tray";
      };

      "module/clock" = {
        type = "internal/date";

        # See "https://en.cppreference.com/w/cpp/io/manip/put_time" for details on how to format the date string
        # NOTE: if you want to use syntax tags here you need to use %%{...}
        date = "%Y-%m-%d%";
        # Optional time format
        time = "%H:%M";
        label = "%date% %time%";

        format = "<label>";
      };

      "module/power" = {
        type = "custom/text";

        format = "%{A1:~/.local/bin/powermenu.sh rofi:}<label>%{A} ";
        label = "";
      };
    };

    script = # bash
      ''
        exec polybar top &
      '';
  };
}

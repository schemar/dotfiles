{ pkgs, ... }:
{
  # Make sure we actually start:
  systemd.user.services.polybar.Unit.After = [ "graphical-session.target" ];
  systemd.user.services.polybar.Unit.PartOf = [ "graphical-session.target" ];
  systemd.user.services.polybar.Install.WantedBy = [ "graphical-session.target" ];

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
        # Twice due to scaling?:
        height = "56px";
        bottom = false;

        dpi-x = 192;
        dpi-y = 192;

        font = [
          "Monaspace Neon:size=11"
          "OpenMoji Color:size=11"
          "Symbols Nerd Font Mono:size=11"
        ];

        background = "#0B0A0F";
        foreground = "#A2A2A9";

        border-bottom-size = "1px";
        border-color = "#A19DD4";

        # No separate padding, so both in one:
        module-margin = "12px";

        modules = {
          left = "i3";
          center = "window";
          right = "cpu memory ethernet wifi audio bluetooth privacy idle tray clock power";
        };
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
        label-mode-padding = "10px";

        label-focused = "%icon%";
        label-unfocused = "%icon%";
        label-visible = "%icon%";
        label-urgent = "%icon%";

        label-focused-padding = "12px";
        label-unfocused-padding = "12px";
        label-visible-padding = "12px";
        label-urgent-padding = "12px";

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
        # Only run in i3
        [ "$XDG_CURRENT_DESKTOP" = "i3" ] || exit 0

        exec polybar top
      '';
  };
}

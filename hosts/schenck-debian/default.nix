{
  config,
  lib,
  pkgs,
  ...
}:
{
  # * Add shell to /etc/shells
  #   !! Needs updating after zsh upgrades (store path changes) if pkexec breaks.
  #   `nix eval --raw nixpkgs#zsh.outPath` helps.
  # * Install Debian's swaylock: sudo apt install swaylock
  # * After home-manager switch:
  #   * Enable the polkit service: systemctl --user enable --now polkit-kde-agent.service
  #   * Create the desktop file for sway (see below for details): /usr/share/wayland-sessions/sway.desktop

  # Make sure the direct path to the shell in the nix store is listed in /etc/shells
  # Example:
  # bat /etc/shells
  #   1 # /etc/shells: valid login shells
  #   2 /bin/sh
  #   3 /usr/bin/sh
  #   4 /bin/bash
  #   5 /usr/bin/bash
  #   6 /bin/rbash
  #   7 /usr/bin/rbash
  #   8 /usr/bin/dash
  #   9 /home/schemar/.nix-profile/bin/zsh
  #  10 /nix/store/309lg0w4dj1nbcr04pwzzkhvisfnmqqn-zsh-5.9/bin/zsh

  # The following statements are required to integrate home-manager better with
  # the KDE plasma session of the host:
  targets.genericLinux.enable = true;

  home.file.".config/plasma-workspace/env/10-home-manager-xdg-data-dirs.sh" = {
    executable = true;
    text = ''
      #!/bin/sh
      export XDG_DATA_DIRS="${config.home.sessionVariables.XDG_DATA_DIRS}"
    '';
  };

  # Make sure these are set in $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
  # That way they will be sourced properly by the sway wrapper script (see below).
  home.sessionVariables = {
    XDG_CURRENT_DESKTOP = "sway";
    XDG_SESSION_DESKTOP = "sway";

    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };
  systemd.user.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  systemd.user = {
    services.polkit-kde-agent = {
      # A polkit agent for graphical privilege escalation.
      # !! You must enable the service:
      # systemctl --user enable --now polkit-kde-agent.service
      Unit = {
        Description = "KDE Polkit Authentication Agent";
        # These targets exist on most systemd desktops. If one is missing, systemd will ignore ordering.
        After = [
          "graphical-session.target"
          "dbus.service"
        ];
        PartOf = [ "graphical-session.target" ];
        # Only start in sway, not e.g. plasma:
        ConditionEnvironment = "XDG_CURRENT_DESKTOP=sway";
      };

      Service = {
        Type = "simple";
        ExecStart = "/usr/lib/x86_64-linux-gnu/libexec/polkit-kde-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;

        # Force it to use X11 via XWayland (reliable under Sway)
        Environment = [
          "QT_QPA_PLATFORM=xcb"
        ];
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };

  # Sway desktop stuff.
  #
  # For sway to be available on the Plasma/SDDM debian host, add the following
  # file (!! REPLACE USERNAME):
  #
  # /usr/share/wayland-sessions/sway.desktop
  #
  # [Desktop Entry]
  # Name=Sway
  # Exec=/home/<USERNAME>/.nix-profile/bin/sway
  # Type=Application
  # DesktopNames=sway
  #
  # For swaylock to work, use Debian's binary to ensure a working PAM stack:
  # sudo apt install swaylock

  home.file.".local/bin/powermenu.sh".text = # bash
    ''
      #!/usr/bin/env bash

      choice=$(printf "󰗼 Lock\n󰍃 Logout\n󰜉 Reboot\n󰐥 Shutdown\n󰒲 Hibernate" \
        | fuzzel -d --prompt="Power > ")

      case "$choice" in
        "󰗼 Lock")
          swaylock
          ;;
        "󰍃 Logout")
          swaymsg exit
          ;;
        "󰜉 Reboot")
          systemctl reboot
          ;;
        "󰐥 Shutdown")
          systemctl poweroff
          ;;
        "󰒲 Hibernate")
          systemctl hibernate
          ;;
      esac
    '';
  home.file.".local/bin/powermenu.sh".executable = true;

  wayland.windowManager.sway = {
    enable = true;
    systemd.enable = true;
    wrapperFeatures.gtk = true; # Include fixes for GTK apps under Sway

    config = {
      focus = {
        followMouse = false;
      };

      fonts = {
        names = [
          "Monaspace Neon"
          "Symbols Nerd Font Mono"
        ];
        size = 13.0;
      };
      bars = [
        {
          command = "${pkgs.waybar}/bin/waybar";
        }
      ];

      modifier = "Mod4"; # Use the Super/Windows key as the Mod key
      terminal = "${pkgs.ghostty}/bin/ghostty";

      startup = [
        {
          command = ''
            swayidle -w \
              timeout 300 'swaylock -fF' \
              timeout 600 'swaymsg "output * dpms off"' \
              resume 'swaymsg "output * dpms on"' \
              before-sleep 'swaylock -fF'
          '';
        }

        {
          command = "nm-applet";
        }
      ];

      input = {
        "type:keyboard" = {
          # Map capslock to escape:
          "xkb_options" = "caps:escape";
        };
      };

      output = {
        "HDMI-A-1" = {
          scale = "1.5";
        };
      };

      keybindings = lib.mkOptionDefault {
        "Mod4+Shift+e" = "exec ~/.local/bin/powermenu.sh";

        "Mod4+d" = "exec ${pkgs.fuzzel}/bin/fuzzel";
        "Mod4+Shift+d" = "exec ${pkgs.bemoji}/bin/bemoji --type";
      };
    };

    extraConfig = ''
      xwayland enable
    '';
  };

  services.mako = {
    enable = true;

    settings = {
      default-timeout = 10000; # 10 seconds
      font = "Monaspace Neon 13";
    };
  };

  programs = {
    swaylock = {
      enable = true;
      # Disabled due to broken PAM stack with Debian. Instead:
      # sudo apt install swaylock
      package = null;

      settings = {
        color = "191724";
      };
    };

    waybar = {
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

    fuzzel = {
      enable = true;

      settings = {
        main = {
          font = "Monaspace Neon:size=13,Symbols Nerd Font Mono:size=13";
          terminal = "${pkgs.ghostty}/bin/ghostty";
        };
      };
    };
  };

  # Install host specific packages:
  home.packages = with pkgs; [
    # Configured when started by sway (see sway config).
    swayidle

    libnotify
    wl-clipboard

    xdg-desktop-portal-wlr
    slurp
    grim

    networkmanager
    networkmanagerapplet

    bemoji
    wtype # Type on wayland like xdotool; used by bemoji

    ghostty
  ];

  home.file.".config/systemd/user/xdg-desktop-portal-wlr.service".text = # ini
    ''
      [Unit]
      Description=xdg-desktop-portal-wlr
      PartOf=graphical-session.target
      After=graphical-session.target

      [Service]
      Type=dbus
      BusName=org.freedesktop.impl.portal.desktop.wlr
      ExecStart=${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr
      Restart=on-failure

      [Install]
      WantedBy=graphical-session.target
    '';

  # Overwrite default desktop file to not have "DBusActivatable=true"
  home.file.".local/share/applications/com.mitchellh.ghostty.desktop" = {
    text = ''
      [Desktop Entry]
      Version=1.0
      Name=Ghostty
      Type=Application
      Comment=A terminal emulator
      TryExec=${pkgs.ghostty}/bin/ghostty
      Exec=${pkgs.ghostty}/bin/ghostty --gtk-single-instance=true
      Icon=com.mitchellh.ghostty
      Categories=System;TerminalEmulator;
      Keywords=terminal;tty;pty;
      StartupNotify=true
      StartupWMClass=com.mitchellh.ghostty
      Terminal=false
      Actions=new-window;
      X-GNOME-UsesNotifications=true
      X-TerminalArgExec=-e
      X-TerminalArgTitle=--title=
      X-TerminalArgAppId=--class=
      X-TerminalArgDir=--working-directory=
      X-TerminalArgHold=--wait-after-command
      X-KDE-Shortcuts=Ctrl+Alt+T

      [Desktop Action new-window]
      Name=New Window
      Exec=${pkgs.ghostty}/bin/ghostty --gtk-single-instance=true
    '';
  };

  # Uses home-manager standalone module on debian linux:
  imports = [
    ../../home/standalone.nix
  ];
}

{
  config,
  pkgs,
  ...
}:
{
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
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  systemd.user.sessionVariables = config.home.sessionVariables;

  # Sway desktop stuff.
  #
  # For sway to be available on the Plasma/SDDM debian host, add the following
  # file (!! REPLACE USERNAME):
  #
  # /usr/share/wayland-sessions/sway.desktop
  #
  # [Desktop Entry]
  # Name=Sway
  # Exec=/home/<USERNAMW>/.local/bin/sway-session
  # Type=Application
  # DesktopNames=sway
  #
  # For swaylock to work, use Debian's binary to ensure a working PAM stack:
  # sudo apt install swaylock

  # Wrapper script to start sway to ensure config from home-manager is set:
  home.file.".local/bin/sway-session".text = ''
    #!/usr/bin/env bash
    set -e

    # Load Home Manager env (sets XDG_* and fixes PATH consistently)
    if [ -r "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
      source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    fi

    # Ensure dbus/systemd user services see the same env
    if command -v dbus-update-activation-environment >/dev/null 2>&1; then
      systemctl --user import-environment XDG_CONFIG_HOME XDG_CACHE_HOME XDG_DATA_HOME XDG_STATE_HOME PATH
      dbus-update-activation-environment --systemd --all
    fi

    exec sway
  '';
  home.file.".local/bin/sway-session".executable = true;

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # Include fixes for GTK apps under Sway:contentReference[oaicite:0]{index=0}
    config = {
      fonts = {
        names = [
          "Monaspace Neon"
          "Symbols Nerd Font Mono"
        ];
        size = 13.0;
      };
      bars = [
        {
          position = "top";
          statusCommand = "${pkgs.i3status}/bin/i3status";
          fonts = {
            names = [
              "Monaspace Neon"
              "Symbols Nerd Font Mono"
            ];
            size = 13.0;
          };
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
    };
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
  };

  # Install host specific packages:
  home.packages = with pkgs; [
    # Configured when started by sway (see sway config).
    swayidle

    libnotify
    wl-clipboard

    ghostty
  ];

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
      Exec=/nix/store/4fkwvnja07zmivbi0l68351rzd3166hp-ghostty-1.2.3/bin/ghostty --gtk-single-instance=true

    '';
  };

  # Uses home-manager standalone module on debian linux:
  imports = [
    ../../home/standalone.nix
  ];
}

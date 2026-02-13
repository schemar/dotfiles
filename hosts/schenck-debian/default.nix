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

  home.sessionVariables = {
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
          "sway.service"
        ];
        Wants = [ "sway.service" ];
        PartOf = [ "graphical-session.target" ];
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

  # For sway to be available on the Plasma/SDDM debian host, add the following
  # file (!! REPLACE USERNAME):
  # (Note that this sets the wayland specific env vars; for Xorg that's done in xsession/xprofile.)
  #
  # /usr/share/wayland-sessions/sway.desktop
  #
  # [Desktop Entry]
  # Name=Sway
  # Exec=/usr/bin/env XDG_SESSION_TYPE=wayland XDG_CURRENT_DESKTOP=sway XDG_SESSION_DESKTOP=sway NIXOS_OZONE_WL=1 /home/<USERNAME>/.nix-profile/bin/sway
  # Type=Application
  # DesktopNames=sway
  #
  # For swaylock to work, use Debian's binary to ensure a working PAM stack:
  # sudo apt install swaylock
  #
  # For i3 to be available on the Plasma/SDDM debian host, add the following
  # file (!! REPLACE USERNAME):
  #
  # /usr/share/xsessions/i3.desktop
  #
  # [Desktop Entry]
  # Name=i3
  # Comment=i3 window manager
  # Exec=dbus-run-session -- /home/<your-username>/.nix-profile/bin/i3
  # Type=Application
  # DesktopNames=i3

  wayland.windowManager.sway = {
    config = {
      output = {
        "HDMI-A-1" = {
          scale = "2";
          bg = "${../../assets/images/daniel-leone-v7daTKlZzaw-unsplash.jpg} fill";
        };
      };
    };
  };

  xsession = {
    enable = true;

    profileExtra = ''
      # Scaling
      QT_AUTO_SCREEN_SCALE_FACTOR=1
      QT_ENABLE_HIGHDPI_SCALING=1
      GDK_SCALE=2

      # Make sure wayland isn't leaking:
      unset WAYLAND_DISPLAY
      unset NIXOS_OZONE_WL
      unset XDG_CURRENT_DESKTOP
      unset XDG_SESSION_TYPE
      export XDG_CURRENT_DESKTOP=i3
      export XDG_SESSION_TYPE=x11
    '';
  };

  xresources = {
    extraConfig = ''
      Xft.dpi: 192
    '';
  };

  # For some reason, the scaling in wayland makes the fonts way bigger. Adjusting:
  programs.ghostty.settings."font-size" = lib.mkForce 11.0;

  # Install host specific packages:
  home.packages = with pkgs; [
    vivaldi
  ];

  # Uses home-manager standalone module on debian linux:
  imports = [
    ./screencast.nix
    ../../home/linux-desktop.nix
    ../../home/standalone.nix
  ];
}

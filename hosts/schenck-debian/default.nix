{
  config,
  lib,
  ...
}:
{
  # * Add shell to /etc/shells
  #   !! Needs updating after zsh upgrades (store path changes) if pkexec breaks.
  #   `nix eval --raw nixpkgs#zsh.outPath` helps.
  # * Install Debian's swaylock: sudo apt install swaylock
  # * After home-manager switch:
  #   * Create the desktop file for sway and i3 (see below for details)

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
  systemd.user.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  systemd.user = {
    services.polkit-kde-agent = {
      # A polkit agent for graphical privilege escalation.
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
  # Exec=/home/<USERNAME>/.local/bin/start-sway.sh
  # Type=Application
  # DesktopNames=sway
  #
  # For swaylock to work, use Debian's binary to ensure a working PAM stack:
  # sudo apt install swaylock

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

  # For some reason, the scaling in wayland makes the fonts way bigger. Adjusting:
  programs.ghostty.settings."font-size" = lib.mkForce 11.0;

  # Uses home-manager standalone module on debian linux:
  imports = [
    ./screencast.nix
    ../../home/linux-desktop.nix
    ../../home/standalone.nix
  ];
}

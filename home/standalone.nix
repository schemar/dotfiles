{
  config,
  pkgs,
  username,
  ...
}:
{
  # Standalone home-manager configuration for user
  # Use this for systems where home-manager is run standalone (not as a NixOS/nix-darwin module)

  targets.genericLinux.enable = true;

  home.file.".config/plasma-workspace/env/10-home-manager-xdg-data-dirs.sh" = {
    executable = true;
    text = ''
      #!/bin/sh
      export XDG_DATA_DIRS="${config.home.sessionVariables.XDG_DATA_DIRS}"
    '';
  };

  systemd.user.sessionVariables = config.home.sessionVariables;

  home.username = username;
  home.homeDirectory = "/home/${username}";

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };

  # Fonts
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    ghostty

    monaspace
    nerd-fonts.symbols-only
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

  imports = [
    ./default.nix
  ];
}

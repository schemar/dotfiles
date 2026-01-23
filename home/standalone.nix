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

  imports = [
    ./default.nix
  ];
}

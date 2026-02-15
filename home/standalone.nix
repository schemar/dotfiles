{
  pkgs,
  username,
  ...
}:
{
  # Standalone home-manager configuration for user
  # Use this for systems where home-manager is run standalone (not as a NixOS/nix-darwin module)

  home.username = username;
  home.homeDirectory = "/home/${username}";

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;
  };

  imports = [
    ./default.nix
  ];
}

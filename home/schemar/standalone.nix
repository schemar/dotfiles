{ ... }:
{
  # Standalone home-manager configuration for user "schemar"
  # Use this for systems where home-manager is run standalone (not as a NixOS/nix-darwin module)

  home.username = "schemar";
  home.homeDirectory = "/home/schemar";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports = [
    ./default.nix
  ];
}

{ ... }:
{
  home.username = "schemar";
  home.homeDirectory = "/home/schemar";

  home.stateVersion = "24.11"; # ok if you mean to baseline there

  programs.home-manager.enable = true;

  imports = [
    ./home.nix
  ];
}

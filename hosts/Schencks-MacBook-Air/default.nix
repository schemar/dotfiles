{ inputs, username, ... }:
{
  # Host configuration for Schencks-MacBook-Air (nix-darwin)

  imports = [
    # System-level configurations:
    ../../system/common.nix
    ../../system/darwin.nix
    ../../system/home-manager.nix

    # Home-manager as a nix-darwin module:
    inputs.home-manager.darwinModules.home-manager
  ];
}

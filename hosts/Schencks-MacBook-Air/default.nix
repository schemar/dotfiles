{ inputs, ... }:
{
  # Host configuration for Schencks-MacBook-Air (nix-darwin)

  imports = [
    # System-level configurations:
    ../../system/common.nix
    ../../system/darwin.nix

    # Home-manager as a nix-darwin module:
    inputs.home-manager.darwinModules.home-manager
  ];

  # Configure home-manager to use the schemar user config:
  home-manager.users.schemar = {
    imports = [
      ../../home/schemar
    ];
  };

  # Host-specific overrides:
  home-manager.extraSpecialArgs.npmAlias = null;
}

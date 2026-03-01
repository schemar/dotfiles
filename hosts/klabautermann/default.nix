{ inputs, username, ... }:
{
  # Host configuration for klabautermann (NixOS)

  nixpkgs.hostPlatform = "x86_64-linux";

  imports = [
    # NixOS-specific configs:
    ./configuration.nix

    # System-level configurations:
    ../../system/common.nix
    ../../system/nixos.nix

    # Home-manager as a NixOS module:
    inputs.home-manager.nixosModules.home-manager
  ];

  # Configure home-manager to use the user config:
  home-manager.users.${username} = {
    imports = [
      ../../home
    ];
  };
}

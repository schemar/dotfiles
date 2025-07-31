{ ... }:
{
  nixpkgs.hostPlatform = "aarch64-linux";

  imports = [
    ./configuration.nix
    ../common.nix
    ../../users/schemar/common.nix
    ../../users/schemar/nixos.nix
  ];
}

{ ... }:
{
  nixpkgs.hostPlatform = "aarch64-linux";

  users.users.schemar.openssh.authorizedKeys.keys = [
    ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK/4l6M0rsHmCuYHkUxuk42+gKN/tySO9CRLp0NOUjuH''
  ];

  imports = [
    ./configuration.nix
    ../common.nix
    ../../users/schemar/common.nix
    ../../users/schemar/nixos.nix
  ];
}

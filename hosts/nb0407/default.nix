{
  inputs,
  username,
  pkgs,
  ...
}:
{
  imports = [
    # NixOS-specific configs:
    ./configuration.nix
    ../../system/common.nix
    ../../system/nixos-desktop.nix
    ../../system/secure-boot.nix

    # Home-manager as a NixOS module:
    inputs.home-manager.nixosModules.home-manager
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  systemd.network.enable = true;

  environment.systemPackages = with pkgs; [
    keepassxc
    gh
  ];

  virtualisation.podman.enable = true;
  # Have a "docker" command that runs podman.
  # Useful for projects that have a justfile with docker commands.
  virtualisation.podman.dockerCompat = true;

  home-manager.users.${username} = {
    imports = [
      ../../home/default.nix
      ../../home/linux-desktop.nix
      ./home.nix
    ];
  };
}

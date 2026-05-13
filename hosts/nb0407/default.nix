{
  inputs,
  username,
  lib,
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

  virtualisation = {
    podman = {
      enable = true;
      # Have a "docker" command that runs podman.
      # Useful for projects that have a justfile with docker commands.
      dockerCompat = true;
    };
    virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
      # Workaround for kernel issues with VirtualBox 7.2.4 and Kernel 7.0.5:
      enableKvm = true;
      # KVM only supports NAT:
      addNetworkInterface = false;
    };
  };
  users.extraGroups.vboxusers.members = [ username ];
  systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;

  home-manager.users.${username} = {
    imports = [
      ../../home/default.nix
      ../../home/linux-desktop.nix
      ./home.nix
    ];
  };
}

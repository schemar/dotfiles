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

    # RTS Repo:
    python3
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

    wayland.windowManager.sway.config.startup = [
      {
        command = "${pkgs.ghostty}/bin/ghostty && ~/.local/bin/swaywait.sh com.mitchellh.ghostty 'move workspace 1'";
      }
      {
        command = "${pkgs.vivaldi}/bin/vivaldi && ~/.local/bin/swaywait.sh vivaldi-stable 'move workspace 2'";
      }
      {
        command = "${pkgs.todoist-electron}/bin/todoist-electron && ~/.local/bin/swaywait.sh Todoist 'move workspace 3'";
      }
      {
        command = "${pkgs.obsidian}/bin/obsidian && ~/.local/bin/swaywait.sh obsidian 'move workspace 7'";
      }

      {
        command = "swaymsg 'workspace number 5";
      }
    ];
  };
}

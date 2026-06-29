{
  inputs,
  config,
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

  # SSD trimming:
  services.fstrim.enable = true;
  # Lid handling:
  services.logind = {
    enable = true;
    settings.Login = {
      HandleLidSwitch = "suspend";
      HandleLidSwitchExternalPower = "suspend";
      HandleLidSwitchDocked = "ignore";
    };
  };
  # Prevent CPU overheating:
  services.thermald.enable = true;
  # Battery save management:
  services.tlp = {
    enable = true;
    pd.enable = true;
    settings = {
      # No wifi power saving ever:
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "off";
    };
  };
  # Intel GPU (including "xe" driver):
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      # Required for modern Intel GPUs (Xe iGPU and ARC)
      intel-media-driver # VA-API (iHD) userspace
      vpl-gpu-rt # oneVPL (QSV) runtime

      # Optional (compute / tooling):
      intel-compute-runtime # OpenCL (NEO) + Level Zero for Arc/Xe
    ];
  };
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "iHD"; # Prefer the modern iHD backend
  };
  boot.initrd.kernelModules =
    lib.mkIf (lib.versionAtLeast config.boot.kernelPackages.kernel.version "6.8")
      [ "xe" ];

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

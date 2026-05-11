{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "aegir"; # Define your hostname.
  networking.networkmanager.enable = true;

  swapDevices = [
    {
      device = "/dev/nvme0n1p6";
      # Solid state drives have fast random access times, which make them great
      # for swap if you ignore the limited lifespan. Enabling TRIM (discard) on
      # the swap files can help avoid unnecessary copy actions on the SSD,
      # reducing wear and potentially helping increase performance.
      # https://wiki.nixos.org/wiki/Swap#discard
      options = [ "discard" ];
    }
  ];

  time.timeZone = "Europe/Berlin";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "de_DE.UTF-8";
      LC_IDENTIFICATION = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_NAME = "de_DE.UTF-8";
      LC_NUMERIC = "de_DE.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_TELEPHONE = "de_DE.UTF-8";
      LC_TIME = "de_DE.UTF-8";
    };
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "de_DE.UTF-8/UTF-8"
    ];
  };

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  users.users.schemar = {
    isNormalUser = true;
    description = "Martin Schenck";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };

  system.stateVersion = "25.11";
}

{
  modulesPath,
  lib,
  pkgs,
  username,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ./hardware-configuration.nix
  ];

  boot.loader.grub = {
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    # devices = [ ];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  environment.systemPackages = map lib.lowPrio [
    pkgs.curl
    pkgs.gitMinimal
  ];

  networking.hostName = "klabautermann";

  # SSH configuration:
  services.openssh = {
    enable = true;
    allowSFTP = false;
    ports = [ 22 ];

    # https://infosec.mozilla.org/guidelines/openssh#modern-openssh-67
    settings = {
      LogLevel = "VERBOSE";

      # only auth via authorizedKeys
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;

      KexAlgorithms = [
        "curve25519-sha256@libssh.org"
        "ecdh-sha2-nistp521"
        "ecdh-sha2-nistp384"
        "ecdh-sha2-nistp256"
        "diffie-hellman-group-exchange-sha256"
      ];
      Ciphers = [
        "chacha20-poly1305@openssh.com"
        "aes256-gcm@openssh.com"
        "aes128-gcm@openssh.com"
        "aes256-ctr"
        "aes192-ctr"
        "aes128-ctr"
      ];
      Macs = [
        "hmac-sha2-512-etm@openssh.com"
        "umac-128-etm@openssh.com"
        "hmac-sha2-512"
        "umac-128@openssh.com"
      ];
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK/4l6M0rsHmCuYHkUxuk42+gKN/tySO9CRLp0NOUjuH"
  ];
  users.users.${username}.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK/4l6M0rsHmCuYHkUxuk42+gKN/tySO9CRLp0NOUjuH"
  ];

  system.stateVersion = "24.05";
}

{ ... }:
{
  nixpkgs.hostPlatform = "aarch64-linux";

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
    ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIChFNUALd5qGQ96dfCbPAwLq/qV1v7xOCXJlyFCwMSkV''
    ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKgAC0Py5KkT0hrmIKcXHLGKe1/57+/0A1RjNB1BPS6i''
  ];
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

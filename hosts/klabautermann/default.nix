{ inputs, ... }:
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

  # Configure home-manager to use the schemar user config:
  home-manager.users.schemar = {
    imports = [
      ../../home/schemar
    ];
  };

  # Host-specific overrides:
  home-manager.extraSpecialArgs.npmAlias = null;

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
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIChFNUALd5qGQ96dfCbPAwLq/qV1v7xOCXJlyFCwMSkV"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKgAC0Py5KkT0hrmIKcXHLGKe1/57+/0A1RjNB1BPS6i"
  ];
  users.users.schemar.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK/4l6M0rsHmCuYHkUxuk42+gKN/tySO9CRLp0NOUjuH"
  ];

  # ACME/Let's Encrypt configuration:
  security.acme = {
    acceptTerms = true;
    defaults.email = "martinschenck@fastmail.com";
  };

  # Nginx web server:
  services.nginx = {
    enable = true;
    virtualHosts."klabautermann.schemar.net" = {
      enableACME = true;
      forceSSL = true;

      locations."/" = {
        return = # html
          ''
            200 '<html style="background-color: black;">
              <body>
                <div style="color: grey; max-width: 800px; margin: 0 auto;">
                  <h1>Klabautermann</h1>
                  <p>https://klabautermann.schemar.net/</p>
                </div>
              </body>
            </html>'
          '';
        extraConfig = ''
          default_type text/html;
        '';
      };
    };
  };
}

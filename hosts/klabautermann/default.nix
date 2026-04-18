{ inputs, username, ... }:
{
  # Host configuration for klabautermann (NixOS)

  nixpkgs.hostPlatform = "x86_64-linux";

  imports = [
    # NixOS-specific configs:
    ./configuration.nix

    # System-level configurations:
    ../../system/common.nix

    # Home-manager as a NixOS module:
    inputs.home-manager.nixosModules.home-manager
  ];

  # Create the user:
  users.users.${username} = {
    isNormalUser = true;
    # "wheel" for sudo:
    extraGroups = [ "wheel" ];
  };

  # Passwordless sudo for user:
  security.sudo.extraRules = [
    {
      users = [ username ];
      commands = [
        {
          command = "ALL";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

  # Configure home-manager to use the user config:
  home-manager.users.${username} = {
    imports = [
      ../../home
    ];
  };
}

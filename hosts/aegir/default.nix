{
  inputs,
  username,
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

  # SSD trimming:
  services.fstrim.enable = true;
  # Prevent CPU overheating:
  services.thermald.enable = true;

  # AMD GPU:
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  home-manager.users.${username} = {
    imports = [
      ../../home/default.nix
      ../../home/nixos-desktop.nix
      {
        # Configure output of this host:
        wayland.windowManager.sway = {
          config = {
            output = {
              "HDMI-A-1" = {
                scale = "2";
                bg = "${../../assets/images/marc-linnemann-wDx3q0yb7fk-unsplash_darker.jpg} fill";
              };
            };
          };
        };

      }
    ];
  };
}

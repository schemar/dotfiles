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

    # Home-manager as a NixOS module:
    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager.users.${username} = {
    imports = [
      ../../home/default.nix
      ../../home/linux-desktop.nix
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

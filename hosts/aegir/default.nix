{
  lib,
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
        # For some reason, the scaling in wayland makes the fonts way bigger. Adjusting:
        programs.ghostty.settings."font-size" = lib.mkForce 11.0;

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

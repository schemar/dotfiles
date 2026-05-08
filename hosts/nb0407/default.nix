{
  inputs,
  username,
  pkgs,
  ...
}:
{
  imports = [
    # NixOS-specific configs:
    ./configuration.nix
    ../../system/common.nix

    # Secure Boot
    ./secure-boot.nix

    # Home-manager as a NixOS module:
    inputs.home-manager.nixosModules.home-manager
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;
  security.pam.services = {
    greetd.enableGnomeKeyring = true;
    swaylock.enableGnomeKeyring = true;
  };
  security.polkit.enable = true;

  systemd.network.enable = true;

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        # Shows battery charge of connected devices on supported
        # Bluetooth adapters. Defaults to 'false'.
        Experimental = true;
        # When enabled other devices can connect faster to us, however
        # the tradeoff is increased power consumption. Defaults to
        # 'false'.
        FastConnectable = true;
      };
      Policy = {
        # Enable all controllers when they are found. This includes
        # adapters present on start as well as adapters that are plugged
        # in later on. Defaults to 'true'.
        AutoEnable = true;
      };
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-wlr
    ];
    config.common.default = "*";
  };

  programs.dconf.enable = true;
  programs.sway.enable = true;

  environment.systemPackages = with pkgs; [
    tuigreet

    keepassxc

    vim
    gh
  ];

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd sway";
        user = "greeter";
      };
    };
  };

  services.gnome.gnome-keyring.enable = true;

  virtualisation.podman.enable = true;
  # Have a "docker" command that runs podman.
  # Useful for projects that have a justfile with docker commands.
  virtualisation.podman.dockerCompat = true;

  home-manager.users.${username} = {
    imports = [
      ../../home/default.nix
      ../../home/linux-desktop.nix
      {
        services.kanshi = {
          enable = true;

          # Get criteria with swaymsg -t get_outputs
          settings = [
            {
              profile.name = "undocked";
              profile.outputs = [
                {
                  criteria = "eDP-1";
                  scale = 2.0;
                  status = "enable";
                }
              ];
            }
            {
              profile.name = "home_office";
              profile.outputs = [
                {
                  criteria = "eDP-1";
                  status = "disable";
                }
                {
                  criteria = "Dell Inc. DELL S2722QC 5Q7VLD3";
                  scale = 2.0;
                  status = "enable";
                }
              ];
            }
            {
              profile.name = "entelios_office";
              profile.outputs = [
                {
                  criteria = "eDP-1";
                  status = "disable";
                }
                {
                  criteria = "Dell Inc. DELL U2412M Y1H5T27508CL";
                  scale = 1.0;
                  status = "enable";
                }
              ];
            }
          ];
        };
        services.podman.enable = true;

        home.packages = with pkgs; [
          gh
          prek

          podman-compose

          universal-ctags
          watchexec

          remmina # RDP

          # RTS repo:
          python310
          gcc
          gnumake
        ];
      }
    ];
  };
}

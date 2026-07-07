{
  pkgs,
  ...
}:
{
  targets.genericLinux.enable = true;

  # Get criteria with swaymsg -t get_outputs
  xdg.configFile."kanshi/config".text = ''
    profile laptop_only {
      output eDP-1 enable scale 2.0
    }
    profile {
      output eDP-1 disable 
      output "Dell Inc. DELL S2722QC 5Q7VLD3" enable scale 2.0
    }
    profile entelios_office {
      output eDP-1 disable
      output "Dell Inc. DELL U2412M Y1H5T27508CL" enable scale 1.0
    }
    profile entelios_office_2 {
      output eDP-1 disable
      output "Dell Inc. DELL U2414H 292K477303PL" enable scale 1.0
    }
  '';

  wayland.windowManager.sway.config.startup = [
    { command = "blueman-applet"; }
    { command = "${pkgs.kdePackages.polkit-kde-agent-1}/libexec/polkit-kde-authentication-agent-1"; }
  ];

  imports = [
    ../../home
    ../../home/standalone.nix
    ../../home/linux-desktop.nix
    {
      home.packages = with pkgs; [
        gh
        (pkgs.writeShellScriptBin "vpn" ''
          function printhelp {
              echo "Usage: vpn COMMAND"
              echo ""
              echo "Valid options for COMMAND:"
              echo "up"
              echo "down"
              echo "kill"
              echo "help"
          }

          if [ "$#" -eq 0 ]; then
              printhelp
              exit 1
          fi

          case "$1" in
          up)
              sudo swanctl --initiate --child checkpoint
              ;;
          down)
              sudo swanctl --terminate --child checkpoint
              ;;
          kill)
              echo "# => Terminate the next command with 'C-c' if it hangs!"
              echo "# => Killing will commence anyway."
              sudo swanctl --terminate --child checkpoint
              sudo pkill -f vpn-dns-updown
              sudo systemctl restart strongswan
              ;;
          help)
              printhelp
              ;;
          *)
              printhelp
              exit 1
              ;;
          esac
        '')
      ];

      # Disable the packages that are managed by "parent fedora":
      programs.ghostty.package = null;
      programs.ghostty.systemd.enable = false;
      programs.swaylock.package = null;
      programs.wezterm.package = null;
      programs.fuzzel.package = null;
      wayland.windowManager.sway.package = null;
      services.mako.package = null;
      programs.waybar.package = pkgs.emptyDirectory;
    }
  ];
}

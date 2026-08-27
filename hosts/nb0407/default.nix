{
  pkgs,
  lib,
  ...
}:
{
  targets.genericLinux.enable = true;

  # Get criteria with swaymsg -t get_outputs
  xdg.configFile."kanshi/config".text = ''
    output eDP-1 {
      alias $INTERNAL
      scale 2.0
    }
    output "Dell Inc. DELL S2722QC 5Q7VLD3" {
      alias $HOME
      scale 2.0
    }

    profile laptop_only {
      output $INTERNAL enable
    }
    profile home_office {
      output $INTERNAL disable
      output $HOME enable
    }
    profile {
      output $INTERNAL disable
      output DP-5 'Lenovo Group Limited P27QD-40 VNACWFPB' position 0,0 scale 1.3333 mode 2560x1440@119.998 enable
      output DP-7 'Lenovo Group Limited P27QD-40 VNACWFPT' position 1920,0 scale 1.3333 mode 2560x1440@119.998 enable
    }
    profile {
      output $INTERNAL disable
      output DP-6 'Lenovo Group Limited P27QD-40 VNACWFPB' position 0,0 scale 1.3333 mode 2560x1440@119.998 enable
      output DP-9 'Lenovo Group Limited P27QD-40 VNACWFPT' position 1920,0 scale 1.3333 mode 2560x1440@119.998 enable
    }
    profile {
      output $INTERNAL disable
      output DP-5 position 1920,0 scale 1.3333 mode 2560x1440@119.998 enable
      output DP-7 position 0,0 scale 1.3333 mode 2560x1440@119.998 enable
    }
    profile {
      output $INTERNAL disable
      output DP-6 position 1920,0 scale 1.3333 mode 2560x1440@119.998 enable
      output DP-9 position 0,0 scale 1.3333 mode 2560x1440@119.998 enable
    }
    profile {
      output $INTERNAL disable
      output DP-1 'Lenovo Group Limited P27QD-40 VNACWFR2'scale 1.3333 mode 2560x1440@119.998 enable
    }
  '';

  wayland.windowManager.sway.config.startup = [
    { command = "blueman-applet"; }
    { command = "kanshi"; }
    { command = "/usr/libexec/polkit-mate-authentication-agent-1"; }
  ];

  imports = [
    ../../home
    ../../home/standalone.nix
    ../../home/linux-desktop.nix
    {
      home.packages = [
        (pkgs.writeShellScriptBin "vpn" ''
          function printhelp {
              echo "Usage: vpn COMMAND"
              echo ""
              echo "Valid options for COMMAND:"
              echo "up       establish a connection"
              echo "down     close the connection"
              echo "kill     kill the connection and restart strongswan"
              echo "re       kill then up"
              echo "help     this help message"
          }

          if [ "$#" -eq 0 ]; then
              printhelp
              exit 1
          fi

          function vpnup {
              sudo swanctl --initiate --child checkpoint 1>/dev/null
          }
          function vpndown {
              sudo swanctl --terminate --child checkpoint 1>/dev/null
          }
          function vpnkill {
              echo "# => Terminate the next command with 'C-c' if it hangs!"
              echo "# => Killing will commence anyway."
              sudo swanctl --terminate --child checkpoint 1>/dev/null
              sudo pkill -f vpn-dns-updown
              sudo systemctl restart strongswan
          }

          case "$1" in
          up)
              vpnup
              ;;
          down)
              vpndown
              ;;
          kill)
              vpnkill
              ;;
          re)
              vpnkill
              vpnup
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

      programs.nixvim.plugins.copilot-vim.enable = true;

      # Vivaldi from snap has wrong exec and name:
      xdg.desktopEntries."vivaldi_vivaldi-stable" = {
        name = "Vivaldi";
        genericName = "Web Browser";
        exec = "/snap/bin/vivaldi.vivaldi-stable %U";
        startupNotify = true;
        terminal = false;
        icon = "/snap/vivaldi/current/meta/gui/icon.png";
        type = "Application";
        categories = [
          "Network"
          "WebBrowser"
        ];
        mimeType = [
          "application/pdf"
          "application/rdf+xml"
          "application/rss+xml"
          "application/xhtml+xml"
          "application/xhtml_xml"
          "application/xml"
          "image/gif"
          "image/jpeg"
          "image/png"
          "image/webp"
          "text/html"
          "text/xml"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
        ];
      };
      xdg.mimeApps = {
        defaultApplications = {
          "text/html" = [ "vivaldi_vivaldi-stable.desktop" ];
          "x-scheme-handler/about" = [ "vivaldi_vivaldi-stable.desktop" ];
          "x-scheme-handler/http" = [ "vivaldi_vivaldi-stable.desktop" ];
          "x-scheme-handler/https" = [ "vivaldi_vivaldi-stable.desktop" ];
          "x-scheme-handler/unknown" = [ "vivaldi_vivaldi-stable.desktop" ];
        };
      };

      # Disable the packages that are managed by "parent ubuntu":
      programs.fuzzel = {
        package = null;
        # Too wide otherwise with ubuntu:
        settings.main.width = lib.mkForce "60";
      };
      programs.ghostty.package = null;
      programs.ghostty.systemd.enable = false;
      programs.swaylock.package = null;
      programs.wezterm.package = null;
      wayland.windowManager.sway.package = null;
      services.mako.package = null;
      programs.waybar.package = pkgs.emptyDirectory;
    }
  ];
}

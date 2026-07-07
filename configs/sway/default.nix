{
  lib,
  pkgs,
  inputs,
  ...
}:
{
  home.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    NIXOS_OZONE_WL = "1";
  };
  systemd.user.sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";

    NIXOS_OZONE_WL = "1";
  };

  xdg.configFile."sway/blueberry_peach_dark" = {
    source = "${inputs.blueberry-peach}/ports/sway/blueberry_peach_dark";
  };
  xdg.configFile."sway/blueberry_peach_light" = {
    source = "${inputs.blueberry-peach}/ports/sway/blueberry_peach_light";
  };

  wayland.windowManager.sway = {
    enable = true;

    systemd.enable = true;
    wrapperFeatures.gtk = true; # Include fixes for GTK apps under Sway

    config = import ./config.nix {
      inherit lib pkgs;
    };

    # Necessary for include to work:
    checkConfig = false;
    extraConfigEarly = # sway
      ''
        include ./blueberry_peach_dark
      '';

    extraConfig = # sway
      ''
        for_window [shell="xwayland"] title_format "[XWayland] %title"

        # Make sure tmux-server, etc. exit when sway exits so that new sessions
        # start with new servers that attach to the correct dbus, etc.
        exec --no-startup-id swaymsg -mt subscribe '[]' || true && ${pkgs.tmux}/bin/tmux kill-server
        exec --no-startup-id swaymsg -mt subscribe '[]' || true && pkill ghostty
        exec --no-startup-id swaymsg -mt subscribe '[]' || true && systemctl --user stop mako.service
      '';

    extraSessionCommands = # bash
      ''
        export XDG_SESSION_DESKTOP=sway
        export XDG_CURRENT_DESKTOP=sway
        export XDG_SESSION_TYPE=wayland
        export MOZ_ENABLE_WAYLAND=1
        export QT_QPA_PLATFORM=wayland
        export SDL_VIDEODRIVER=wayland
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
  };
}

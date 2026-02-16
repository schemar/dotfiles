{ lib, pkgs }:
{
  focus = {
    followMouse = false;
  };

  fonts = {
    names = [
      "Monaspace Neon"
      "OpenMoji Color"
      "Symbols Nerd Font Mono"
    ];
    size = 13.0;
  };

  modifier = "Mod4"; # Use the Super/Windows key as the Mod key
  terminal = "${pkgs.ghostty}/bin/ghostty";

  gaps = {
    outer = 6;
    inner = 6;
  };

  window = {
    titlebar = false;
    border = 1;
  };

  defaultWorkspace = "workspace number 1";

  colors = {
    background = "#191724";
    focused = {
      border = "#A19DD4";
      background = "#191724";
      text = "#A2A2A9";
      indicator = "#C394C2";
      childBorder = "#A19DD4";
    };
    focusedInactive = {
      border = "#84848C";
      background = "#0B0A0F";
      text = "#878794";
      indicator = "#37363E";
      childBorder = "#37363E";
    };
    unfocused = {
      border = "#0B0A0F";
      background = "#0B0A0F";
      text = "#878794";
      indicator = "#37363E";
      childBorder = "#37363E";
    };
    placeholder = {
      background = "#191724";
      text = "#A2A2A9";

      # unused
      border = "#000000";
      indicator = "#000000";
      childBorder = "#000000";
    };
  };

  keybindings = lib.mkOptionDefault {
    "Mod4+Shift+a" = "focus child";

    "Mod4+n" = "exec ${pkgs.mako}/bin/makoctl dismiss";

    "Mod4+Shift+e" = "exec ~/.local/bin/powermenu.sh";
    "Mod4+Shift+s" = "exec ~/.local/bin/settingsmenu.sh";

    "Mod4+d" = "exec ${pkgs.fuzzel}/bin/fuzzel";
    "Mod4+Shift+d" = "exec ${pkgs.bemoji}/bin/bemoji --type";

    # Commands provided by avizo.service
    "XF86AudioRaiseVolume" = "exec volumectl -u up";
    "XF86AudioLowerVolume" = "exec volumectl -u down";
    "XF86AudioMute" = "exec volumectl toggle-mute";
    "XF86AudioMicMute" = "exec volumectl -m toggle-mute";
    "XF86MonBrightnessUp" = "exec lightctl up";
    "XF86MonBrightnessDown" = "exec lightctl down";

    "XF86AudioPlay" = "exec playerctl play-pause";
    "XF86AudioPause" = "exec playerctl play-pause";
    "XF86AudioNext" = "exec playerctl next";
    "XF86AudioPrev" = "exec playerctl previous";
    "XF86AudioStop" = "exec playerctl stop";
    "XF86Search" = "exec fuzzel";
  };

}

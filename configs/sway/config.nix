{
  lib,
  pkgs,
}:
let
  terminal = "ghostty";
in
{
  focus = {
    followMouse = false;
  };

  fonts = {
    names = [
      "MonoLisaText"
      "Symbols Nerd Font Mono"
      "Noto Color Emoji"
    ];
    size = 11.0;
  };

  # For any defaults left from the sway/i3 modules in home manager:
  modifier = "Mod4"; # Use the Super/Windows key as the Mod key
  terminal = terminal;

  gaps = {
    top = 0;
    right = 6;
    bottom = 6;
    left = 6;
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

  bars = [
    {
      command = "waybar";
    }
  ];

  startup = [
    {
      command = "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_DATA_DIRS";
    }

    {
      command = ''
        swayidle -w \
          timeout 300 'swaylock -fF' \
          timeout 600 'swaymsg "output * dpms off"' \
          resume 'swaymsg "output * dpms on"' \
          before-sleep 'swaylock -fF'
      '';
    }

    { command = "nm-applet"; }
    { command = "systemctl --user start mako"; }
    { command = "avizo-service"; }

    {
      command = "~/.local/bin/darkmode.sh";
    }
  ];

  seat = {
    "*" = {
      hide_cursor = "when-typing enable";
    };
  };

  input = {
    "type:keyboard" = {
      # Map capslock to escape;
      # Allows Umlauts and  with right alt as compose key;
      "xkb_options" = "caps:escape,compose:ralt";

      "repeat_delay" = "200";
      "repeat_rate" = "50";
    };
  };

  keybindings = lib.mkOptionDefault {
    "Mod4+Shift+a" = "focus child";

    "Mod4+n" = "exec makoctl dismiss";

    "Mod4+Shift+e" = "exec ~/.local/bin/powermenu.sh";
    "Mod4+Shift+s" = "exec ~/.local/bin/settingsmenu.sh";

    "Mod4+d" = "exec fuzzel";
    "Mod4+Shift+d" = "exec BEMOJI_PICKER_CMD='fuzzel -d' ${pkgs.bemoji}/bin/bemoji --noline";

    # `grim -g "$(slurp)"` to capture the given coordinates
    # `grim -` (- as file) to send the result to stdout instead of a file
    # `swappy -f -` to read the file from stdin (-)
    "Mod4+p" = "exec grim -g \"$(slurp)\" - | swappy -f -";
    "Mod4+Shift+p" = "exec grim - | swappy -f -";

    # Commands provided by avizo.service
    "XF86AudioRaiseVolume" = "exec ${pkgs.avizo}/bin/volumectl -u up";
    "XF86AudioLowerVolume" = "exec ${pkgs.avizo}/bin/volumectl -u down";
    "XF86AudioMute" = "exec ${pkgs.avizo}/bin/volumectl toggle-mute";
    "XF86AudioMicMute" = "exec ${pkgs.avizo}/bin/volumectl -m toggle-mute";
    "XF86MonBrightnessUp" = "exec ${pkgs.avizo}/bin/lightctl up";
    "XF86MonBrightnessDown" = "exec ${pkgs.avizo}/bin/lightctl down";

    "XF86AudioPlay" = "exec playerctl play-pause";
    "XF86AudioPause" = "exec playerctl play-pause";
    "XF86AudioNext" = "exec playerctl next";
    "XF86AudioPrev" = "exec playerctl previous";
    "XF86AudioStop" = "exec playerctl stop";

    "XF86Search" = "exec fuzzel";

    # More or less default:
    "Mod4+1" = "workspace number 1";
    "Mod4+0" = "workspace number 10";
    "Mod4+2" = "workspace number 2";
    "Mod4+3" = "workspace number 3";
    "Mod4+4" = "workspace number 4";
    "Mod4+5" = "workspace number 5";
    "Mod4+6" = "workspace number 6";
    "Mod4+7" = "workspace number 7";
    "Mod4+8" = "workspace number 8";
    "Mod4+9" = "workspace number 9";
    "Mod4+Down" = "focus down";
    "Mod4+Left" = "focus left";
    "Mod4+Return" = "exec ${terminal}";
    "Mod4+Right" = "focus right";
    "Mod4+Shift+0" = "move container to workspace number 10";
    "Mod4+Shift+1" = "move container to workspace number 1";
    "Mod4+Shift+2" = "move container to workspace number 2";
    "Mod4+Shift+3" = "move container to workspace number 3";
    "Mod4+Shift+4" = "move container to workspace number 4";
    "Mod4+Shift+5" = "move container to workspace number 5";
    "Mod4+Shift+6" = "move container to workspace number 6";
    "Mod4+Shift+7" = "move container to workspace number 7";
    "Mod4+Shift+8" = "move container to workspace number 8";
    "Mod4+Shift+9" = "move container to workspace number 9";
    "Mod4+Shift+Down" = "move down";
    "Mod4+Shift+Left" = "move left";
    "Mod4+Shift+Right" = "move right";
    "Mod4+Shift+Up" = "move up";
    "Mod4+Shift+b" = "exec swaylock";
    "Mod4+Shift+c" = "reload";
    "Mod4+Shift+h" = "move left";
    "Mod4+Shift+j" = "move down";
    "Mod4+Shift+k" = "move up";
    "Mod4+Shift+l" = "move right";
    "Mod4+Shift+minus" = "move scratchpad";
    "Mod4+Shift+q" = "kill";
    "Mod4+Shift+space" = "floating toggle";
    "Mod4+Up" = "focus up";
    "Mod4+a" = "focus parent";
    "Mod4+b" = "splith";
    "Mod4+e" = "layout toggle split";
    "Mod4+f" = "fullscreen toggle";
    "Mod4+h" = "focus left";
    "Mod4+j" = "focus down";
    "Mod4+k" = "focus up";
    "Mod4+l" = "focus right";
    "Mod4+minus" = "scratchpad show";
    "Mod4+r" = "mode resize";
    "Mod4+s" = "layout stacking";
    "Mod4+space" = "focus mode_toggle";
    "Mod4+v" = "splitv";
    "Mod4+w" = "layout tabbed";
  };
}

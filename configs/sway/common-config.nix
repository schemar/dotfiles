{
  lib,
  pkgs,
  commands,
}:
let
  terminal = "${pkgs.ghostty}/bin/ghostty";
in
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

  # For any defaults left from the sway/i3 modules in home manager:
  modifier = "Mod4"; # Use the Super/Windows key as the Mod key
  terminal = terminal;

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

    "Mod4+n" = commands.notificationDismissCommand;

    "Mod4+Shift+e" = commands.powerCommand;
    "Mod4+Shift+s" = commands.settingsCommand;

    "Mod4+d" = commands.applicationCommand;
    "Mod4+Shift+d" = commands.emojiCommand;

    # Commands provided by avizo.service
    "XF86AudioRaiseVolume" = commands.audioUpCommand;
    "XF86AudioLowerVolume" = commands.audioDownCommand;
    "XF86AudioMute" = commands.audioMuteCommand;
    "XF86AudioMicMute" = commands.audioMicMuteCommand;
    "XF86MonBrightnessUp" = commands.brightnessUpCommand;
    "XF86MonBrightnessDown" = commands.brightnessDownCommand;

    "XF86AudioPlay" = "exec playerctl play-pause";
    "XF86AudioPause" = "exec playerctl play-pause";
    "XF86AudioNext" = "exec playerctl next";
    "XF86AudioPrev" = "exec playerctl previous";
    "XF86AudioStop" = "exec playerctl stop";

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
    "XF86Search" = "exec fuzzel";
  };

}

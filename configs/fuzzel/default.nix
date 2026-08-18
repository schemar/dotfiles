{ inputs, ... }:
{
  xdg.configFile = {
    "fuzzel/blueberry_peach_light.ini".source =
      "${inputs.blueberry-peach}/ports/fuzzel/blueberry_peach_light.ini";
    "fuzzel/blueberry_peach_dark.ini".source =
      "${inputs.blueberry-peach}/ports/fuzzel/blueberry_peach_dark.ini";
  };

  programs.fuzzel = {
    enable = true;

    settings = {
      main = {
        # Symlinks to blueberry_peach_dark or _light defined above.
        # Symlink is updated by darkmode.sh and lightmode.sh
        include = "~/.config/fuzzel/blueberry_peach.ini";

        width = "80";
        horizontal-pad = "12";
        vertical-pad = "8";
        inner-pad = "12";
        font = "MonoLisaText:size=16,Symbols Nerd Font Mono:size=16,Noto Color Emoji:size=16";
        use-bold = false;
        terminal = "ghostty";
      };

      border.width = "1";
      border.radius = "0";
    };
  };
}

{ pkgs, ... }:
{
  programs.fuzzel = {
    enable = true;

    settings = {
      main = {
        width = "80";
        "horizontal-pad" = "12";
        "vertical-pad" = "8";
        "inner-pad" = "12";
        font = "Monaspace Neon:size=11,OpenMoji Color:size=11,Symbols Nerd Font Mono:size=11";
        terminal = "${pkgs.ghostty}/bin/ghostty";
      };

      colors.background = "0B0A0FFF";
      colors.text = "A2A2A9FF";
      colors.prompt = "A19DD4FF";
      colors.placeholder = "878794FF";
      colors.input = "A2A2A9FF";
      colors.match = "A19DD4FF";
      colors.selection = "37363EFF";
      colors.selection-text = "A2A2A9FF";
      colors.selection-match = "A19DD4FF";
      colors.counter = "A19DD4FF";
      colors.border = "A19DD4FF";

      border.width = "1";
      border.radius = "0";
    };
  };
}

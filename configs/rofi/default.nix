{ pkgs, ... }:
{
  programs.rofi = {
    enable = true;

    font = "Monaspace Neon 11";
    terminal = "${pkgs.ghostty}/bin/ghostty";

    extraConfig = {
      dpi = 192;
      case-smart = true;
    };

    theme = "blueberrypeach-dark";
  };

  xdg.configFile."rofi/blueberrypeach-dark.rasi" = {
    source = ./blueberrypeach-dark.rasi;
  };
}

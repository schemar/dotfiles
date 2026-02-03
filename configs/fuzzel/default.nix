{ pkgs, ... }:
{
  programs.fuzzel = {
    enable = true;

    settings = {
      main = {
        font = "Monaspace Neon:size=13,OpenMoji Color:size=13,Symbols Nerd Font Mono:size=13";
        terminal = "${pkgs.ghostty}/bin/ghostty";
      };
    };
  };
}

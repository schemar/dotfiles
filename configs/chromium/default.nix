{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    package = pkgs.vivaldi;
  };

  home.packages = [
    pkgs.vivaldi-ffmpeg-codecs
  ];
}

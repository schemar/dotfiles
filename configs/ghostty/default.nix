{ inputs, ... }:
{
  xdg.configFile."ghostty/config" = {
    source = ./config;
  };
  xdg.configFile."ghostty/themes/blueberry_peach_dark" = {
    source = ./themes/blueberry_peach_dark;
  };
  xdg.configFile."ghostty/themes/blueberry_peach_light" = {
    source = ./themes/blueberry_peach_light;
  };
  # TODO: Move these to blueberry peach package:
  # xdg.configFile."ghostty/themes/blueberry_peach_dark" = {
  #   source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_dark";
  # };
  # xdg.configFile."ghostty/themes/blueberry_peach_light" = {
  #   source = "${inputs.blueberry-peach}/ports/ghostty/blueberry_peach_light";
  # };
}

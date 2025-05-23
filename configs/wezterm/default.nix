{ inputs, ... }:
{
  xdg.configFile."wezterm/wezterm.lua" = {
    source = ./wezterm.lua;
  };
  xdg.configFile."wezterm/colors/blueberry_peach_dark.toml" = {
    source = "${inputs.blueberry-peach}/ports/wezterm/blueberry_peach_dark.toml";
  };
  xdg.configFile."wezterm/colors/blueberry_peach_light.toml" = {
    source = "${inputs.blueberry-peach}/ports/wezterm/blueberry_peach_light.toml";
  };
}

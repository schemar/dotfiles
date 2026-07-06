{ inputs, ... }:
{
  xdg.configFile."yazi/theme.toml" = {
    source = "${inputs.blueberry-peach}/ports/yazi/theme.toml";
  };
  xdg.configFile."yazi/flavors/blueberry-peach-dark.yazi" = {
    source = "${inputs.blueberry-peach}/ports/yazi/blueberry-peach-dark.yazi";
  };
  xdg.configFile."yazi/flavors/blueberry-peach-light.yazi" = {
    source = "${inputs.blueberry-peach}/ports/yazi/blueberry-peach-light.yazi";
  };

  programs.yazi = {
    enable = true;
    enableZshIntegration = true;

    shellWrapperName = "yy";
  };
}

{ inputs, ... }:
{
  programs.lazygit = {
    enable = true;
    settings = {
      customCommands = [
        {
          key = "<c-/>";
          context = "global";
          command = "~/.config/lazygit/graphite.sh";
          refresh = true;
          output = "terminal";
        }
      ];
    };
  };

  xdg.configFile."lazygit/blueberry_peach_dark.yml" = {
    source = "${inputs.blueberry-peach}/ports/lazygit/blueberry_peach_dark.yml";
  };
  xdg.configFile."lazygit/blueberry_peach_light.yml" = {
    source = "${inputs.blueberry-peach}/ports/lazygit/blueberry_peach_light.yml";
  };

  xdg.configFile."lazygit/graphite.sh" = {
    source = ./graphite.sh;
  };
}

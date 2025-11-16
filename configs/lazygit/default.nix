{ inputs, ... }:
{
  programs.lazygit = {
    enable = true;
    settings = {
      gui = {
        scrollHeight = 5;
        scrollOffMargin = 5;

        showNumstatInFilesView = true;

        border = "single";
        filterMode = "fuzzy";
      };
      git = {
        paging = {
          # Same themeing as in git delta config. Theme comes from bat.
          pager = "delta --paging=never --line-numbers --features syntax-theme-`$HOME/.config/current_theme`";
          colorArg = "always";
        };
      };
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

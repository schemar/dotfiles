{ ... }:
{
  programs.lazygit = {
    enable = true;
    settings = {
      gui = {
        theme = {
          lightTheme = false;
          activeBorderColor = [
            "#A19DD4"
            "bold"
          ];
          inactiveBorderColor = [ "#878794" ];
          optionsTextColor = [ "#7AA8CE" ];
          selectedLineBgColor = [ "#0B0A0F" ];
          cherryPickedCommitBgColor = [ "#37363E" ];
          cherryPickedCommitFgColor = [ "#A19DD4" ];
          unstagedChangesColor = [ "#DF8BA0" ];
          defaultFgColor = [ "#A2A2A9" ];
          searchingActiveBorderColor = [ "#C7B96F" ];
          authorColors = {
            "*" = "#A19DD4";
          };
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

  xdg.configFile."lazygit/graphite.sh" = {
    source = ./graphite.sh;
  };
}

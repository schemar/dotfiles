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
          command = "gt {{.Form.Command}}";
          output = "terminal";
          refresh = true;
          prompts = [
            {
              type = "menu";
              name = "Command";
              title = "What do you want to do?";
              key = "Command";
              options = [
                {
                  value = "up";
                  name = "Up";
                  description = "Move up one branch in the stack";
                }
                {
                  value = "down";
                  name = "Down";
                  description = "Move down one branch in the stack";
                }
                {
                  value = "sync";
                  name = "Sync";
                  description = "Sync with remote";
                }
                {
                  value = "create";
                  name = "Create";
                  description = "Create a new branch on top of the current location in the stack";
                }
                {
                  value = "modify";
                  name = "Modify";
                  description = "Amend the current branch";
                }
                {
                  value = "submit";
                  name = "Submit";
                  description = "Submit the current branch for review";
                }
              ];
            }
          ];
        }
      ];
    };
  };
}

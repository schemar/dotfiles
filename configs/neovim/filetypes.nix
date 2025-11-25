{ ... }:
{
  programs.nixvim.files = {
    "ftplugin/gdscript.lua" = {
      opts = {
        expandtab = false;
      };
    };
    "ftplugin/typescript.lua" = {
      keymaps = [
        {
          action = "<cmd>VtsExec source_actions<cr>";
          key = "<leader>la";
          options = {
            noremap = true;
            silent = true;
            desc = "Code actions";
          };
        }
      ];
    };
  };
}

{ inputs, ... }:
{
  imports = [
    inputs.nixvim.homeModules.nixvim
    ./opts.nix
    ./lua.nix
    ./theme.nix
    ./filetypes.nix
    ./languages
    ./plugins
  ];

  xdg.configFile."nvim/lua/blueberry_peach/light.lua" = {
    source = "${inputs.blueberry-peach}/ports/neovim/blueberry_peach_light.lua";
  };
  xdg.configFile."nvim/lua/blueberry_peach/dark.lua" = {
    source = "${inputs.blueberry-peach}/ports/neovim/blueberry_peach_dark.lua";
  };

  programs.nixvim =
    let
      border = "single";
    in
    {
      enable = true;
      defaultEditor = true;
      vimdiffAlias = true;

      nixpkgs.config.allowUnfree = true;

      globals = {
        mapleader = " ";
        maplocalleader = "\\";
      };

      autoCmd = [
        {
          # Start terminal in insert mode:
          command = "startinsert";
          pattern = "*";
          event = "TermOpen";
        }
      ];

      diagnostic.settings = {
        virtual_lines = false;
        float = {
          border = border;
        };
        signs = {
          text.__raw = # lua
            ''
              {
                [vim.diagnostic.severity.ERROR] = "󰅚";
                [vim.diagnostic.severity.WARN] = "󰀪";
                [vim.diagnostic.severity.INFO] = "󰋽";
                [vim.diagnostic.severity.HINT] = "󰌶";
              }
            '';
          numhl.__raw = # lua
            ''
              {
                [vim.diagnostic.severity.ERROR] = "DiagnosticSignError";
                [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn";
                [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo";
                [vim.diagnostic.severity.HINT] = "DiagnosticSignHint";
              }
            '';
        };
      };
    };
}

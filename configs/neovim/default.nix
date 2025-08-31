{ inputs, ... }:
{
  imports = [
    inputs.nixvim.homeModules.nixvim
    ./lua.nix
    ./theme.nix
    ./lsp.nix
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

      opts = {
        # [[ Folding ]]
        foldcolumn = "0"; # '0' to hide or '1' to show
        foldlevel = 99; # Using ufo provider need a large value, feel free to decrease the value
        foldlevelstart = 99;
        foldenable = true;

        # [[ Misc ]]
        timeout = true;
        timeoutlen = 300; # num: Timeout, e.g. for which-key
        updatetime = 1000; # num: Timeout for "cursor hold" event
        clipboard = "unnamedplus"; # str: Clipboard integration with macOS
        splitkeep = "cursor"; # The default "screen" moves the cursor wrongly, which leads to problems, e.g. with Trouble

        # [[ Context ]]
        colorcolumn = "80"; # str: Show col for max line length
        number = true; # bool: Show line numbers
        scrolloff = 5; # int: Min num lines of context
        signcolumn = "yes"; # str: Show the sign column

        # [[ Filetypes ]]
        encoding = "utf8"; # str: String encoding to use
        fileencoding = "utf8"; # str: File encoding to use

        # [[ Search ]]
        ignorecase = true; # bool: Ignore case in search patterns
        smartcase = true; # bool: Override ignorecase if search contains capitals
        incsearch = true; # bool: Use incremental search

        # [[ Whitespace ]]
        expandtab = true; # bool: Use spaces instead of tabs
        shiftwidth = 2; # num: Size of an indent
        softtabstop = 2; # num: Number of spaces tabs count for in insert mode
        tabstop = 2; # num: Number of spaces tabs count for
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

      files = {
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
    };
}

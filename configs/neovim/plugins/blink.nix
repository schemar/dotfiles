{ ... }:
{
  programs.nixvim.plugins.blink-cmp =
    let
      border = "single";
    in
    {
      enable = true;
      settings.__raw = # lua
        ''
          {
            -- For details, see:
            -- 1. Recipes: https://cmp.saghen.dev/recipes.html
            -- 2. Sources: https://cmp.saghen.dev/configuration/sources.html

            -- 'default' for mappings similar to built-in completion
            -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
            -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
            -- See the full "keymap" documentation for information on defining your own keymap.
            --
            -- Most importantly, use <C-y> to accept the completion and <C-e> to cancel it.
            keymap = {
              preset = "default",

              ["<C-k>"] = { "select_prev", "fallback" },
              ["<C-j>"] = { "select_next", "fallback" },
              ["<CR>"] = { "accept", "fallback" },
            },

            appearance = {
              -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
              -- Adjusts spacing to ensure icons are aligned
              nerd_font_variant = "mono",
            },

            completion = {
              menu = {
                border = "${border}",
              },
              documentation = {
                auto_show = true,
                auto_show_delay_ms = 0,
                window = {
                  border = "${border}",
                },
              },
              list = {
                selection = {
                  -- When `true`, will automatically select the first item in the completion list
                  preselect = false,
                  -- preselect = function(ctx) return vim.bo.filetype ~= 'markdown' end,

                  -- When `true`, inserts the completion item automatically when selecting it
                  -- You may want to bind a key to the `cancel` command (default <C-e>) when using this option,
                  -- which will both undo the selection and hide the completion menu
                  auto_insert = true,
                  -- auto_insert = function(ctx) return vim.bo.filetype ~= 'markdown' end
                },
              },
            },

            signature = {
              enabled = true,
              window = {
                border = "${border}",
              },
            },

            -- Default list of enabled providers defined so that you can extend it
            -- elsewhere in your config, without redefining it, due to `opts_extend`
            sources = {
              default = { "lsp", "path", "snippets", "buffer" },
            },

            cmdline = {
              enabled = true,
              keymap = {
                preset = "cmdline",
                ["<C-k>"] = { "select_prev", "fallback" },
                ["<C-j>"] = { "select_next", "fallback" },
              },
              completion = {
                list = {
                  selection = {
                    preselect = false,
                  },
                },
                menu = {
                  auto_show = true,
                },
              },
            },
          }
        '';
    };
}

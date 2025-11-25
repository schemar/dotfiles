{ ... }:
{
  programs.nixvim.plugins.telescope = {
    enable = true;

    extensions.fzf-native.enable = true;
    extensions.ui-select.enable = true;
    enabledExtensions = [ "smart_open" ];

    luaConfig.pre = # lua
      ''
        local open_with_trouble = require("trouble.sources.telescope").open

        local telescope = require("telescope")
        local actions = require("telescope.actions")
      '';
    settings.__raw = # lua
      ''
        {
          defaults = {
            mappings = {
              i = {
                ["<c-e>"] = actions.close,
                ["<c-y>"] = actions.select_default,
                ["<c-j>"] = actions.move_selection_next,
                ["<c-k>"] = actions.move_selection_previous,
                ["<c-t>"] = open_with_trouble,
              },
              n = {
                ["<c-e>"] = actions.close,
                ["<c-t>"] = open_with_trouble,
                ["o"] = actions.select_default,
                ["q"] = actions.close,
              },
            },
            -- Themeing
            sorting_strategy = "ascending", -- List from the top down
            layout_strategy = "horizontal",
            layout_config = {
              prompt_position = "top",
            },
            borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" }
          },
          pickers = {
            live_grep = {
              mappings = {
                -- Press `<c-f>` to restart search within the current results
                i = { ["<c-f>"] = actions.to_fuzzy_refine },
              },
            },
          },
          extensions = {
            fzf = {
              fuzzy = true, -- false will only do exact matching
              override_generic_sorter = true, -- override the generic sorter
              override_file_sorter = true, -- override the file sorter
              case_mode = "smart_case", -- or "ignore_case" or "respect_case"
              -- the default case_mode is "smart_case"
            },
            smart_open = {
              match_algorithm = "fzf",
              disable_devicons = false,
              mappings = {
                i = {
                  ["<c-w>"] = function()
                    vim.api.nvim_input("<c-s-w>")
                  end,
                },
              },
            },
            ["ui-select"] = {
              require("telescope.themes").get_dropdown({}),
            },
          },
        }
      '';
  };
}

{ ... }:
{
  programs.nixvim.plugins.aerial = {
    enable = true;

    settings.__raw = # lua
      ''
        {
          attach_mode = "global",
          backends = { "treesitter", "lsp", "markdown", "asciidoc", "man" },
          close_automatic_events = { "switch_buffer" },
          icons = icons,
          show_guides = true,
          guides = {
            mid_item = "├╴",
            last_item = "└╴",
            nested_top = "│ ",
            whitespace = "  ",
          },
          filter_kind = false,
          -- Keymaps in aerial window. Can be any value that `vim.keymap.set` accepts OR a table of keymap
          -- options with a `callback` (e.g. { callback = function() ... end, desc = "", nowait = true })
          -- Additionally, if it is a string that matches "actions.<name>",
          -- it will use the mapping at require("aerial.actions").<name>
          -- Set to `false` to remove a keymap
          keymaps = {
            ["<TAB>"] = "actions.scroll",
            ["o"] = {
              callback = function()
                -- Temporarily set close_on_select to true so that aerial
                -- closes when we jump to a definition.
                require("aerial.config").close_on_select = true
                require("aerial").select()
                require("aerial.config").close_on_select = false
              end,
              desc = "Jump and quit",
              nowait = true,
            },
          },
          nav = {
            keymaps = {
              ["<CR>"] = "actions.jump",
              ["o"] = "actions.jump",
              ["<2-LeftMouse>"] = "actions.jump",
              ["<C-s>"] = "actions.jump_vsplit",
              ["<C-v>"] = "actions.jump_split",
              ["h"] = "actions.left",
              ["l"] = "actions.right",
              ["q"] = "actions.close",
              ["<C-c>"] = "actions.close",
              ["<C-e>"] = "actions.close",
            },
          },
        }
      '';
  };
}

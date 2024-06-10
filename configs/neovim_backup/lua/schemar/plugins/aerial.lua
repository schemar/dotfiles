return {
  "stevearc/aerial.nvim",
  event = { "VeryLazy" },
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "nvim-tree/nvim-web-devicons",
  },
  opts = {
    icons = {
      Collapsed = require("schemar.icons").ui.ChevronShortRight,
    },
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
  },
}

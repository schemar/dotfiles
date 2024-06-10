return {
  "sindrets/diffview.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  lazy = false,
  opts = {
    keymaps = {
      disable_defaults = false,
      view = {
        { "n", "q", ":DiffviewClose<CR>", { desc = "Close DiffView" } },
      },
      file_panel = {
        { "n", "q", ":DiffviewClose<CR>", { desc = "Close DiffView" } },
      },
      file_history_panel = {
        { "n", "q", ":DiffviewClose<CR>", { desc = "Close DiffView" } },
      },
    },
  },
}

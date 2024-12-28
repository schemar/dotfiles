return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  keys = {
    {
      "<leader>?",
      function()
        require("which-key").show({ global = false })
      end,
      desc = "Buffer Local Keymaps (which-key)",
    },
  },
  config = function()
    local wk = require("which-key")
    wk.setup({
      preset = "helix",
      icons = {
        mappings = false,
      },
    })
    wk.add({
      { "<c-w>", group = "Window" },
      { "<c-w>s", "<cmd>vsplit<cr>", desc = "Split window" },
      { "<m-s>", "<cmd>vsplit<cr>", desc = "Split window" },
      { "<c-w>v", "<cmd>split<cr>", desc = "Split window verticallly" },
      { "<leader>", group = "Leader" },
      { "<leader>g", desc = "Git" },
      { "<leader>l", group = "Code" },
      { "<leader>la", vim.lsp.buf.code_action, desc = "Code actions" },
      { "<leader>le", vim.diagnostic.open_float, desc = "Floating diagnostic" },
      { "<leader>lr", vim.lsp.buf.rename, desc = "Rename" },
      { "<leader>t", desc = "Todos" },
    })
  end,
}

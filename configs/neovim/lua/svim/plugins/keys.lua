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
      {
        "<c-w>s",
        "<cmd>vsplit<cr>",
        desc = "Split window",
      },
      {
        "<m-s>",
        "<cmd>vsplit<cr>",
        desc = "Split window",
      },
      {
        "<c-w>v",
        "<cmd>split<cr>",
        desc = "Split window verticallly",
      },
      { "<leader>", group = "Leader" },
      { "<leader>g", desc = "Git" },
      { "<leader>gt", group = "Graphite" },
      {
        "<leader>gtl",
        "<cmd>!gt ls<cr>",
        desc = "List stacks",
      },
      {
        "<leader>gtc",
        "<cmd>terminal gt create<cr>",
        desc = "Create interactive",
      },
      {
        "<leader>gts",
        "<cmd>!gt sync --all --no-interactive --quiet<cr><cmd>edit<cr>",
        desc = "Sync",
      },
      {
        "<leader>gtp",
        "<cmd>terminal gt submit --publish --no-edit<cr>",
        desc = "Submit (publish)",
      },
      {
        "<leader>gtP",
        "<cmd>terminal gt submit --draft --no-edit<cr>",
        desc = "Submit (draft)",
      },
      {
        "<leader>gtS",
        "<cmd>terminal gt sync --all<cr>",
        desc = "Sync interactive",
      },
      {
        "<leader>gtm",
        "<cmd>!gt modify --no-interactive --quiet<cr><cmd>edit<cr>",
        desc = "Modify",
      },
      {
        "<leader>gtM",
        "<cmd>terminal gt modify",
        desc = "Modify interactive",
      },
      {
        "<leader>gtu",
        "<cmd>!gt up --no-interactive --quiet<cr><cmd>edit<cr>",
        desc = "Up",
      },
      {
        "<leader>gtd",
        "<cmd>!gt down --no-interactive --quiet<cr><cmd>edit<cr>",
        desc = "Down",
      },
      { "<leader>l", group = "Code" },
      {
        "<leader>la",
        vim.lsp.buf.code_action,
        desc = "Code actions",
      },
      {
        "<leader>le",
        vim.diagnostic.open_float,
        desc = "Floating diagnostic",
      },
      { "<leader>lr", vim.lsp.buf.rename, desc = "Rename" },
      { "<leader>t", desc = "Todos" },
    })
  end,
}

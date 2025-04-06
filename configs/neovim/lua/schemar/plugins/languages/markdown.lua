return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      markdown = {
        order = { "null-ls" },
        sync = true,
      },
    },
  },
  {
    "MeanderingProgrammer/render-markdown.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
    opts = {},
  },
}

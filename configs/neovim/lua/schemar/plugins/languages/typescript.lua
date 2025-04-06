return {
  "lukas-reineke/lsp-format.nvim",
  -- Extends plugins/format.lua
  opts = {
    typescript = {
      order = { "vtsls", "eslint", "null-ls" },
      sync = true,
    },
  },
}

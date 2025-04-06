return {
  "lukas-reineke/lsp-format.nvim",
  -- Extends plugins/format.lua
  opts = {
    scss = {
      order = { "cssls", "null-ls" },
      sync = true,
    },
  },
}

return {
  "lukas-reineke/lsp-format.nvim",
  -- Extends plugins/format.lua
  opts = {
    json = {
      order = { "jsonls", "null-ls" },
      sync = true,
    },
  },
}

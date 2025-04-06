return {
  "lukas-reineke/lsp-format.nvim",
  -- Extends plugins/format.lua
  opts = {
    vue = {
      order = { "html", "null-ls" },
      sync = true,
    },
  },
}

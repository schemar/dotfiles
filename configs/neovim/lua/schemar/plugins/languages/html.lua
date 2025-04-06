return {
  "lukas-reineke/lsp-format.nvim",
  -- Extends plugins/format.lua
  opts = {
    html = {
      order = { "html", "null-ls" },
      sync = true,
    },
  },
}

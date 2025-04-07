return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      javascript = { "eslint", "prettier" },
    },
    -- See also typescript.lua
  },
}

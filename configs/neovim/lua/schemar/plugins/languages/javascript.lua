return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      javascript = {
        order = { "eslint", "prettier" },
        sync = true,
        -- Format only with eslint and prettier.
        -- Prevents back and forth formatting.
        exclude = { "vtsls" },
      },
    },
    -- See also typescript.lua
  },
}

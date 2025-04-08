return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      vue = {
        order = { "null-ls" },
        sync = true,
        -- Format only with stylelint and prettier.
        -- Prevents back and forth formatting.
        exclude = { "html" },
      },
    },
  },
}

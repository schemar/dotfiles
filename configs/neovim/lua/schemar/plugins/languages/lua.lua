return {
  "lukas-reineke/lsp-format.nvim",
  -- Extends plugins/format.lua
  opts = {
    lua = {
      order = { "lua_ls", "null-ls" },
      sync = true,
    },
  },
}

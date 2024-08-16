return {
  "lukas-reineke/lsp-format.nvim",
  opts = {
    gdscript = { "efm" },
    html = {
      order = { "efm" },
      sync = true,
    },
    javascript = { "eslint_d", "prettier" },
    json = {
      order = { "jsonls", "efm" },
      sync = true,
    },
    lua = {
      order = { "lua_ls", "efm" },
      sync = true,
    },
    markdown = {
      order = { "efm" },
      sync = true,
    },
    scss = {
      order = { "cssls", "efm" },
      sync = true,
    },
    typescript = {
      order = { "typescript-tools", "tsserver", "efm" },
      sync = true,
    },
    vue = {
      order = { "efm" },
      sync = true,
    },
    yaml = { "yamlfix", "efm" },
  },
}

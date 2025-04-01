return {
  "lukas-reineke/lsp-format.nvim",
  opts = {
    gdscript = { "null-ls" },
    html = {
      order = { "html", "null-ls" },
      sync = true,
    },
    javascript = { "eslint_d", "prettier" },
    json = {
      order = { "jsonls", "null-ls" },
      sync = true,
    },
    lua = {
      order = { "lua_ls", "null-ls" },
      sync = true,
    },
    markdown = {
      order = { "null-ls" },
      sync = true,
    },
    scss = {
      order = { "cssls", "null-ls" },
      sync = true,
    },
    typescript = {
      order = { "vtsls", "eslint", "null-ls" },
      sync = true,
    },
    vue = {
      order = { "html", "null-ls" },
      sync = true,
    },
    yaml = { "yamlfix", "null-ls" },
  },
}

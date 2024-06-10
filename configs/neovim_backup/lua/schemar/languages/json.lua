local lsp_config = require("lspconfig")

lsp_config.jsonls.setup({
  on_attach = require("schemar.languages.shared").on_attach,
  capabilities = require("schemar.languages.shared").capabilities,
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
})
lsp_config.efm.setup({
  on_attach = require("schemar.languages.shared").on_attach,
  init_options = { documentFormatting = true },
  filetypes = {
    "json",
  },
})

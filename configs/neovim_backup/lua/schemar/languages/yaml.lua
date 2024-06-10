local lsp_config = require("lspconfig")

lsp_config.yamlls.setup({
  on_attach = require("schemar.languages.shared").on_attach,
  capabilities = require("schemar.languages.shared").capabilities,
  settings = {
    yaml = {
      schemaStore = {
        -- You must disable built-in schemaStore support if you want to use
        -- this plugin and its advanced options like `ignore`.
        enable = false,
        -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
        url = "",
      },
      schemas = require("schemastore").yaml.schemas(),
    },
  },
})

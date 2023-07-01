local lsp_config = require("lspconfig")

require("typescript").setup({
  disable_commands = false, -- prevent the plugin from creating Vim commands
  debug = false,           -- enable debug logging for commands
  go_to_source_definition = {
    fallback = true,       -- fall back to standard LSP definition on failure
  },
  server = {               -- pass options to lspconfig's setup method
    on_attach = require("schemar.languages.shared").on_attach,
    capabilities = require("schemar.languages.shared").capabilities,
  },
})
lsp_config.eslint.setup({
  on_attach = require("schemar.languages.shared").on_attach,
  capabilities = require("schemar.languages.shared").capabilities,
})

local null_ls = require("null-ls")
null_ls.register({
  null_ls.builtins.formatting.prettier,
  require("typescript.extensions.null-ls.code-actions")
})

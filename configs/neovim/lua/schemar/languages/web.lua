local lsp_config = require("lspconfig")

lsp_config.cssls.setup({
	on_attach = require("schemar.languages.shared").on_attach,
	capabilities = require("schemar.languages.shared").capabilities,
})
lsp_config.html.setup({
	on_attach = require("schemar.languages.shared").on_attach,
	capabilities = require("schemar.languages.shared").capabilities,
})
lsp_config.jsonls.setup({
	on_attach = require("schemar.languages.shared").on_attach,
	capabilities = require("schemar.languages.shared").capabilities,
})

local null_ls = require("null-ls")
null_ls.register({ null_ls.builtins.formatting.jq })

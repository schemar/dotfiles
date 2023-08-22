local lsp_config = require("lspconfig")

require("typescript-tools").setup({
	on_attach = require("schemar.languages.shared").on_attach,
})
lsp_config.eslint.setup({
	on_attach = require("schemar.languages.shared").on_attach,
	capabilities = require("schemar.languages.shared").capabilities,
})

local null_ls = require("null-ls")
null_ls.register({
	null_ls.builtins.formatting.prettier,
})

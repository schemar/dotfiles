local lsp_config = require("lspconfig")

require("typescript-tools").setup({
	on_attach = require("schemar.languages.shared").on_attach,
})
lsp_config.efm.setup({
	on_attach = require("schemar.languages.shared").on_attach,
	init_options = { documentFormatting = true },
	filetypes = {
		"javascript",
		"javascriptreact",
		"typescript",
		"typescriptreact",
		"javascript.jsx",
		"typescript.tsx",
	},
})

local lsp_config = require("lspconfig")

lsp_config.efm.setup({
	on_attach = require("schemar.languages.shared").on_attach,
	init_options = { documentFormatting = true },
	filetypes = {
		"vue",
	},
})

local lsp_config = require("lspconfig")

require("typescript-tools").setup({
	on_attach = function(client)
		-- No formatting from typescript. EFM will handle it.
		client.resolved_capabilities.document_formatting = false
	end,
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

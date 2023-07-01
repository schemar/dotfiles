return {
	capabilities = require("cmp_nvim_lsp").default_capabilities(),
	on_attach = function(client)
		require("lsp-format").on_attach(client)
	end,
}

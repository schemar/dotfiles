return {
	"lukas-reineke/lsp-format.nvim",
	opts = {
		html = {
			order = { "html", "efm" },
			sync = true,
		},
		json = {
			order = { "jsonls", "efm" },
			sync = true,
		},
		lua = {
			order = { "lua_ls", "efm" },
			sync = true,
		},
		markdown = {
			order = { "efm" },
			sync = true,
		},
		scss = {
			order = { "cssls", "efm" },
			sync = true,
		},
		typescript = {
			order = { "typescript-tools", "tsserver", "efm" },
			sync = true,
		},
	},
}

return {
	"lukas-reineke/lsp-format.nvim",
	opts = {
		typescript = {
			-- prettier and eslint override LSP
			order = { "typescript-tools", "tsserver", "efm" },
			sync = true,
		},
		lua = {
			-- StyLua overrides LSP
			order = { "lua_ls", "efm" },
			sync = true,
		},
	},
}

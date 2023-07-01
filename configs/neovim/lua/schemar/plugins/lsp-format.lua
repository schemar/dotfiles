return {
	"lukas-reineke/lsp-format.nvim",
	opts = {
		typescript = {
			-- Prettier overrides LSP and ESLint
			order = { "tsserver", "eslint", "null-ls" },
		},
		lua = {
			-- StyLua overrides LSP
			order = { "lua_ls", "null-ls" },
			sync = true,
		},
	},
}

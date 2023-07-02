return {
	"lukas-reineke/lsp-format.nvim",
	opts = {
		typescript = {
			-- Prettier overrides LSP and ESLint
			order = { "tsserver", "eslint", "null-ls" },
			sync = true,
		},
		lua = {
			-- StyLua overrides LSP
			order = { "lua_ls", "null-ls" },
			sync = true,
		},
	},
}

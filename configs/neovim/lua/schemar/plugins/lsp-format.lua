return {
	"lukas-reineke/lsp-format.nvim",
	opts = {
		lua = {
			-- StyLua overrides LSP
			order = { "lua_ls", "null-ls" },
			sync = true,
		},
	},
}

return {
	"MunifTanjim/prettier.nvim", -- Prettier for TS/JS formatting (for null-ls)
	event = { "BufReadPre", "BufNewFile" },
	opts = {
		bin = "prettierd", -- or `"prettier"`
		filetypes = {
			"css",
			"graphql",
			"html",
			"javascript",
			"javascriptreact",
			"json",
			"less",
			"markdown",
			"scss",
			"typescript",
			"typescriptreact",
			"vue",
			"yaml",
		},
	},
}

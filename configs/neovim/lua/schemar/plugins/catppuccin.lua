return {
	"catppuccin/nvim",
	name = "catppuccin",
	priority = 1000,
	opts = {
		integrations = {
			cmp = true,
			fidget = true,
			markdown = true,
			mason = true,
			native_lsp = {
				enabled = true,
				virtual_text = {
					errors = { "italic" },
					hints = { "italic" },
					warnings = { "italic" },
					information = { "italic" },
				},
				underlines = {
					errors = { "underline" },
					hints = { "underline" },
					warnings = { "underline" },
					information = { "underline" },
				},
				inlay_hints = {
					background = true,
				},
			},
			telescope = true,
			treesitter = true,
		},
	},
}

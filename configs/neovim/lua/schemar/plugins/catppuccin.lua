return {
	"catppuccin/nvim",
	name = "catppuccin",
	priority = 1000,
	opts = {
		integrations = {
			aerial = true,
			alpha = true,
			cmp = true,
			fidget = true,
			gitsigns = true,
			lsp_trouble = true,
			markdown = true,
			mason = true,
			mini = true,
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
			neogit = true,
			notify = true,
			telescope = true,
			treesitter = true,
			which_key = true,
		},
	},
}

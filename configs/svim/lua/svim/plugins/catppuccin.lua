return {
	"catppuccin/nvim",
	name = "catppuccin",
	priority = 1000,
	lazy = true,
	opts = {
		flavour = "mocha", -- latte, frappe, macchiato, mocha
		integrations = {
			aerial = true,
			gitsigns = true,
			neogit = true,
			mason = true,
			native_lsp = {
				enabled = true,
				virtual_text = {
					errors = { "italic" },
					hints = { "italic" },
					warnings = { "italic" },
					information = { "italic" },
					ok = { "italic" },
				},
				underlines = {
					errors = { "underline" },
					hints = { "underline" },
					warnings = { "underline" },
					information = { "underline" },
					ok = { "underline" },
				},
				inlay_hints = {
					background = true,
				},
			},
			ufo = true,
		},
	},
}

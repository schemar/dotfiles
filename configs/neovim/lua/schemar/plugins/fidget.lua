return {
	"j-hui/fidget.nvim",
	dependencies = { "neovim/nvim-lspconfig" },
	tag = "legacy",
	event = { "BufReadPost", "BufNewFile" },
	opts = {
		text = {
			spinner = "arc",
		},
		notification = {
			window = {
				blend = 0,
			},
		},
	},
}

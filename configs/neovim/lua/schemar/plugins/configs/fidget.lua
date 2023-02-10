return {
	"j-hui/fidget.nvim", -- Spinner to show when LSP Server starts
	event = { "BufReadPost", "BufNewFile" },
	opts = {
		text = {
			spinner = "arc",
		},
	},
}

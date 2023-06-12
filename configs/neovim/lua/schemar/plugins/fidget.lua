return {
	"j-hui/fidget.nvim", -- Spinner to show when LSP Server starts
	tag = "legacy",
	event = { "BufReadPost", "BufNewFile" },
	opts = {
		text = {
			spinner = "arc",
		},
	},
	window = {
		blend = 0,
	},
}

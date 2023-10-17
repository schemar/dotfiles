return {
	"sindrets/diffview.nvim",
	dependencies = {
		"nvim-tree/nvim-web-devicons",
	},
	cmd = {
		"DiffviewFileHistory",
		"DiffviewOpen",
		"DiffviewClose",
		"DiffviewToggleFiles",
		"DiffviewFocusFiles",
		"DiffviewRefresh",
	},
	opts = {
		keymaps = {
			disable_defaults = false,
			view = {
				{ "n", "q", ":DiffviewClose<CR>", { desc = "Close DiffView" } },
			},
			file_panel = {
				{ "n", "q", ":DiffviewClose<CR>", { desc = "Close DiffView" } },
			},
			file_history_panel = {
				{ "n", "q", ":DiffviewClose<CR>", { desc = "Close DiffView" } },
			},
		},
	},
}

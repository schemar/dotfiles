return {
	"glepnir/dashboard-nvim",
	event = "VimEnter",
	opts = {
		theme = "hyper",
		config = {
			week_header = {
				enable = true,
			},
			shortcut = {
				{ desc = " Update", group = "@property", action = "Lazy update", key = "u" },
				{
					desc = " Files",
					group = "@property",
					action = "Telescope find_files",
					key = "f",
				},
				{ desc = "󰗼 Quit", group = "@property", action = "q", key = "q" },
			},
			footer = {},
		},
	},
	dependencies = { { "nvim-tree/nvim-web-devicons" } },
}

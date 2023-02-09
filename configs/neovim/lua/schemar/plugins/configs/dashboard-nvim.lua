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
				{ desc = "ﮮ Update", group = "@property", action = "Lazy update", key = "u" },
				{
					desc = " Files",
					group = "Label",
					action = "Telescope find_files",
					key = "f",
				},
			},
			footer = {},
		},
	},
	dependencies = { { "nvim-tree/nvim-web-devicons" } },
}

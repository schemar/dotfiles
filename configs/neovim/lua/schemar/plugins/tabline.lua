return {
	"kdheepak/tabline.nvim",
	lazy = false,
	dependencies = {
		{ "nvim-lualine/lualine.nvim", name = "lualine" },
		"nvim-tree/nvim-web-devicons",
	},
	opts = {
		options = {
			show_filename_only = true,
			show_tabs_only = true,
		},
	},
}

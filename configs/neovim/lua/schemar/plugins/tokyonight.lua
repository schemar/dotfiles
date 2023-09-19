return {
	"folke/tokyonight.nvim",
	lazy = false,
	priority = 1000,
	opts = {
		style = "day",
		sidebars = { "aerial", "qf", "help", "NvimTree" },
		dim_inactive = true,
		on_colors = function(colors)
			local util = require("tokyonight.util")
			-- Make bg_dark actually darker, even though tokyonight "day" usually
			-- inverts colors meaning bg_dark by default is lighter than bg.
			colors.bg_dark = util.blend(colors.bg, colors.fg, 0.97)
		end,
	},
}

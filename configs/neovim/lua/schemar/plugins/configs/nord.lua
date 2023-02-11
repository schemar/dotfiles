return {
	"shaunsingh/nord.nvim", -- The better nord theme
	priority = 1000,
	config = function(_, opts)
		vim.g.nord_contrast = true -- Different background for sidebars and pop-ups (e.g. Telescope)
		vim.g.nord_borders = true -- Borders between windows visible
		vim.g.nord_disable_background = false -- When true, use terminal background instead of theme background
		vim.g.nord_italic = true -- Italic highlights of text
		vim.g.nord_uniform_diff_background = true -- Colorful backgrounds in diff mode
		vim.g.nord_bold = false -- Bold highlights of text
		-- Load the colorscheme
		require("nord").set()
	end,
}

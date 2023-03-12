return {
	"akinsho/toggleterm.nvim", -- Toggle terminals like Doom Emacs
	version = "*",
	cmd = { "ToggleTerm", "ToggleTermToggleAll" },
	keys = {
		{ "<leader>l", ":ToggleTerm<CR>", silent = true, desc = "Toggle terminal" },
		{ "<leader>L", ":ToggleTermToggleAll<CR>", silent = true, desc = "Toggle all terminals" },
	},
	opts = {
		direction = "horizontal",
		-- size can be a number or function which is passed the current terminal
		size = function(term)
			if term.direction == "horizontal" then
				return 15
			elseif term.direction == "vertical" then
				return vim.o.columns * 0.4
			end
		end,
	},
}

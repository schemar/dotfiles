return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	config = function()
		local wk = require("which-key")
		wk.register({
			["<c-w>"] = {
				name = "Window",
				s = { "<cmd>vsplit<cr>", "Split window" },
				v = { "<cmd>split<cr>", "Split window verticallly" },
			},
		})
	end,
}

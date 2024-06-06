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
			["<leader>"] = {
				name = "Leader",
				g = "Git",
				l = {
					name = "Code",

					r = { vim.lsp.buf.rename, "Rename" },
				},
			},
		})
	end,
}

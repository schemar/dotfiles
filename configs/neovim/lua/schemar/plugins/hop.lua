return {
	"phaazon/hop.nvim", -- Snipe words/letters on screen
	branch = "v2", -- optional but strongly recommended
	event = { "BufReadPost", "BufNewFile" },
	config = function()
		require("hop").setup({ keys = "asdfghjkl;" })

		local wk = require("which-key")
		wk.register({
			["m"] = { ":HopWord<CR>", "Move cursor to word" },
		})
	end,
}

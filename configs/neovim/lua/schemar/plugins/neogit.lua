return {
	"NeogitOrg/neogit",
	dependencies = { "nvim-lua/plenary.nvim" },
	cmd = "Neogit",
	config = function()
		local icons = require("schemar.icons").ui
		require("neogit").setup({
			disable_commit_confirmation = true,
			disable_builtin_notifications = true,
			signs = {
				-- { CLOSED, OPENED }
				section = { icons.ChevronShortRight, icons.ChevronShortDown },
				item = { icons.ChevronShortRight, icons.ChevronShortDown },
				hunk = { "", "" },
			},
		})
	end,
}

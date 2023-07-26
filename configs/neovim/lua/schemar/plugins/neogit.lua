return {
	"NeogitOrg/neogit",
	dependencies = {
		"sindrets/diffview.nvim",
		"nvim-telescope/telescope.nvim",
		"nvim-lua/plenary.nvim",
	},
	cmd = "Neogit",
	config = function()
		local icons = require("schemar.icons").ui
		require("neogit").setup({
			disable_commit_confirmation = true,
			disable_builtin_notifications = true,
			integrations = {
				telescope = true,
				diffview = true,
			},
			signs = {
				-- { CLOSED, OPENED }
				section = { icons.ChevronShortRight, icons.ChevronShortDown },
				item = { icons.ChevronShortRight, icons.ChevronShortDown },
				hunk = { "", "" },
			},
		})
	end,
}

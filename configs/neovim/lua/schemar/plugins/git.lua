local wk = require("which-key")

-- Normal mode
wk.register({
	g = {
		name = "Git",
		b = {
			function()
				require("telescope.builtin").git_branches()
			end,
			"Branches",
		},
		d = { ":DiffviewOpen<CR>", "Diff view" },
		f = { ":DiffviewFileHistory -f %<CR>", "File history" },
		g = { ":Neogit<CR>", "Neogit" },
		l = { "V:'<,'>DiffviewFileHistory -f<CR>", "Line history" },
		t = { ":Gitsigns toggle_current_line_blame<CR>", "Toggle blame" },
	},
}, { prefix = "<leader>" })

-- Visual mode
wk.register({
	g = {
		name = "Git",
		l = { ":'<,'>DiffviewFileHistory -f<CR>", "Line history" },
	},
}, { prefix = "<leader>", mode = "v" })

return {
	{
		"lewis6991/gitsigns.nvim", -- Git gutter
		event = { "BufReadPost", "BufNewFile" },
		config = function(_, opts)
			require("gitsigns").setup(opts)
			-- This is for diagnostic signs on the line number column.
			-- Use this to beautify the plain E W signs.
			local signs = require("schemar.icons").diagnostics
			for type, icon in pairs(signs) do
				local hl = "DiagnosticSign" .. type
				vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
			end
		end,
	},
	{
		"tpope/vim-fugitive",
		cmd = { "Git", "Gedit", "Gdiffsplit", "Gvdiffsplit" },
	},
	{
		"sindrets/diffview.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons", "nvim-lua/plenary.nvim" },
		cmd = { "DiffviewOpen", "DiffviewFileHistory" },
	},
	{
		"TimUntersberger/neogit", -- Think magit
		cmd = "Neogit",
		opts = {
			disable_builtin_notifications = true,
		},
	},
}

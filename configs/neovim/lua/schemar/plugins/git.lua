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
		"tpope/vim-fugitive", -- For :Git
		cmd = { "Git", "Gedit", "Gdiffsplit", "Gvdiffsplit" },
	},
	{
		"sindrets/diffview.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons", "nvim-lua/plenary.nvim" },
		cmd = { "DiffviewOpen", "DiffviewFileHistory" },
		config = function()
			local icons = require("schemar.icons")
			local actions = require("diffview.actions")

			require("diffview").setup({
				icons = { -- Only applies when use_icons is true.
					folder_closed = icons.Folder,
					folder_open = icons.FolderOpen,
				},
				signs = {
					fold_closed = icons.ChevronShortRight,
					fold_open = icons.ChevronShortDown,
					done = icons.BoxChecked,
				},
				keymaps = {
					view = {
						{ "n", "<leader>ft", actions.toggle_files },
					},
					file_panel = {
						{ "n", "j", actions.select_next_entry },
						{ "n", "k", actions.select_prev_entry },
						{ "n", "q", ":tabclose<CR>" },
						{ "n", "<cr>", "<c-w>k" },
						{ "n", "o", actions.close },
						{ "n", "<leader>ft", actions.toggle_files },
					},
					file_history_panel = {
						{ "n", "j", actions.select_next_entry },
						{ "n", "k", actions.select_prev_entry },
						{ "n", "q", ":tabclose<CR>" },
						{ "n", "<cr>", "<c-w>k" },
						{ "n", "o", actions.close },
						{ "n", "<leader>ft", actions.toggle_files },
					},
				},
			})
		end,
	},
	{
		"TimUntersberger/neogit", -- Think magit
		cmd = "Neogit",
		opts = {
			disable_builtin_notifications = true,
		},
	},
}

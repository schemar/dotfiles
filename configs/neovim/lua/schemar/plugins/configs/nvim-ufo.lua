return {
	"kevinhwang91/nvim-ufo", -- Better folding
	name = "ufo",
	event = { "BufReadPost", "BufNewFile" },
	dependencies = { "kevinhwang91/promise-async" },
	priority = 1, -- Load after LSP, etc.
	opts = {
		close_fold_kinds = { "imports" },
	},
	config = function(_, opts)
		vim.o.foldcolumn = "1" -- '0' is not bad
		-- Disable the fold column for certain filetypes:
		vim.api.nvim_create_autocmd("Filetype", {
			pattern = {
				"aerial",
				"gitcommit",
				"NvimTree",
				"NeogitCommitMessage",
				"NeogitStatus",
			},
			command = "set foldcolumn=0",
		})

		vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
		vim.o.foldlevelstart = 99
		vim.o.foldenable = true

		-- Must be overridden in order for ufo to keep its folds
		vim.keymap.set("n", "zR", require("ufo").openFoldsExceptKinds)
		vim.keymap.set("n", "zM", require("ufo").closeAllFolds)

		require("ufo").setup(opts)
	end,
}

return {
	"kevinhwang91/nvim-ufo", -- Better folding
	name = "ufo",
	event = { "BufReadPost", "BufNewFile" },
	dependencies = { "kevinhwang91/promise-async" },
	priority = 1, -- Load after LSP, etc.
	config = function(_, opts)
		vim.o.foldcolumn = "0" -- '0' to hide or '1' to show

		vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
		vim.o.foldlevelstart = 99
		vim.o.foldenable = true

		-- Must be overridden in order for ufo to keep its folds
		vim.keymap.set("n", "zR", require("ufo").openFoldsExceptKinds)
		vim.keymap.set("n", "zM", require("ufo").closeAllFolds)

		require("ufo").setup({
			close_fold_kinds = { "imports" },
		})
	end,
}

return {
	"kevinhwang91/nvim-ufo",
	dependencies = { "kevinhwang91/promise-async" },
	event = { "BufReadPost", "BufNewFile" },
	config = function()
		vim.o.foldcolumn = "0" -- '0' to hide or '1' to show
		vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
		vim.o.foldlevelstart = 99
		vim.o.foldenable = true

		-- Using ufo provider need remap `zR` and `zM`. If Neovim is 0.6.1, remap yourself
		vim.keymap.set("n", "zR", require("ufo").openAllFolds)
		vim.keymap.set("n", "zM", require("ufo").closeAllFolds)

		-- Use Option 2 as source: nvim lsp as LSP client.
		-- Only this option allows to fold the imports.
		require("ufo").setup({
			close_fold_kinds_for_ft = { default = { "imports" } },
		})
	end,
}

return {
	"RRethy/vim-illuminate", -- Highlight similar words (e.g. references with LSP)
	opts = {
		filetypes_denylist = {
			"dirvish",
			"fugitive",
			"aerial",
			"NvimTree",
			"NeogitCommitMessage",
			"NeogitStatus",
		},
	},
	config = function(_, opts)
		require("illuminate").configure(opts)
	end,
}

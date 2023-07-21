return {
	"akinsho/toggleterm.nvim",
	event = { "VeryLazy" },
	version = "*",
	opts = {
		open_mapping = [[<c-A>]],
		insert_mappings = true, -- whether or not the open mapping applies in insert mode
		terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
		direction = "float",
		close_on_exit = false,
		float_opts = {
			border = "rounded",
		},
	},
}

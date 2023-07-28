return {
	"folke/todo-comments.nvim",
	dependencies = { "nvim-lua/plenary.nvim" },
	event = { "VeryLazy" },
	opts = {
		keywords = {
			TODOMS = { icon = " ", color = "info" },
		},
	},
}

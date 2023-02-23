return {
	"echasnovski/mini.starter",
	version = false,
	event = "VimEnter",
	config = function()
		require("mini.starter").setup()
	end,
}

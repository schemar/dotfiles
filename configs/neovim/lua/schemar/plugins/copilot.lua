return {
	"github/copilot.vim",
	event = "VeryLazy",
	init = function()
		-- Ignores command as 16 is "end of life"
		-- vim.g.copilot_node_command = "/Users/schemar/.asdf/installs/nodejs/16.20.2/bin/node"
		vim.g.copilot_no_tab_map = true
		vim.g.copilot_assume_mapped = true
		vim.g.copilot_tab_fallback = ""

		vim.api.nvim_set_keymap("i", "<C-b>", 'copilot#Accept("<CR>")', { silent = true, expr = true })
	end,
}

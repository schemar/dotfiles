-- Use VTSLS' code actions, as they provide more options than FZF-Lua's LSP code
-- actions.
vim.api.nvim_set_keymap(
	"n",
	"<leader>la",
	"<cmd>VtsExec source_actions<cr>",
	{ noremap = true, silent = true, desc = "Code actions" }
)

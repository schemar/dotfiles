-- Disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- [[ Package manager installation ]]
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

-- [[ Leaders ]]
-- These bindings must be set before initializing lazy.nvim
-- These keybindings need to be defined before the first /
-- is called; otherwise, it will default to "\"
vim.g.mapleader = " "
vim.g.maplocalleader = ","

require("schemar.options")
require("lazy").setup("schemar.plugins")
require("schemar.keymaps")

-- [[ Highlight Yanking ]]
vim.cmd([[autocmd TextYankPost * silent! lua vim.highlight.on_yank()]])

-- Keep window around when terminal closes
vim.api.nvim_create_autocmd("TermClose", {
	callback = function()
		local buf = vim.api.nvim_get_current_buf()
		vim.api.nvim_buf_set_keymap(buf, "n", "<Esc>", ":Bdelete<CR>", { noremap = true })
		vim.api.nvim_buf_set_keymap(buf, "t", "<Esc>", ":Bdelete<CR>", { noremap = true })
	end,
})

-- [[ Borders ]]
local _border = "single"

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = _border,
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = _border,
})

vim.diagnostic.config({
	float = {
		border = _border,
		source = "always",
	},
	signs = true,
	underline = true,
	update_in_insert = false,
	virtual_text = true,
	severity_sort = true,
})

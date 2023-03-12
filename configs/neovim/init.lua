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
vim.g.localleader = ","

require("schemar.options")
require("lazy").setup("schemar.plugins")
require("schemar.keymaps")

-- [[ Terminal ]]
-- Auto-close without losing window (see capital `B` in `Bdelete`):
-- See also https://github.com/neovim/neovim/issues/14986
vim.cmd([[autocmd TermClose * execute 'Bdelete! ' . expand('<abuf>')]])

-- [[ Highlight Yanking ]]
vim.cmd([[augroup SchemarYankingAutocommand]])
vim.cmd([[autocmd!]])
vim.cmd([[autocmd TextYankPost * silent! lua vim.highlight.on_yank()]])
vim.cmd([[augroup END]])

-- [[ Borders ]]
local _border = "single"

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = _border,
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = _border,
})

vim.diagnostic.config({
	float = { border = _border },
})

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- These bindings must be set before initializing lazy.nvim
vim.g.mapleader = " "

require("schemar.options")

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

require("lazy").setup("schemar.plugins", { defaults = { lazy = true } })

vim.cmd.colorscheme("catppuccin")

require("schemar.languages")
require("schemar.keymaps")

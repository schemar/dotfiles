-- Disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- LEADER
-- These keybindings need to be defined before the first /
-- is called; otherwise, it will default to "\"
vim.g.mapleader = " "
vim.g.localleader = ","

require("schemar.options")
require("schemar.plugins")
require("schemar.keymaps")

-- [[Colors]]
-- Main theme.
vim.cmd([[colorscheme nord]])

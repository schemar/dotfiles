require("schemar.config.options") -- Make sure to load this first so that "leader" is set before loading plugins.
require("schemar.config.lazy") -- Load lazy.nvim and plugins
require("schemar.config.keymaps").init() -- Load keymaps
require("schemar.config.autocmds") -- Load autocmds

-- Prepend mise shims to PATH
vim.env.PATH = vim.env.HOME .. "/.local/share/mise/shims:" .. vim.env.PATH

-- Progress moving plugins
-- catppuccin done
-- comments done
-- completion done
-- editor done
-- files done
-- formatter done
-- git done
-- keys done
-- lspconfig still missing extraction of servers to plugins/languages/
--           from null-ls as well as lsp-config
-- markdown done
-- scnacks done
-- ufo done

local M = {}

--- Escapes characters when mapping terminal mode keys.
-- This is required when mapping terminal mode commands.
-- See also https://github.com/folke/which-key.nvim/issues/125
M.terminal_escape = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

--- Wrapper for vim commands in terminal mode.
-- Automatically prefixes and postfixes the command.
M.terminal_command = function(str)
	return M.terminal_escape("<C-\\><C-N>:<C-U>" .. str .. "<CR>")
end

return M

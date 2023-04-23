local M = {}

--- Escapes characters when mapping terminal mode keys.
M.terminal_escape = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

return M

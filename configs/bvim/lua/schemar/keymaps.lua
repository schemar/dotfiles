-- Note that additional keymaps are defined in the LSP and cmp files.
--
-- Which-Key
local wk = require("which-key")

-- Without prefix:
wk.register({
	["<c-w>"] = {
		name = "Windows",
		s = { ":vsplit<CR>", "Split window" },
		v = { ":split<CR>", "Split window vertically" },
	},
})

-- With leader prefix:
wk.register({
	b = {
		name = "Buffer",
		n = { ":bn<CR>", "Next buffer" },
		p = { ":bp<CR>", "Previous buffer" },
	},
	f = {
		name = "File",
		o = { ":Other<CR>", "Open other file" },
		O = { ":OtherVSplit<CR>", "Open other file in split" },
		s = { ":w<CR>", "Save" },
		S = { ":wa<CR>", "Save all" },
	},
	w = {
		name = "Window",
		c = { ":close<CR>", "Close window" },
		s = { ":vsplit<CR>", "Split window" },
		v = { ":split<CR>", "Vertically split window" },
	},
	q = { name = "Quit", q = { ":quitall<CR>", "Quit all" } },
	u = {
		function()
			require("lazy").update({ show = false })
			require("mason-registry").refresh(function() end) -- Providing an empty fallback to make sure the refresh is async.
		end,
		"Update",
	},
	z = {
		function()
			-- If there is more than one window, zoom it by opening a new tab with only
			-- one window with the current buffer.
			-- If there is only one window, close the tabpage.
			-- This way, you can "zoom" and "unzoom" a buffer.

			-- allWindows includes windows like LSP messages.
			local allWindows = vim.api.nvim_tabpage_list_wins(0)
			-- normalWindows are only the "normal" ones that show buffers to edit.
			local normalWindows = vim.tbl_filter(function(key)
				return vim.api.nvim_win_get_config(key).relative == ""
			end, allWindows)
			-- We want the count of "normal" windows to decide whether to zoom or
			-- unzoom.
			local windowCount = #normalWindows

			if windowCount > 1 then
				vim.cmd([[tab split]])
			else
				-- Check if we have more than one tabpage.
				-- Cannot close the last tabpage.
				if vim.api.nvim_call_function("tabpagenr", { "$" }) > 1 then
					vim.cmd([[tabclose]])
				end
			end
		end,
		"(Un)Zoom buffer in new tab",
	},
}, { prefix = "<leader>" })

-- Terminal:
local terminal_escape = require("schemar.utils").terminal_escape
local terminal_command = require("schemar.utils").terminal_command
wk.register({
	["<esc><esc>"] = { terminal_escape("<C-\\><C-N>"), "Escape terminal mode" },
	["<C-h>"] = { terminal_command("TmuxNavigateLeft"), "Tmux left" },
	["<C-j>"] = { terminal_command("TmuxNavigateDown"), "Tmux down" },
	["<C-k>"] = { terminal_command("TmuxNavigateUp"), "Tmux up" },
	["<C-l>"] = { terminal_command("TmuxNavigateRight"), "Tmux right" },
}, { mode = "t" })

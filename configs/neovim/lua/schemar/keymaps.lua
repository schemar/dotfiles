-- Note that additional keymaps are defined in the LSP and cmp files.
--
-- Which-Key
local wk = require("which-key")
local telescope = require("telescope")
local builtin = require("telescope.builtin")

-- Without prefix:
wk.register({
	["<c-w>"] = {
		name = "Windows",
		s = { ":vsplit<CR>", "Split window" },
		v = { ":split<CR>", "Split window vertically" },
	},
	["["] = {
		name = "Previous",
		d = { vim.diagnostic.goto_prev, "Previous diagnostic" },
		b = { ":bp<CR>", "Previous buffer" },
	},
	["]"] = {
		name = "Next",
		d = { vim.diagnostic.goto_next, "Next diagnostic" },
		b = { ":bn<CR>", "Next buffer" },
	},
})

-- With leader prefix:
wk.register({
	["<leader>"] = { builtin.find_files, "Find file" },
	["/"] = { builtin.live_grep, "Grep directory" },
	["?"] = { builtin.current_buffer_fuzzy_find, "Grep current buffer" },
	[";"] = { builtin.command_history, "Command history" },
	[":"] = { builtin.commands, "Commands" },
	r = { builtin.resume, "Resume telescope" },
	b = {
		name = "Buffer",
		b = { builtin.buffers, "Find buffer" },
		d = { ":Bdelete<CR>", "Delete buffer" },
		n = { ":bn<CR>", "Next buffer" },
		p = { ":bp<CR>", "Previous buffer" },
	},
	c = {
		name = "Code",
		d = { ":Trouble document_diagnostics<CR>", "Show diagnostics" },
		D = {
			":Trouble workspace_diagnostics<CR>",
			"Show workspace diagnostics",
		},
		e = { vim.diagnostic.open_float, "Floating diagnostic" },
	},
	f = {
		name = "File",
		f = { builtin.oldfiles, "Find previously open file" },
		F = {
			function()
				telescope.extensions.file_browser.file_browser({
					hidden = true,
					grouped = true,
					initial_mode = "normal",
				})
			end,
			"Browse files",
		},
		o = { ":Other<CR>", "Open other file" },
		O = { ":OtherVSplit<CR>", "Open other file in split" },
		s = { ":w<CR>", "Save" },
		S = { ":wa<CR>", "Save all" },
		t = { ":NvimTreeFindFile<CR>", "Go to current file in file tree" },
		T = { ":NvimTreeToggle<CR>", "Toggle file tree" },
	},
	g = {
		name = "Git",
		b = { ":Gitsigns toggle_current_line_blame<CR>", "Toggle blame" },
		g = { ":Neogit<CR>", "Neogit" },
	},
	h = { builtin.help_tags, "Help" },
	w = {
		name = "Window",
		c = { ":close<CR>", "Close window" },
		s = { ":vsplit<CR>", "Split window" },
		v = { ":split<CR>", "Vertically split window" },
	},
	q = { name = "Quit", q = { ":quitall<CR>", "Quit all" } },
}, { prefix = "<leader>" })

-- Terminal:
wk.register({ ["<esc><esc>"] = { "<C-\\><C-n>", "Escape terminal mode." } }, { mode = "t" })

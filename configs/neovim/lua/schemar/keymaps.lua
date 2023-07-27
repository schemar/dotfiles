local wk = require("which-key")

-- No prefix:
wk.register({
	["["] = {
		name = "Previous",
		c = {
			function()
				require("gitsigns").prev_hunk()
			end,
			"Previous hunk",
		},
		e = { vim.diagnostic.goto_prev, "Previous diagnostic" },
		E = {
			function()
				vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR })
			end,
			"Previous error diagnostic",
		},
		t = { "<cmd>tabprevious<cr>", "Previous tab" },
	},
	["]"] = {
		name = "Next",
		c = {
			function()
				require("gitsigns").next_hunk()
			end,
			"Next hunk",
		},
		e = { vim.diagnostic.goto_next, "Next diagnostic" },
		E = {
			function()
				vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR })
			end,
			"Next error diagnostic",
		},
		t = { "<cmd>tabnext<cr>", "Next tab" },
	},
	["<c-w>"] = {
		name = "Window",
		s = { "<cmd>vsplit<cr>", "Split window" },
		v = { "<cmd>split<cr>", "Split window verticallly" },
	},
	g = {
		name = "Go",
		d = {
			"<cmd>Trouble lsp_definitions<cr>",
			"Go to definition",
		},
		i = {
			"<cmd>Trouble lsp_implementations<cr>",
			"Go to implementations",
		},
		r = {
			"<cmd>Trouble lsp_references<cr>",
			"Go to references",
		},
		t = { "<cmd>tabnext<cr>", "Next tab" },
		T = { "<cmd>tabprevious<cr>", "Previous tab" },
	},

	K = { vim.lsp.buf.hover, "Hover Doc" },
})

-- <leader> prefix:
wk.register({
	["<leader>"] = { "<cmd>Telescope smart_open<cr>", "Search file" },
	["/"] = { "<cmd>Telescope live_grep<cr>", "Grep" },
	r = { "<cmd>Telescope resume<cr>", "Resume telescope" },
	b = {
		name = "Buffer",
		b = { "<cmd>Telescope buffers<cr>", "Find buffer" },
		d = { "<cmd>bdelete<cr>", "Delete buffer" },
	},
	f = {
		name = "File",
		b = { "<cmd>NvimTreeToggle<cr>", "Browse files" },
		B = { "<cmd>NvimTreeFindFileToggle<cr>", "Browse files (curr. dir.)" },
		f = { "<cmd>Telescope find_files<cr>", "Find file" },
		o = { "<cmd>Other<cr>", "Open 'other' file" },
		O = { "<cmd>OtherClear<cr><cmd>Other<cr>", "Open 'other' file with select" },
		["<c-o>"] = { "<cmd>OtherVSplit<cr>", "Open 'other' file" },
		r = { "<cmd>Telescope oldfiles<cr>", "Open recent file" },
	},
	g = {
		name = "Git",
		g = { "<cmd>Neogit<cr>", "Git overview" },
		d = {
			name = "Diff",
			d = { "<cmd>DiffviewOpen<cr>", "Diff view" },
			b = { "<cmd>DiffviewFileHistory<cr>", "Diff branch" },
			f = { "<cmd>DiffviewFileHistory %<cr>", "Diff file" },
		},
		D = {
			function()
				require("gitsigns").toggle_deleted()
			end,
			"Show deleted lines",
		},
		s = {
			function()
				require("gitsigns").stage_hunk()
			end,
			"Stage hunk",
		},
		u = {
			function()
				require("gitsigns").undo_stage_hunk()
			end,
			"Unstage hunk",
		},
		x = {
			function()
				require("gitsigns").reset_hunk()
			end,
			"Reset hunk",
		},
	},
	l = {
		name = "Code",
		f = { "<cmd>Format<cr>", "Format file" },
		a = {
			function()
				vim.lsp.buf.code_action({ apply = true })
			end,
			"Code actions",
		},
		d = { "<cmd>Trouble document_diagnostics<cr>", "Show diagnostics" },
		D = {
			"<cmd>Trouble workspace_diagnostics<cr>",
			"Show workspace diagnostics",
		},
		e = { vim.diagnostic.open_float, "Floating diagnostic" },
		o = { "<cmd>AerialToggle<cr>", "Toggle code outline" },
		r = { vim.lsp.buf.rename, "Rename" },
		s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document symbols" },
		S = { "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "Workspace symbols" },
	},
	s = {
		name = "Cursor",
		r = {
			function()
				require("flash").remote()
			end,
			"Jump (remote)",
			mode = "o",
		},
		R = {
			function()
				require("flash").treesitter_search()
			end,
			"Treesitter search",
			mode = { "x", "o" },
		},
		s = {
			function()
				require("flash").jump()
			end,
			"Jump",
			mode = { "n", "x", "o" },
		},
		S = {
			function()
				require("flash").treesitter()
			end,
			"Jump (treesitter)",
			mode = { "n", "x", "o" },
		},
	},
	t = {
		name = "Tab",
		t = { "<cmd>tabnew<cr>", "New tab" },
		j = { "<cmd>tabnext<cr>", "Next tab" },
		k = { "<cmd>tabprevious<cr>", "Previous tab" },
		c = { "<cmd>tabclose<cr>", "Close tab" },
	},
}, { prefix = "<leader>" })

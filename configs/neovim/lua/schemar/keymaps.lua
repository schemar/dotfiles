local wk = require("which-key")

-- No prefix<cmd>
wk.register({
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
		t = { vim.lsp.buf.type_definition, "Go to type definition" },
	},

	K = { vim.lsp.buf.hover, "Hover Doc" },
})

-- <leader> prefix:
wk.register({
	["<leader>"] = { "<cmd>Telescope smart_open<cr>", "Search file" },
	b = {
		name = "Buffer",
		b = { "<cmd>Telescope buffers<cr>", "Find buffer" },
	},
	c = {
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
		r = { vim.lsp.buf.rename, "Rename" },
		s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document symbols" },
		S = { "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "Workspace symbols" },
	},
	f = {
		name = "File",
		b = { "<cmd>Telescope file_browser<cr>", "Browse files" },
		B = { "<cmd>Telescope file_browser path=%<cmd>p<cmd>h select_buffer=true<cr>", "Browse files (curr. dir.)" },
		f = { "<cmd>Telescope find_files<cr>", "Find file" },
		r = { "<cmd>Telescope oldfiles<cr>", "Open recent file" },
	},
}, { prefix = "<leader>" })

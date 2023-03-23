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
	z = { ":tab split<CR>", "Zoom buffer in new tab" },
	Z = { ":tabclose<CR>", "Unzoom by closing tab" },
}, { prefix = "<leader>" })

-- Terminal:
wk.register({ ["<esc><esc>"] = { "<C-\\><C-n>", "Escape terminal mode." } }, { mode = "t" })

return {
	-- [[ Utils ]]
	{
		"nvim-lua/plenary.nvim", -- Plugin with util functions required by other plugins
		lazy = true,
	},

	-- [[ Theming ]]
	{
		"folke/todo-comments.nvim", -- Highlight and list TODOs, etc.
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			highlight = {
				pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlightng (vim regex)
				-- Was [[.*<(KEYWORDS)\s*:]] including colon.
			},
			search = {
				pattern = [[\b(KEYWORDS)]], -- ripgrep regex
				-- Was [[\b(KEYWORDS):]] including colon.
			},
		},
	},
	{
		"norcalli/nvim-colorizer.lua", -- Show color-codes colored
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
	{
		"danilamihailov/beacon.nvim", -- Highlight cursor on jump
		event = { "BufReadPost", "BufNewFile" },
	},

	-- [[ Interface ]]
	{
		"christoomey/vim-tmux-navigator", -- Switch windows/panes vim/tmux
		event = { "VeryLazy" },
	},

	{
		"folke/which-key.nvim", -- Like Emacs which key
		event = { "VeryLazy" },
		config = true,
	},

	{
		"booperlv/nvim-gomove", -- Alt-h/j/k/l to move line
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
	{
		"famiu/bufdelete.nvim", -- Keep windows around when deleting buffers
		cmd = "Bdelete",
		keys = {
			{ "<leader>bd", ":Bdelete<CR>", desc = "Delete buffer" },
		},
	},
	{
		"rgroli/other.nvim", -- Go to alternative file, e.g. ts<->vue or test
		name = "other-nvim",
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			-- Map files to other files.
			-- See documentation for details and more options.
			mappings = {
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.vue", context = "vue" },
				{ pattern = "(.*)/(.*).vue", target = "%1/%2.ts", context = "vue" },
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.spec.ts", context = "spec" },
				{ pattern = "(.*)/(.*).spec.ts", target = "%1/%2.ts", context = "spec" },
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.test.ts", context = "test" },
				{ pattern = "(.*)/(.*).test.ts", target = "%1/%2.ts", context = "test" },
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.unit.test.ts", context = "test" },
				{ pattern = "(.*)/(.*).unit.test.ts", target = "%1/%2.ts", context = "test" },
			},
		},
	},

	{
		"windwp/nvim-autopairs", -- Auto-pair tags, etc.
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
}

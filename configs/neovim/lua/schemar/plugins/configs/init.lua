return {
	-- [[ Utils ]]
	"nvim-lua/plenary.nvim", -- Plugin with util functions required by other plugins
	"L3MON4D3/LuaSnip", -- Snippets plugin

	-- [[ Theming ]]
	"mrjones2014/nvim-ts-rainbow", -- Rainbow parentheses
	{
		"lewis6991/gitsigns.nvim", -- Git gutter
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},

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
	"danilamihailov/beacon.nvim", -- Highlight cursor on jump
	{
		"nvim-tree/nvim-web-devicons", -- Fancy icons in pop-ups
		opts = {
			-- globally enable different highlight colors per icon (default to true)
			-- if set to false all icons will have the default icon's color
			color_icons = true,
			-- globally enable default icons (default to false)
			-- will get overriden by `get_icons` option
			default = true,
		},
	},
	"onsails/lspkind.nvim", -- Icons in completion dialogue

	-- [[ Interface ]]
	"christoomey/vim-tmux-navigator", -- Switch windows/panes vim/tmux

	{
		"kylechui/nvim-surround", -- E.g. cs"' to replace surrounding " with '
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
	{
		"folke/which-key.nvim", -- Like Emacs which key
		config = true,
	},

	{
		"numToStr/Comment.nvim", -- Easier (un)commenting
		dependencies = { "JoosepAlviste/nvim-ts-context-commentstring" },
		event = { "BufReadPost", "BufNewFile" },
		opts = {
			pre_hook = function()
				return require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook()
			end,
		},
	},
	"JoosepAlviste/nvim-ts-context-commentstring", -- Improved comment management; integrates with Comment.nvim

	{
		"booperlv/nvim-gomove", -- Alt-h/j/k/l to move line
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
	"famiu/bufdelete.nvim", -- Keep windows around when deleting buffers
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
			},
		},
	},

	-- [[ Languages ]]
	"neovim/nvim-lspconfig", -- Configurations for Nvim LSP
	"jose-elias-alvarez/null-ls.nvim", -- NeoVim as LSP server
	"lukas-reineke/lsp-format.nvim", -- Easier management of auto-saving from LSP sources

	"williamboman/mason.nvim", -- Manage language servers, linters, etc.
	"williamboman/mason-lspconfig.nvim", -- Integration mason/lsp
	"jayp0521/mason-null-ls.nvim", -- Integration mason/null-ls

	"SmiteshP/nvim-navic", -- Winbar breadcrumbs, e.g. for code context

	"b0o/schemastore.nvim", -- Schemas for JSON files

	{
		"windwp/nvim-autopairs", -- Auto-pair tags, etc.
		event = { "BufReadPost", "BufNewFile" },
		config = true,
	},
	"windwp/nvim-ts-autotag", -- Auto-tags for HTML, Vue, etc.
}

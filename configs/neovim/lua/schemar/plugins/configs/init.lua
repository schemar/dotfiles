return {
	-- [[ Utils ]]
	"nvim-lua/plenary.nvim", -- Plugin with util functions required by other plugins
	"L3MON4D3/LuaSnip", -- Snippets plugin

	-- [[ Theming ]]
	"mrjones2014/nvim-ts-rainbow", -- Rainbow parentheses
	"nvim-lualine/lualine.nvim", -- Modeline
	"lewis6991/gitsigns.nvim", -- Git gutter

	"folke/todo-comments.nvim", -- Highlight and list TODOs, etc.
	"norcalli/nvim-colorizer.lua", -- Show color-codes colored
	"danilamihailov/beacon.nvim", -- Highlight cursor on jump
	"nvim-tree/nvim-web-devicons", -- Fancy icons in pop-ups
	"onsails/lspkind.nvim", -- Icons in completion dialogue

	-- [[ Interface ]]
	"christoomey/vim-tmux-navigator", -- Switch windows/panes vim/tmux

	{ "nvim-telescope/telescope.nvim", branch = "0.1.x" }, -- Fancy picker (think fzf)
	"nvim-telescope/telescope-file-browser.nvim", -- Think Emacs directory browser
	{
		"nvim-telescope/telescope-fzf-native.nvim", -- FZF algorithm for telescope
		build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
	},
	"folke/trouble.nvim", -- Better looking quicklist, diagnostics, etc.
	{
		"kylechui/nvim-surround", -- E.g. cs"' to replace surrounding " with '
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
	},
	"folke/which-key.nvim", -- Like Emacs which key

	"numToStr/Comment.nvim", -- Easier (un)commenting
	"JoosepAlviste/nvim-ts-context-commentstring", -- Improved comment management; integrates with Comment.nvim

	"booperlv/nvim-gomove", -- Alt-h/j/k/l to move line
	"famiu/bufdelete.nvim", -- Keep windows around when deleting buffers
	"rgroli/other.nvim", -- Go to alternative file, e.g. ts<->vue or test

	-- [[ Languages ]]
	"neovim/nvim-lspconfig", -- Configurations for Nvim LSP
	"jose-elias-alvarez/null-ls.nvim", -- NeoVim as LSP server
	"lukas-reineke/lsp-format.nvim", -- Easier management of auto-saving from LSP sources
	"MunifTanjim/prettier.nvim", -- Prettier for TS/JS formatting (for null-ls)

	"williamboman/mason.nvim", -- Manage language servers, linters, etc.
	"williamboman/mason-lspconfig.nvim", -- Integration mason/lsp
	"jayp0521/mason-null-ls.nvim", -- Integration mason/null-ls

	"ThePrimeagen/refactoring.nvim", -- Refactoring tools for code
	"SmiteshP/nvim-navic", -- Winbar breadcrumbs, e.g. for code context

	"b0o/schemastore.nvim", -- Schemas for JSON files

	"windwp/nvim-autopairs", -- Auto-pair tags, etc.
	"windwp/nvim-ts-autotag", -- Auto-tags for HTML, Vue, etc.

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			local ts_update = require("nvim-treesitter.install").update({
				with_sync = true,
			})
			ts_update()
		end,
	},
	"nvim-treesitter/nvim-treesitter-textobjects", -- Additional textobjects for treesitter
	"nvim-treesitter/nvim-treesitter-context", -- Keep e.g. function at top when scrolling below

	-- [[ Completion ]]
	"hrsh7th/cmp-nvim-lsp", -- LSP source for nvim-cmp
	"hrsh7th/cmp-nvim-lsp-signature-help", -- Function signature source for nvim-cmp
	"hrsh7th/cmp-buffer", -- Buffer source for nvim-cmp
	"hrsh7th/cmp-path", -- Path source for nvim-cmp
	"hrsh7th/cmp-cmdline", -- Command line source for nvim-cmp
	"saadparwaiz1/cmp_luasnip", -- Snippets source for nvim-cmp
	"hrsh7th/nvim-cmp", -- Autocompletion plugin
}

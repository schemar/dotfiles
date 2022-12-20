local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({
			"git",
			"clone",
			"--depth",
			"1",
			"https://github.com/wbthomason/packer.nvim",
			install_path,
		})
		vim.cmd([[packadd packer.nvim]])
		return true
	end
	return false
end

local _ = ensure_packer()

require("packer").startup(function(use)
	-- [[ Utils ]]
	use("wbthomason/packer.nvim") -- Package manager
	use("nvim-lua/plenary.nvim") -- Plugin with util functions required by other plugins
	use("L3MON4D3/LuaSnip") -- Snippets plugin

	-- [[ Theming ]]
	use("shaunsingh/nord.nvim") -- The better nord theme
	use("p00f/nvim-ts-rainbow") -- Rainbow parentheses
	use("lukas-reineke/indent-blankline.nvim") -- Indent guides
	use("nvim-lualine/lualine.nvim") -- Modeline
	use("lewis6991/gitsigns.nvim") -- Git gutter

	use("folke/todo-comments.nvim") -- Highlight and list TODOs, etc.
	use("norcalli/nvim-colorizer.lua") -- Show color-codes colored
	use("danilamihailov/beacon.nvim") -- Highlight cursor on jump
	use("nvim-tree/nvim-web-devicons") -- Fancy icons in pop-ups
	use("onsails/lspkind.nvim") -- Icons in completion dialogue
	use("RRethy/vim-illuminate") -- Highlight similar words (e.g. references with LSP)

	-- [[ Interface ]]
	use("christoomey/vim-tmux-navigator") -- Switch windows/panes vim/tmux

	use({ "nvim-telescope/telescope.nvim", branch = "0.1.x" }) -- Fancy picker (think fzf)
	use("nvim-telescope/telescope-file-browser.nvim") -- Think Emacs directory browser
	use("folke/trouble.nvim") -- Better looking quicklist, diagnostics, etc.
	use("nvim-tree/nvim-tree.lua") -- File browser
	use({
		"kylechui/nvim-surround", -- E.g. cs"' to replace surrounding " with '
		tag = "*", -- Use for stability; omit to use `main` branch for the latest features
	})

	use("TimUntersberger/neogit") -- Think magit
	use("folke/which-key.nvim") -- Like Emacs which key

	use("numToStr/Comment.nvim") -- Easier (un)commenting
	use("JoosepAlviste/nvim-ts-context-commentstring") -- Improved comment management; integrates with Comment.nvim

	use("booperlv/nvim-gomove") -- Alt-h/j/k/l to move line
	use("famiu/bufdelete.nvim") -- Keep windows around when deleting buffers
	use("rgroli/other.nvim") -- Go to alternative file, e.g. ts<->vue or test

	-- [[ Languages ]]
	use("neovim/nvim-lspconfig") -- Configurations for Nvim LSP
	use("jose-elias-alvarez/null-ls.nvim") -- NeoVim as LSP server
	use("lukas-reineke/lsp-format.nvim") -- Easier management of auto-saving from LSP sources
	use("MunifTanjim/prettier.nvim") -- Prettier for TS/JS formatting (for null-ls)

	use("williamboman/mason.nvim") -- Manage language servers, linters, etc.
	use("williamboman/mason-lspconfig.nvim") -- Integration mason/lsp
	use("jayp0521/mason-null-ls.nvim") -- Integration mason/null-ls

	use("ThePrimeagen/refactoring.nvim") -- Refactoring tools for code
	use("SmiteshP/nvim-navic") -- Winbar breadcrumbs, e.g. for code context

	use("lukas-reineke/headlines.nvim") -- Highlight markdown (and other) headlines
	use("b0o/schemastore.nvim") -- Schemas for JSON files

	use({
		"glepnir/lspsaga.nvim", -- UI Improvements for LSP
		branch = "main",
	})

	use("windwp/nvim-autopairs") -- Auto-pair tags, etc.
	use("windwp/nvim-ts-autotag") -- Auto-tags for HTML, Vue, etc.

	-- Treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = function()
			local ts_update = require("nvim-treesitter.install").update({
				with_sync = true,
			})
			ts_update()
		end,
	})
	use("nvim-treesitter/nvim-treesitter-textobjects") -- Additional textobjects for treesitter
	use("nvim-treesitter/nvim-treesitter-context") -- Keep e.g. function at top when scrolling below

	-- [[ Completion ]]
	use("hrsh7th/cmp-nvim-lsp") -- LSP source for nvim-cmp
	use("hrsh7th/cmp-nvim-lsp-signature-help") -- Function signature source for nvim-cmp
	use("hrsh7th/cmp-buffer") -- Buffer source for nvim-cmp
	use("hrsh7th/cmp-path") -- Path source for nvim-cmp
	use("hrsh7th/cmp-cmdline") -- Command line source for nvim-cmp
	use("saadparwaiz1/cmp_luasnip") -- Snippets source for nvim-cmp
	use("hrsh7th/nvim-cmp") -- Autocompletion plugin
end)

--
-- Indent Guides.
require("indent_blankline").setup({
	show_current_context = true,
	show_current_context_start = true,
})

--
-- File tree
local lib = require("nvim-tree.lib")
local view = require("nvim-tree.view")

local function collapse_all()
	require("nvim-tree.actions.tree-modifiers.collapse-all").fn()
end

local function edit_or_open()
	-- open as vsplit on current node
	local action = "edit"
	local node = lib.get_node_at_cursor()

	-- Just copy what's done normally with vsplit
	if node.link_to and not node.nodes then
		require("nvim-tree.actions.node.open-file").fn(action, node.link_to)
		view.close() -- Close the tree if file was opened
	elseif node.nodes ~= nil then
		lib.expand_or_collapse(node)
	else
		require("nvim-tree.actions.node.open-file").fn(action, node.absolute_path)
		view.close() -- Close the tree if file was opened
	end
end

local function vsplit_preview()
	-- open as vsplit on current node
	local action = "vsplit"
	local node = lib.get_node_at_cursor()

	-- Just copy what's done normally with vsplit
	if node.link_to and not node.nodes then
		require("nvim-tree.actions.node.open-file").fn(action, node.link_to)
	elseif node.nodes ~= nil then
		lib.expand_or_collapse(node)
	else
		require("nvim-tree.actions.node.open-file").fn(action, node.absolute_path)
	end

	-- Finally refocus on tree if it was lost
	view.focus()
end

local icons = require("schemar.icons").git
require("nvim-tree").setup({
	renderer = {
		icons = {
			git_placement = "after",
			glyphs = {
				git = {
					unstaged = icons.FileUnstaged,
					staged = icons.FileStaged,
					unmerged = icons.FileUnmerged,
					renamed = icons.FileRenamed,
					untracked = icons.FileUntracked,
					deleted = icons.FileDeleted,
					ignored = icons.FileIgnored,
				},
			},
		},
	},
	view = {
		mappings = {
			custom_only = false,
			list = {
				{ key = "l", action = "edit", action_cb = edit_or_open },
				{ key = "L", action = "vsplit_preview", action_cb = vsplit_preview },
				{ key = "h", action = "close_node" },
				{ key = "H", action = "collapse_all", action_cb = collapse_all },
			},
		},
	},
	actions = { open_file = { quit_on_open = false } },
})

--
-- Treesitter setup.
require("nvim-treesitter.configs").setup({
	-- A list of parser names, or "all"
	ensure_installed = {
		"bash",
		"css",
		"gitcommit",
		"html",
		"javascript",
		"json",
		"lua",
		"markdown",
		"markdown_inline",
		"typescript",
	},

	-- Install parsers synchronously (only applied to `ensure_installed`)
	sync_install = false,

	-- Automatically install missing parsers when entering buffer
	-- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
	auto_install = true,

	-- List of parsers to ignore installing (for "all")
	-- ignore_install = {  },

	---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
	-- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

	highlight = {
		-- `false` will disable the whole extension
		enable = true,

		-- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
		-- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
		-- the name of the parser)
		-- list of language that will be disabled
		-- disable = { },

		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = false,
	},

	rainbow = {
		enable = true,
		-- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
		extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
		max_file_lines = nil, -- Do not enable for files with more than n lines, int
		colors = { -- MUST be exactly seven
			"#bf616a",
			"#ebcb8b",
			"#b48ead",
			"#d08770",
			"#bf616a",
			"#ebcb8b",
			"#b48ead",
		},
		-- termcolors = {} -- table of colour name strings
	},

	autotag = {
		enable = true, -- Through auto-tag plugin
	},

	textobjects = {
		select = {
			enable = true,
			lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
		},
		move = {
			enable = true,
			set_jumps = true, -- whether to set jumps in the jumplist
		},
	},

	indent = { -- Indentation based on = operator (experimental)
		enable = true,
	},

	context_commentstring = { -- For nvim-ts-context-commentstring plugin
		enable = true,
		enable_autocmd = false, -- Disabled when used with Comment.nvim
	},
})
require("treesitter-context").setup({})

--
-- Nord theme
vim.g.nord_contrast = true -- Different background for sidebars and pop-ups (e.g. Telescope)
vim.g.nord_borders = true -- Borders between windows visible
vim.g.nord_disable_background = false -- When true, use terminal background instead of theme background
vim.g.nord_italic = true -- Italic highlights of text
vim.g.nord_uniform_diff_background = true -- Colorful backgrounds in diff mode
vim.g.nord_bold = false -- Bold highlights of text
-- Load the colorscheme
require("nord").set()

--
-- Headlines (compatible with Nord)
require("headlines").setup({
	markdown = {
		headline_highlights = {
			"Headline1",
			"Headline2",
			"Headline3",
			"Headline4",
			"Headline5",
			"Headline6",
		},
		codeblock_highlight = "CodeBlock",
		dash_highlight = "Dash",
		quote_highlight = "Quote",
	},
})

--
-- Modeline
local branch_max_length = 30
local function get_branch()
	require("lualine.components.branch.git_branch").init()
	local branch = require("lualine.components.branch.git_branch").get_branch()
	return string.sub(branch, math.max(string.len(branch) - branch_max_length, 0), string.len(branch))
end

require("lualine").setup({
	options = {
		icons_enabled = true,
		theme = "nord",
		component_separators = { left = "", right = "" },
		section_separators = { left = "", right = "" },
		disabled_filetypes = {
			statusline = {},
			winbar = { "NvimTree", "NeogitCommitMessage", "NeogitStatus" },
		},
		ignore_focus = {},
		always_divide_middle = true,
		globalstatus = true,
		refresh = { statusline = 1000, tabline = 1000, winbar = 1000 },
	},
	sections = {
		lualine_a = { "mode", "searchcount" },
		lualine_b = { get_branch, "diff" },
		lualine_c = { { "filename", path = 1, shorting_target = 70 } },
		lualine_x = { { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
		lualine_y = { "filetype" },
		lualine_z = { "location", "progress" },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = { "mode", "searchcount", get_branch, "diff" },
		lualine_c = { { "filename", path = 1, shorting_target = 70 } },
		lualine_x = { { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
		lualine_y = { "filetype", "locally", "progress" },
		lualine_z = {},
	},
	tabline = {},
	winbar = {
		lualine_a = {},
		lualine_b = { { "filetype", icon_only = true }, "filename" },
		lualine_c = {
			function()
				return require("nvim-navic").get_location()
			end,
		},
		lualine_x = {},
		lualine_y = {},
		lualine_z = {},
	},

	inactive_winbar = {
		lualine_a = {},
		lualine_b = { { "filetype", icon_only = true }, "filename" },
		lualine_c = {
			function()
				return require("nvim-navic").get_location()
			end,
		},
		lualine_x = {},
		lualine_y = {},
		lualine_z = {},
	},
	extensions = { "nvim-tree" },
})

--
-- Prettier plugin
local prettier = require("prettier")

prettier.setup({
	bin = "prettierd", -- or `'prettier'`
	filetypes = {
		"css",
		"graphql",
		"html",
		"javascript",
		"javascriptreact",
		"json",
		"less",
		"markdown",
		"scss",
		"typescript",
		"typescriptreact",
		"vue",
		"yaml",
	},
})

--
-- Fancy icons plugin
require("nvim-web-devicons").setup({
	-- globally enable different highlight colors per icon (default to true)
	-- if set to false all icons will have the default icon's color
	color_icons = true,
	-- globally enable default icons (default to false)
	-- will get overriden by `get_icons` option
	default = true,
})

--
-- Color highlighting
require("colorizer").setup()

--
-- Git gutter
require("gitsigns").setup()

--
-- Neogit
require("neogit").setup({})

--
-- Surround
require("nvim-surround").setup({})

--
-- Which Key
require("which-key").setup({})

--
-- Comment
require("Comment").setup({
	pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
})

--
-- Auto pairs
require("nvim-autopairs").setup()

--
-- Todo Comments
require("todo-comments").setup({
	highlight = {
		pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlightng (vim regex)
		-- Was [[.*<(KEYWORDS)\s*:]] including colon.
	},
	search = {
		pattern = [[\b(KEYWORDS)]], -- ripgrep regex
		-- Was [[\b(KEYWORDS):]] including colon.
	},
})

--
-- Trouble
require("trouble").setup({})

--
-- Telescope
local telescope = require("telescope")
local telescopeConfig = require("telescope.config")

-- Clone the default Telescope configuration
local vimgrep_arguments = { unpack(telescopeConfig.values.vimgrep_arguments) }

-- I want to search in hidden/dot files.
table.insert(vimgrep_arguments, "--hidden")
-- I don't want to search in the `.git` directory.
table.insert(vimgrep_arguments, "--glob")
table.insert(vimgrep_arguments, "!.git/*")

local trouble = require("trouble.providers.telescope")
telescope.setup({
	defaults = {
		-- `hidden = true` is not supported in text grep commands.
		vimgrep_arguments = vimgrep_arguments,
		mappings = {
			i = { ["<c-t>"] = trouble.open_with_trouble },
			n = { ["<c-t>"] = trouble.open_with_trouble },
		},
	},
	pickers = {
		find_files = {
			-- `hidden = true` will still show the inside of `.git/` as it's not `.gitignore`d.
			find_command = { "rg", "--files", "--hidden", "--glob", "!.git/*" },
		},
	},
	extensions = { file_browser = { hijack_netrw = true } },
})
telescope.load_extension("file_browser")

--
-- Go/Move
require("gomove").setup()

--
-- Refactoring
require("refactoring").setup({})

--
-- Other files
require("other-nvim").setup({
	-- Map files to other files.
	-- See documentation for details and more options.
	mappings = {
		{ pattern = "(.*)/(.*).ts", target = "%1/%2.vue", context = "vue" },
		{ pattern = "(.*)/(.*).vue", target = "%1/%2.ts", context = "vue" },
	},
})

--
-- Completion
require("schemar.plugins.cmp")

--
-- LSP
require("schemar.plugins.lsp")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("schemar.plugins.configs")

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
		extended_mode = true,
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
		theme = "catppuccin",
		component_separators = { left = "", right = "" },
		section_separators = { left = "", right = "" },
		disabled_filetypes = {
			statusline = {},
			winbar = { "NvimTree", "NeogitCommitMessage", "NeogitStatus", "aerial" },
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
	extensions = { "aerial", "nvim-tree" },
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
	extensions = {
		file_browser = { hijack_netrw = true },
		fzf = {
			fuzzy = true, -- false will only do exact matching
			override_generic_sorter = true, -- override the generic sorter
			override_file_sorter = true, -- override the file sorter
			case_mode = "smart_case", -- or "ignore_case" or "respect_case"
		},
	},
})
telescope.load_extension("file_browser")
telescope.load_extension("fzf")

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
		{ pattern = "(.*)/(.*).ts", target = "%1/%2.spec.ts", context = "spec" },
		{ pattern = "(.*)/(.*).spec.ts", target = "%1/%2.ts", context = "spec" },
		{ pattern = "(.*)/(.*).ts", target = "%1/%2.test.ts", context = "test" },
		{ pattern = "(.*)/(.*).test.ts", target = "%1/%2.ts", context = "test" },
	},
})

--
-- Completion
require("schemar.plugins.cmp")

--
-- LSP
require("schemar.plugins.lsp")

--
-- Borders
local _border = "single"

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = _border,
})

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = _border,
})

vim.diagnostic.config({
	float = { border = _border },
})

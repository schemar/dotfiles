return {
	{
		"williamboman/mason.nvim",
		cmd = "Mason",
		config = true,
	},
	{
		"jay-babu/mason-null-ls.nvim",
		lazy = true,
		dependencies = { "williamboman/mason.nvim" },
		-- Do not run setup here, will be done by none-ls.
	},
	{
		"nvimtools/none-ls.nvim",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"nvim-lua/plenary.nvim",
			"jay-babu/mason-null-ls.nvim",
		},
		config = function()
			local null_ls = require("null-ls")
			null_ls.setup({
				sources = {
					null_ls.builtins.diagnostics.ansiblelint,
					null_ls.builtins.diagnostics.yamllint,
					null_ls.builtins.diagnostics.zsh,
					-- [[ Formatters, etc. go here ]]
					-- !! Remember to add your formatters in formatter.lua
					null_ls.builtins.formatting.stylua,
					null_ls.builtins.formatting.prettier,
					null_ls.builtins.formatting.yamlfix,
				},
			})
			require("mason-null-ls").setup({
				-- [[ Formatters, without a null_ls setup go here ]]
				-- !! Remember to add your formatters in formatter.lua
				-- I install some formatters here rather than through null_ls, as mason
				-- does not automatically pick those up.
				ensure_installed = { "eslint_d", "jq" },
				automatic_installation = true,
			})
		end,
	},
	{
		"williamboman/mason-lspconfig.nvim",
		lazy = true,
		dependencies = {

			"williamboman/mason.nvim",
		},
		opts = {
			automatic_installation = true,
		},
	},
	{
		"neovim/nvim-lspconfig",
		event = { "BufReadPost", "BufNewFile", "BufWritePre" },
		dependencies = {
			"williamboman/mason-lspconfig.nvim",
			"b0o/schemastore.nvim",
		},
		config = function()
			local lspconfig = require("lspconfig")
			-- nvim-cmp:
			local capabilities = require("cmp_nvim_lsp").default_capabilities()

			-- [[ Language Servers go here ]]
			lspconfig.eslint.setup({})
			lspconfig.jsonls.setup({
				settings = {
					json = {
						schemas = require("schemastore").json.schemas(),
						validate = { enable = true },
					},
				},
			})
			lspconfig.lua_ls.setup({
				capabilities = capabilities,
			})
			-- TypeScript:
			lspconfig.vtsls.setup({})
			lspconfig.yamlls.setup({
				capabilities = {
					textDocument = {
						foldingRange = {
							-- Both required to not get an error with ufo:
							dynamicRegistration = false,
							lineFoldingOnly = true,
						},
					},
				},
			})
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		version = false, -- last release is way too old and doesn't work on Windows
		event = { "BufReadPost", "BufNewFile", "BufWritePre" },
		cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
		lazy = vim.fn.argc(-1) == 0, -- load treesitter early when opening a file from the cmdline
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")

			configs.setup({
				-- A list of parser names, or "all" (the five listed parsers should always be installed)
				ensure_installed = {
					"gdscript",
					"bash",
					"c",
					"c_sharp",
					"cmake",
					"comment",
					"csv",
					"diff",
					"dockerfile",
					"earthfile",
					"gdshader",
					"git_config",
					"git_rebase",
					"gitattributes",
					"gitcommit",
					"gitignore",
					"html",
					"http",
					"javascript",
					"jq",
					"jsdoc",
					"json",
					"json5",
					"lua",
					"lua",
					"luadoc",
					"make",
					"markdown_inline",
					"printf",
					"proto",
					"prql",
					"python",
					"query",
					"regex",
					"robot",
					"scss",
					"sql",
					"ssh_config",
					"tmux",
					"tsx",
					"typescript",
					"vim",
					"vimdoc",
					"vue",
					"xml",
					"yaml",
				},

				-- Install parsers synchronously (only applied to `ensure_installed`)
				sync_install = false,

				-- Automatically install missing parsers when entering buffer
				-- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
				auto_install = true,

				-- List of parsers to ignore installing (or "all")
				-- ignore_install = { "javascript" },

				---- If you need to change the installation directory of the parsers (see -> Advanced Setup)
				-- parser_install_dir = "/some/path/to/store/parsers", -- Remember to run vim.opt.runtimepath:append("/some/path/to/store/parsers")!

				-- [[ Modules ]]
				indent = { enable = true },
				highlight = {
					enable = true,

					-- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
					-- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
					-- the name of the parser)
					-- list of language that will be disabled
					-- disable = { "c", "rust" },
					-- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
					-- disable = function(lang, buf)
					-- 	local max_filesize = 100 * 1024 -- 100 KB
					-- 	local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
					-- 	if ok and stats and stats.size > max_filesize then
					-- 		return true
					-- 	end
					-- end,

					-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
					-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
					-- Using this option may slow down your editor, and you may see some duplicate highlights.
					-- Instead of true it can also be a list of languages
					additional_vim_regex_highlighting = false,
				},
			})
		end,
	},
	{
		"windwp/nvim-ts-autotag",
		opts = {
			opts = {
				enable_close = true, -- Auto close tags
				enable_rename = true, -- Auto rename pairs of tags
				enable_close_on_slash = true, -- Auto close on trailing </
			},
			-- Also override individual filetype configs, these take priority.
			-- Empty by default, useful if one of the "opts" global settings
			-- doesn't work well in a specific filetype
			-- per_filetype = {
			-- 	["html"] = {
			-- 		enable_close = false,
			-- 	},
			-- },
		},
	},
}

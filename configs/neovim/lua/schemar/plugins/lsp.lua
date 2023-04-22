local lsp_servers = {
	"ansiblels",
	"bashls",
	"cssls",
	"dockerls",
	"eslint",
	"html",
	"jsonls",
	"lua_ls",
	"tsserver",
}

-- LSP custom function when client attaches to buffer.
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
	local format_timeout_ms = 2000
	local lsp_format = require("lsp-format")
	local navic = require("nvim-navic")
	-- Auto-format on save
	lsp_format.on_attach(client)

	-- Attach navic for winbar breadcrumbs
	if client.server_capabilities.documentSymbolProvider then
		navic.attach(client, bufnr)
	end

	-- Mappings.
	-- See `:help vim.lsp.*` for documentation on any of the below functions
	local wk = require("which-key")

	-- Without prefix:
	wk.register({
		["["] = {
			d = {
				vim.lsp.diagnostic.goto_prev,
				"Previous diagnostic",
				buffer = bufnr,
			},
		},
		["]"] = {
			d = {
				vim.lsp.diagnostic.goto_next,
				"Next diagnostic",
				buffer = bufnr,
			},
		},
		g = {
			d = {
				":Trouble lsp_definitions<CR>",
				"List definitions",
				buffer = bufnr,
			},
			i = {
				":Trouble lsp_implementations<CR>",
				"List implementations",
				buffer = bufnr,
			},
			r = {
				":Trouble lsp_references<CR>",
				"List references",
				buffer = bufnr,
			},
		},
		K = { vim.lsp.buf.hover, "Hover doc", buffer = bufnr },
	})

	-- With localleader prefix:
	wk.register({
		a = {
			function()
				vim.lsp.buf.code_action({ apply = true })
			end,
			"Code actions",
			buffer = bufnr,
		},
		d = { ":Trouble document_diagnostics<CR>", "Show diagnostics" },
		D = {
			":Trouble workspace_diagnostics<CR>",
			"Show workspace diagnostics",
		},
		e = { vim.diagnostic.open_float, "Floating diagnostic" },
		f = {
			function()
				vim.lsp.buf.format({ async = true, timeout_ms = format_timeout_ms })
			end,
			"Format buffer",
			buffer = bufnr,
		},
		k = { vim.lsp.buf.signature_help, "Signature help", buffer = bufnr },
		r = { vim.lsp.buf.rename, "Rename", buffer = bufnr },
		t = { vim.lsp.buf.type_definition, "Go to type definition", buffer = bufnr },
		["<tab>"] = {
			name = "Workspace",
			a = {
				vim.lsp.buf.add_workspace_folder,
				"Add workspace folder",
				buffer = bufnr,
			},
			l = {
				function()
					print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
				end,
				"List workspace folders",
				buffer = bufnr,
			},
			r = {
				vim.lsp.buf.remove_workspace_folder,
				"Remove workspace folder",
				buffer = bufnr,
			},
		},
	}, { prefix = "<localleader>" })

	-- Visual mode with localleader prefix:
	wk.register({
		a = {
			function()
				vim.lsp.buf.code_action({ apply = true })
			end,
			"Code actions",
			buffer = bufnr,
		},
	}, { prefix = "<localleader>", mode = "v" })

	-- Server specific keymaps:
	if client.name == "eslint" then
		wk.register({ F = { ":EslintFixAll<CR>", "ESLint fix all", buffer = bufnr } }, { prefix = "<localleader>" })
	end
end

return {
	{
		"neovim/nvim-lspconfig", -- Configurations for Nvim LSP
		event = { "BufReadPost", "BufNewFile" },
		dependencies = {
			"b0o/schemastore.nvim", -- Schemas for JSON files
			"jose-elias-alvarez/typescript.nvim",
			"hrsh7th/cmp-nvim-lsp", -- See cmp.lua for more info
			{
				"williamboman/mason.nvim", -- Manage language servers, linters, etc.
				-- IMPORTANT: Mason must be set up before lspconfig and null-ls
				cmd = { "Mason" },
				priority = 500,
				config = function()
					-- Mason to manage external tools like language servers
					require("mason").setup()
					require("mason-lspconfig").setup({
						ensure_installed = lsp_servers,
						automatic_installation = true,
					})
				end,
			},
			{
				"williamboman/mason-lspconfig.nvim", -- Integration mason/lsp
			},
			{
				"jayp0521/mason-null-ls.nvim", -- Integration mason/null-ls
				config = function()
					-- Mason Null-ls handles the installation of the configured sources
					require("mason-null-ls").setup({
						ensure_installed = nil, -- nil, as taken from null_ls setup
						automatic_installation = true,
						automatic_setup = true,
					})
				end,
			},
			{
				"lukas-reineke/lsp-format.nvim", -- Easier management of auto-saving from LSP sources
				opts = {
					typescript = {
						-- Prettier overrides LSP and ESLint
						order = { "tsserver", "eslint", "null-ls" },
					},
					lua = {
						-- StyLua overrides LSP
						order = { "lua_ls", "null-ls" },
					},
				},
			},
			{
				"SmiteshP/nvim-navic", -- Winbar breadcrumbs, e.g. for code context
				config = function()
					-- Navic shows breadcrumbs in the buffer line
					local navic = require("nvim-navic")
					local icons = require("schemar.icons").kind
					navic.setup({
						icons = {
							Array = icons.Array .. " ",
							Boolean = icons.Boolean,
							Class = icons.Class .. " ",
							Color = icons.Color .. " ",
							Constant = icons.Constant .. " ",
							Constructor = icons.Constructor .. " ",
							Enum = icons.Enum .. " ",
							EnumMember = icons.EnumMember .. " ",
							Event = icons.Event .. " ",
							Field = icons.Field .. " ",
							File = icons.File .. " ",
							Folder = icons.Folder .. " ",
							Function = icons.Function .. " ",
							Interface = icons.Interface .. " ",
							Key = icons.Key .. " ",
							Keyword = icons.Keyword .. " ",
							Method = icons.Method .. " ",
							Module = icons.Module .. " ",
							Namespace = icons.Namespace .. " ",
							Null = icons.Null .. " ",
							Number = icons.Number .. " ",
							Object = icons.Object .. " ",
							Operator = icons.Operator .. " ",
							Package = icons.Package .. " ",
							Property = icons.Property .. " ",
							Reference = icons.Reference .. " ",
							Snippet = icons.Snippet .. " ",
							String = icons.String .. " ",
							Struct = icons.Struct .. " ",
							Text = icons.Text .. " ",
							TypeParameter = icons.TypeParameter .. " ",
							Unit = icons.Unit .. " ",
							Value = icons.Value .. " ",
							Variable = icons.Variable .. " ",
						},
						highlight = true, -- Required for catppuccin theme
						separator = " " .. require("schemar.icons").ui.ChevronShortRight .. " ",
						depth_limit = 0,
						depth_limit_indicator = "..",
						safe_output = true,
					})
				end,
			},
		},
		config = function()
			local lsp_flags = {
				-- This is the default in Nvim 0.7+
				debounce_text_changes = 150,
			}

			-- Add additional capabilities supported by nvim-cmp
			local capabilities = require("cmp_nvim_lsp").default_capabilities()
			local lspconfig = require("lspconfig")

			-- Enable some language servers with the additional completion capabilities offered by nvim-cmp
			for _, lsp in ipairs(lsp_servers) do
				if lsp ~= "tsserver" then -- tsserver is managed by "typescript" plugin below
					lspconfig[lsp].setup({
						on_attach = on_attach,
						flags = lsp_flags,
						capabilities = capabilities,
						settings = {
							json = {
								schemas = require("schemastore").json.schemas(),
								validate = { enable = true },
							},
							Lua = {
								diagnostics = {
									-- Get the language server to recognize the `vim` global
									globals = { "vim" },
								},
							},
						},
					})
				end
			end

			require("typescript").setup({
				disable_commands = false, -- prevent the plugin from creating Vim commands
				debug = false, -- enable debug logging for commands
				go_to_source_definition = {
					fallback = true, -- fall back to standard LSP definition on failure
				},
				server = {
					-- pass options to lspconfig's setup method
					on_attach = on_attach,
					flags = lsp_flags,
					capabilities = capabilities,
				},
			})
		end,
	},
	{
		"jose-elias-alvarez/null-ls.nvim", -- NeoVim as LSP server
		event = { "BufReadPost", "BufNewFile" },
		dependencies = {
			"jose-elias-alvarez/typescript.nvim",
		},
		config = function()
			local null_ls = require("null-ls")
			local null_ls_sources = {
				null_ls.builtins.diagnostics.gitlint.with({
					filetypes = { "gitcommit", "NeogitCommitMessage" },
				}),
				null_ls.builtins.diagnostics.hadolint, -- Docker best practices
				null_ls.builtins.diagnostics.shellcheck,
				null_ls.builtins.formatting.jq,
				null_ls.builtins.formatting.shfmt, -- Shell
				null_ls.builtins.formatting.stylua,
				null_ls.builtins.formatting.yamlfmt,
				require("typescript.extensions.null-ls.code-actions"),
			}
			--
			-- NeoVim LSP server capabilities
			null_ls.setup({
				sources = null_ls_sources,
				on_attach = on_attach,
			})
		end,
	},
}

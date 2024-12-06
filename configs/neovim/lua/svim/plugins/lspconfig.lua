return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    config = true,
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
      "b0o/schemastore.nvim",
      "williamboman/mason-lspconfig.nvim",
      "yioneko/nvim-vtsls",
      "j-hui/fidget.nvim",
    },
    config = function()
      local lspconfig = require("lspconfig")
      -- format:
      local on_attach = function(client, bufnr)
        require("lsp-format").on_attach(client, bufnr)
      end

      -- nvim-cmp:
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      -- Extend for lua:
      local capabilities_lua = {}
      -- Copy capabilities table
      for k, v in pairs(capabilities) do
        capabilities_lua[k] = v
      end
      -- And add lua specifics
      capabilities_lua.textDocument.foldingRange = {
        -- Both required to not get an error with ufo:
        dynamicRegistration = false,
        lineFoldingOnly = true,
      }

      -- [[ Language Servers go here ]]
      lspconfig.bashls.setup({
        -- bashls includes shellcheck and shfmt
        capabilities = capabilities,
        on_attach = on_attach,
      })
      lspconfig.cssls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
      })
      lspconfig.efm.setup({
        -- See also separate efm config in repo (outside nvim).
        capabilities = capabilities,
        on_attach = on_attach,
        init_options = { documentFormatting = true },
        filetypes = {
          "gdscript",
          "html",
          "javascript",
          "javascript.jsx",
          "javascriptreact",
          "json",
          "lua",
          "markdown",
          "scss",
          "typescript",
          "typescript.tsx",
          "typescriptreact",
          "vue",
        },
      })
      lspconfig.eslint.setup({
        capabilities = capabilities,
        on_attach = on_attach,
      })
      lspconfig.jsonls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
        settings = {
          json = {
            schemas = require("schemastore").json.schemas(),
            validate = { enable = true },
          },
        },
      })
      lspconfig.html.setup({
        capabilities = capabilities,
        -- No formatting with HTML (confuses afilio).
        -- on_attach = on_attach,
        filetypes = { "html", "templ", "vue" },
      })
      lspconfig.lua_ls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
      })
      -- TypeScript:
      lspconfig.vtsls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
      })
      lspconfig.yamlls.setup({
        capabilities = capabilities_lua,
        on_attach = on_attach,
        settings = {
          yaml = {
            schemaStore = {
              -- You must disable built-in schemaStore support if you want to use
              -- this plugin and its advanced options like `ignore`.
              enable = false,
              -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
              url = "",
            },
            schemas = require("schemastore").yaml.schemas(),
          },
        },
      })
      lspconfig.gdscript.setup({
        capabilities = capabilities,
        on_attach = on_attach,
      })
      lspconfig.gdshader_lsp.setup({
        capabilities = capabilities,
        on_attach = on_attach,
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
          "bash",
          "c",
          "c_sharp",
          "cmake",
          "comment",
          "csv",
          "diff",
          "dockerfile",
          "earthfile",
          "gdscript",
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
    "nvim-treesitter/nvim-treesitter-context",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = true,
  },
  {
    "windwp/nvim-ts-autotag",
    event = { "BufRead", "BufNewFile" },
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
  {
    "j-hui/fidget.nvim",
    lazy = true,
    opts = {
      notification = {
        window = {
          winblend = 0,
        },
      },
      -- ... the rest of your fidget config
    },
  },
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {},
  },
}

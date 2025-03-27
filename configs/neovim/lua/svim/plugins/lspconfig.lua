local icons = require("svim.config").icons.diagnostics

-- [[LSP Settings]]
vim.diagnostic.config({
  virtual_lines = true,
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = icons.Error,
      [vim.diagnostic.severity.WARN] = icons.Warn,
      [vim.diagnostic.severity.INFO] = icons.Info,
      [vim.diagnostic.severity.HINT] = icons.Hint,
    },
    numhl = {
      [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
      [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
      [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
      [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
    },
  },
})

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
      "saghen/blink.cmp",
      "williamboman/mason-lspconfig.nvim",
      "yioneko/nvim-vtsls",
    },
    config = function()
      local lspconfig = require("lspconfig")
      -- format:
      local on_attach = function(client, bufnr)
        require("lsp-format").on_attach(client, bufnr)
      end

      -- blink.cmp:
      local capabilities = require("blink.cmp").get_lsp_capabilities()

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
      lspconfig.zls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
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
        textobjects = {
          select = {
            enable = true,

            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,

            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["am"] = "@function.outer",
              ["im"] = "@function.inner",
              ["ao"] = "@loop.outer",
              ["io"] = "@loop.inner",
              ["ac"] = "@class.outer",
              -- You can optionally set descriptions to the mappings (used in the desc parameter of
              -- nvim_buf_set_keymap) which plugins like which-key display
              ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
              -- You can also use captures from other query groups like `locals.scm`
              ["as"] = {
                query = "@local.scope",
                query_group = "locals",
                desc = "Select language scope",
              },
            },
            -- You can choose the select mode (default is charwise 'v')
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * method: eg 'v' or 'o'
            -- and should return the mode ('v', 'V', or '<c-v>') or a table
            -- mapping query_strings to modes.
            selection_modes = {
              ["@parameter.outer"] = "v", -- charwise
              ["@function.outer"] = "V", -- linewise
              ["@class.outer"] = "<c-v>", -- blockwise
            },
            -- If you set this to `true` (default is `false`) then any textobject is
            -- extended to include preceding or succeeding whitespace. Succeeding
            -- whitespace has priority in order to act similarly to eg the built-in
            -- `ap`.
            --
            -- Can also be a function which gets passed a table with the keys
            -- * query_string: eg '@function.inner'
            -- * selection_mode: eg 'v'
            -- and should return true or false
            include_surrounding_whitespace = false,
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              ["]m"] = "@function.outer",
              --
              -- You can use regex matching (i.e. lua pattern) and/or pass a list in a "query" key to group multiple queries.
              ["]o"] = "@loop.*",
              -- ["]o"] = { query = { "@loop.inner", "@loop.outer" } }
              --
              -- You can pass a query group to use query from `queries/<lang>/<query_group>.scm file in your runtime path.
              -- Below example nvim-treesitter's `locals.scm` and `folds.scm`. They also provide highlights.scm and indent.scm.
              ["]s"] = { query = "@local.scope", query_group = "locals", desc = "Next scope" },
              ["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[o"] = "@loop.*",
              ["[s"] = { query = "@local.scope", query_group = "locals", desc = "Next scope" },
              ["[z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
            },
            -- Below will go to either the start or the end, whichever is closer.
            -- Use if you want more granular movements
            -- Make it even more gradual by adding multiple queries and regex.
            goto_next = {
              ["]l"] = "@conditional.outer",
            },
            goto_previous = {
              ["[l"] = "@conditional.outer",
            },
          },
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
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
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
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {},
  },
}

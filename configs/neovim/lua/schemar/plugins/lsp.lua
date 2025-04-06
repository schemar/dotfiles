-- format:
local on_attach = function(client, bufnr)
  require("lsp-format").on_attach(client, bufnr)
end
-- blink.cmp:
local capabilities = function()
  return require("blink.cmp").get_lsp_capabilities()
end

return {
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    opts = {},
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
    "jay-babu/mason-null-ls.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "nvimtools/none-ls.nvim",
      "nvim-lua/plenary.nvim",
      "saghen/blink.cmp",
      "lukas-reineke/lsp-format.nvim",
    },
    config = function()
      local null_ls = require("null-ls")

      null_ls.setup({
        sources = {
          null_ls.builtins.formatting.prettier,
          null_ls.builtins.diagnostics.stylelint.with({
            extra_filetypes = { "vue" },
          }),
          null_ls.builtins.formatting.stylelint.with({
            extra_filetypes = { "vue" },
          }),
          null_ls.builtins.formatting.stylua,
          null_ls.builtins.formatting.shfmt,
          null_ls.builtins.diagnostics.gdlint,
          null_ls.builtins.formatting.gdformat,
        },
        on_attach = on_attach,
        capabilities = capabilities(),
      })

      require("mason-null-ls").setup({
        ensure_installed = nil,
        -- Should install everything as set up above with null_ls/none-ls:
        automatic_installation = true,
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPost", "BufNewFile", "BufWritePre" },
    dependencies = {
      "b0o/schemastore.nvim",
      "saghen/blink.cmp",
      "williamboman/mason-lspconfig.nvim",
      "yioneko/nvim-vtsls",
      "lukas-reineke/lsp-format.nvim",
    },
    config = function()
      local lspconfig = require("lspconfig")

      -- Extend for lua:
      local capabilities_lua = {}
      -- Copy capabilities table
      for k, v in pairs(capabilities()) do
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
        capabilities = capabilities(),
        on_attach = on_attach,
      })
      lspconfig.cssls.setup({
        capabilities = capabilities(),
        on_attach = on_attach,
      })
      lspconfig.eslint.setup({
        capabilities = capabilities(),
        on_attach = on_attach,
      })
      lspconfig.jsonls.setup({
        capabilities = capabilities(),
        on_attach = on_attach,
        settings = {
          json = {
            schemas = require("schemastore").json.schemas(),
            validate = { enable = true },
          },
        },
      })
      lspconfig.html.setup({
        capabilities = capabilities(),
        -- No formatting with HTML (confuses afilio).
        -- on_attach = on_attach,
        filetypes = { "html", "templ", "vue" },
      })
      lspconfig.lua_ls.setup({
        capabilities = capabilities(),
        on_attach = on_attach,
      })
      -- TypeScript:
      lspconfig.vtsls.setup({
        capabilities = capabilities(),
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
        capabilities = capabilities(),
        on_attach = on_attach,
      })
      lspconfig.gdscript.setup({
        capabilities = capabilities(),
        on_attach = on_attach,
      })
      lspconfig.gdshader_lsp.setup({
        capabilities = capabilities(),
        on_attach = on_attach,
      })
    end,
  },
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {},
  },
}

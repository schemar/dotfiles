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
    event = "VeryLazy",
    cmd = "Mason",
    -- Extended by plugins/languages/
    opts = {
      ensure_installed = {},
    },
    config = function(_, opts)
      require("mason").setup(opts)

      local mason_registry = require("mason-registry")
      for _, package_name in ipairs(opts.ensure_installed) do
        local ok, pkg = pcall(mason_registry.get_package, package_name)
        if ok then
          if not pkg:is_installed() then
            vim.notify(
              "Installing " .. package_name .. " with Mason",
              vim.log.levels.INFO,
              { title = "Mason" }
            )
            pkg:install() -- Install the package if not already installed
          end
        else
          print("Package not found: " .. package_name)
        end
      end
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = "VeryLazy",
    dependencies = {
      "williamboman/mason.nvim",
    },
    -- Extended by plugins/languages/
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
    end,
  },
  {
    "nvimtools/none-ls.nvim",
    event = "VeryLazy",
    dependencies = { "nvim-lua/plenary.nvim" },
    -- Extended by plugins/languages/
    opts = { sources = {} },
    config = function(_, opts)
      local null_ls = require("null-ls")
      opts.on_attach = on_attach

      null_ls.setup(opts)
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    dependencies = {
      "b0o/schemastore.nvim",
      "saghen/blink.cmp",
      "williamboman/mason-lspconfig.nvim",
      "yioneko/nvim-vtsls",
      "lukas-reineke/lsp-format.nvim",
    },
    -- Extended by plugins/languages/
    opts = { servers = {} },
    config = function()
      local lspconfig = require("lspconfig")

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
        capabilities = capabilities(),
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

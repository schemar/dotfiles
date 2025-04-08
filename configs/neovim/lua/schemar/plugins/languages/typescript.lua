return {
  {
    "neovim/nvim-lspconfig",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      local lspconfig = require("lspconfig")
      opts.servers = opts.servers or {}
      opts.servers = vim.tbl_extend("force", opts.servers, {
        vtsls = {
          settings = {
            vtsls = {
              autoUseWorkspaceTsdk = true, -- Ensures workspace TypeScript SDK is used
            },
          },
        },
      })
    end,
  },
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      typescript = {
        -- null-ls is prettier
        order = { "eslint", "null-ls" },
        sync = true,
        -- Format only with eslint and prettier.
        -- Prevents back and forth formatting.
        exclude = { "vtsls" },
      },
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      table.insert(opts.ensure_installed, "vtsls")
    end,
  },
}

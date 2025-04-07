return {
  {
    "neovim/nvim-lspconfig",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.servers = opts.servers or {}
      table.insert(opts.servers, "vtsls")
    end,
  },
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      typescript = {
        -- null-ls is prettier
        order = { "vtsls", "eslint", "null-ls" },
        sync = true,
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

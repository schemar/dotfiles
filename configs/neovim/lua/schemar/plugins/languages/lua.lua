return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      lua = {
        order = { "lua_ls", "null-ls" },
        sync = true,
      },
    },
  },
  {
    "nvimtools/none-ls.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      local null_ls = require("null-ls")

      opts.sources = opts.sources or {}
      table.insert(opts.sources, null_ls.builtins.formatting.stylua)
    end,
  },
  {
    "williamboman/mason.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}

      table.insert(opts.ensure_installed, "stylua")
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      table.insert(opts.ensure_installed, "lua_ls")
    end,
  },
}

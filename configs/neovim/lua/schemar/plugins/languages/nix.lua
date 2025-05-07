return {
  {
    "neovim/nvim-lspconfig",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.servers = opts.servers or {}
      table.insert(opts.servers, "nil_ls")
    end,
  },
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      nix = {
        order = { "null-ls" },
        sync = true,
      },
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      table.insert(opts.ensure_installed, "nil_ls")
    end,
  },
  {
    "nvimtools/none-ls.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      local null_ls = require("null-ls")

      opts.sources = opts.sources or {}
      -- Executable installed by nix:
      table.insert(opts.sources, null_ls.builtins.formatting.nixfmt)
    end,
  },
}

return {
  {
    "neovim/nvim-lspconfig",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.servers = opts.servers or {}
      table.insert(opts.servers, "gdscript")
      table.insert(opts.servers, "gdshader_lsp")
    end,
  },
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      gdscript = {
        order = { "null-ls" },
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
      table.insert(opts.sources, null_ls.builtins.diagnostics.gdlint)
      table.insert(opts.sources, null_ls.builtins.formatting.gdformat)
    end,
  },
  {
    "williamboman/mason.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}

      table.insert(opts.ensure_installed, "gdtoolkit")
    end,
  },
}

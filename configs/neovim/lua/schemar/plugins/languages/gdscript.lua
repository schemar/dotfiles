return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      gdscript = { "null-ls" },
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

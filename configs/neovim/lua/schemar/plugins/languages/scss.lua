return {
  "lukas-reineke/lsp-format.nvim",
  -- Extends plugins/format.lua
  opts = {
    scss = {
      order = { "cssls", "null-ls" },
      sync = true,
    },
  },
  {
    "nvimtools/none-ls.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      local null_ls = require("null-ls")

      opts.sources = opts.sources or {}
      table.insert(
        opts.sources,
        null_ls.builtins.diagnostics.stylelint.with({
          extra_filetypes = { "vue" },
        })
      )
      table.insert(
        opts.sources,
        null_ls.builtins.formatting.stylelint.with({
          extra_filetypes = { "vue" },
        })
      )
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}

      table.insert(opts.ensure_installed, "stylelint_lsp")
    end,
  },
}

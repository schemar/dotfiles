return {
  {
    "nvimtools/none-ls.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      local null_ls = require("null-ls")

      opts.sources = opts.sources or {}
      table.insert(
        opts.sources,
        null_ls.builtins.formatting.prettier.with({
          -- Always format open files, even if they are ignored.
          -- Setting `ignore-path` to empty string means "ignore nothing".
          -- By default, prettier will ignore files in .gitignore and .prettierignore.
          extra_args = { "--ignore-path", "" },
        })
      )
    end,
  },
  {
    "williamboman/mason.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}

      table.insert(opts.ensure_installed, "prettier")
    end,
  },
}

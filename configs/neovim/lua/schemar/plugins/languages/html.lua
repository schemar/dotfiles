return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      html = {
        order = { "html", "null-ls" },
        sync = true,
      },
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      table.insert(opts.ensure_installed, "html")
    end,
  },
}

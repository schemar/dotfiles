return {
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      yaml = { "yamlfix", "null-ls" },
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      table.insert(opts.ensure_installed, "yamlls")
    end,
  },
}

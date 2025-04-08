return {
  {
    "neovim/nvim-lspconfig",
    -- Extends plugins/lsp.lua
    opts = function(_, opts)
      opts.servers = opts.servers or {}
      opts.servers = vim.tbl_extend("force", opts.servers, {
        html = {
          filetypes = { "html", "templ", "vue" },
        },
      })
    end,
  },
  {
    "lukas-reineke/lsp-format.nvim",
    -- Extends plugins/format.lua
    opts = {
      html = {
        order = { "null-ls" },
        sync = true,
        -- Format only with prettier.
        -- Prevents back and forth formatting.
        exclude = { "html" }, -- Exclude html from formatting
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

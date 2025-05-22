local function make_lsp_capabilities()
  local capabilities = require("blink.cmp").get_lsp_capabilities() -- completion with blink.cmp

  -- See nvim-ufo setup
  -- Tell the server the capability of foldingRange,
  -- Neovim hasn't added foldingRange to default capabilities, users must add it manually
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
  }

  return capabilities
end

return {
  {
    "williamboman/mason.nvim",
    event = "VeryLazy",
    cmd = "Mason",
    -- Extended by plugins/languages/
    opts = {
      ensure_installed = {},
    },
    config = function(_, opts)
      require("mason").setup(opts)

      local mason_registry = require("mason-registry")
      for _, package_name in ipairs(opts.ensure_installed) do
        local ok, pkg = pcall(mason_registry.get_package, package_name)
        if ok then
          if not pkg:is_installed() then
            vim.notify(
              "Installing " .. package_name .. " with Mason",
              vim.log.levels.INFO,
              { title = "Mason" }
            )
            pkg:install() -- Install the package if not already installed
          end
        else
          print("Package not found: " .. package_name)
        end
      end
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = "VeryLazy",
    dependencies = {
      "williamboman/mason.nvim",
    },
    -- Extended by plugins/languages/
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      opts.automatic_enable = false
    end,
  },
  {
    "nvimtools/none-ls.nvim",
    event = "VeryLazy",
    dependencies = { "nvim-lua/plenary.nvim" },
    -- Extended by plugins/languages/
    opts = { sources = {} },
    config = function(_, opts)
      local null_ls = require("null-ls")
      opts.on_attach = require("lsp-format").on_attach -- formatting

      null_ls.setup(opts)
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = "VeryLazy",
    dependencies = {
      "b0o/schemastore.nvim",
      "saghen/blink.cmp",
      "williamboman/mason-lspconfig.nvim",
      "yioneko/nvim-vtsls",
      "lukas-reineke/lsp-format.nvim",
    },
    -- Extended by plugins/languages/
    opts = function(_, opts)
      opts.servers = opts.servers or {}
    end,
    config = function(_, opts)
      local lspconfig = require("lspconfig")
      local on_attach = require("lsp-format").on_attach -- formatting
      local capabilities = make_lsp_capabilities()

      for server, server_opts in pairs(opts.servers) do
        -- Was either done as tbl_extend with "cssls" in which case the server
        -- is the second value of ipairs, or as {cssls = { setup = {...}}}
        if type(server_opts) == "string" then
          server = server_opts
          server_opts = {}
        end

        lspconfig[server].setup(vim.tbl_deep_extend("force", {
          capabilities = capabilities,
          on_attach = on_attach,
        }, server_opts))
      end
    end,
  },
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {},
  },
  {
    -- Try out hover.nvim as a replacement, as the default hover started to
    -- crop the text/view.
    "lewis6991/hover.nvim",
    event = "VeryLazy",
    opts = function()
      local border = require("schemar.config.options").border

      return {
        init = function()
          -- Require providers
          require("hover.providers.lsp")
          require("hover.providers.gh")
          require("hover.providers.gh_user")
          -- require('hover.providers.jira')
          -- require('hover.providers.dap')
          require("hover.providers.fold_preview")
          require("hover.providers.diagnostic")
          require("hover.providers.man")
          -- require('hover.providers.dictionary')
          -- require("hover.providers.highlight")
        end,
        preview_opts = {
          border = border,
        },
        -- Whether the contents of a currently open hover window should be moved
        -- to a :h preview-window when pressing the hover keymap.
        preview_window = false,
        title = true,
        mouse_providers = {},
      }
    end,
  },
}

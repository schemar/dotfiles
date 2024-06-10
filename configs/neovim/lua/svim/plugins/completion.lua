return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "nvim-lua/plenary.nvim", -- for petertriho/cmp-git
      "neovim/nvim-lspconfig",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lsp-document-symbol",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-nvim-lua",
      "petertriho/cmp-git",
      "David-Kunz/cmp-npm",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    event = "VeryLazy",
    config = function()
      -- Set up nvim-cmp.
      local cmp = require("cmp")

      cmp.setup({
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
            require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
            -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
            -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
            -- vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
          end,
        },
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-j>"] = cmp.mapping(function()
            cmp.select_next_item()
          end),
          ["<C-k>"] = cmp.mapping(function()
            cmp.select_prev_item()
          end),
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          -- { name = "vsnip" }, -- For vsnip users.
          { name = "luasnip" }, -- For luasnip users.
          -- { name = 'ultisnips' }, -- For ultisnips users.
          -- { name = 'snippy' }, -- For snippy users.
          { name = "buffer" },
          { name = "path" },
          { name = "nvim_lsp_document_symbol" },
          { name = "nvim_lsp_signature_help" },
          { name = "nvim_lua" },
          { name = "npm", keyword_length = 4 },
        }, {
          { name = "buffer" },
        }),
      })

      -- To use git you need to install the plugin petertriho/cmp-git and uncomment lines below
      -- Set configuration for specific filetype.
      cmp.setup.filetype("gitcommit", {
        sources = cmp.config.sources({
          { name = "git" },
        }, {
          { name = "buffer" },
        }),
      })
      require("cmp_git").setup()

      -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline({
          ["<C-j>"] = {
            c = function()
              cmp.select_next_item()
            end,
          },
          ["<C-k>"] = {
            c = function()
              cmp.select_next_item()
            end,
          },
        }),
        sources = {
          { name = "buffer" },
        },
      })

      -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline({
          ["<C-j>"] = {
            c = function()
              cmp.select_next_item()
            end,
          },
          ["<C-k>"] = {
            c = function()
              cmp.select_next_item()
            end,
          },
        }),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
        matching = { disallow_symbol_nonprefix_matching = false },
      })
    end,
  },
  {
    {
      "L3MON4D3/LuaSnip",
      -- follow latest release.
      version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
      -- install jsregexp (optional!).
      build = "make install_jsregexp",
      lazy = true,
    },
  },
  {
    "David-Kunz/cmp-npm",
    lazy = true,
    dependencies = { "nvim-lua/plenary.nvim" },
    ft = "json",
    config = true,
  },
}

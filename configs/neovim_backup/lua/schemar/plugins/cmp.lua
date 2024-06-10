return {
  "hrsh7th/nvim-cmp",
  event = { "VeryLazy" },
  dependencies = {
    "hrsh7th/cmp-nvim-lsp", -- LSP source for nvim-cmp
    "hrsh7th/cmp-nvim-lsp-signature-help", -- Function signature source for nvim-cmp
    "hrsh7th/cmp-buffer", -- Buffer source for nvim-cmp
    "hrsh7th/cmp-path", -- Path source for nvim-cmp
    "hrsh7th/cmp-cmdline", -- Command line source for nvim-cmp
    {
      "L3MON4D3/LuaSnip", -- Snippets plugin
      dependencies = {
        "rafamadriz/friendly-snippets",
      },
      config = function()
        -- Load "friendly-snippets" (dependency):
        require("luasnip.loaders.from_vscode").lazy_load()
      end,
    },
    "saadparwaiz1/cmp_luasnip", -- Snippets source for nvim-cmp
    { "petertriho/cmp-git", dependencies = "nvim-lua/plenary.nvim" },
  },
  config = function()
    local cmp = require("cmp")
    local luasnip = require("luasnip")
    local cmp_git = require("cmp_git")

    cmp.setup({
      snippet = {
        -- REQUIRED - you must specify a snippet engine
        expand = function(args)
          luasnip.lsp_expand(args.body) -- For `luasnip` users.
        end,
      },
      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
      },
      mapping = cmp.mapping.preset.insert({
        -- Use <C-j/k> to select candidates:
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
        ["<CR>"] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = false, -- Selection required to complete on "Enter".
        }),
        -- For (S-)Tab, prefer snippet over completion.
        -- (Prefer C-j/k for completion anyway)
        ["<Tab>"] = cmp.mapping(function(fallback)
          if luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          elseif cmp.visible() then
            cmp.select_next_item()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if luasnip.jumpable(-1) then
            luasnip.jump(-1)
          elseif cmp.visible() then
            cmp.select_prev_item()
          else
            fallback()
          end
        end, { "i", "s" }),
      }),
      sources = cmp.config.sources({
        -- Output will be prioritized according to order.
        { name = "copilot", group_index = 2 },
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "buffer" },
        { name = "luasnip" },
        { name = "nvim_lsp_signature_help" },
      }, {
        { name = "buffer" },
      }),
    })

    cmp.setup.filetype("gitcommit", {
      sources = cmp.config.sources({
        { name = "git" }, -- You can specify the `git` source if [you were installed it](https://github.com/petertriho/cmp-git).
      }, {
        { name = "buffer" },
      }),
    })
    cmp_git.setup()

    -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline({ "/", "?" }, {
      mapping = cmp.mapping.preset.cmdline({
        -- Use <C-j/k> to select candidates:
        ["<C-j>"] = {
          c = function()
            cmp.select_next_item()
          end,
        },
        ["<C-k>"] = {
          c = function()
            cmp.select_prev_item()
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
        -- Use <C-j/k> to select candidates:
        ["<C-j>"] = {
          c = function()
            cmp.select_next_item()
          end,
        },
        ["<C-k>"] = {
          c = function()
            cmp.select_prev_item()
          end,
        },
      }),
      sources = cmp.config.sources({
        { name = "path" },
      }, {
        { name = "cmdline" },
      }),
    })
  end,
}

return {
  { "github/copilot.vim" },
  {
    "saghen/blink.cmp",
    dependencies = {
      {
        "Kaiser-Yang/blink-cmp-git",
        dependencies = { "nvim-lua/plenary.nvim" },
      },
      -- optional: provides snippets for the snippet source
      "rafamadriz/friendly-snippets",
    },

    -- use a release tag to download pre-built binaries
    version = "*",
    -- AND/OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
    -- build = 'cargo build --release',
    -- If you use nix, you can build from source using latest nightly rust with:
    -- build = 'nix run .#build-plugin',

    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      -- For details, see:
      -- 1. Recipes: https://cmp.saghen.dev/recipes.html
      -- 2. Sources: https://cmp.saghen.dev/configuration/sources.html

      -- 'default' for mappings similar to built-in completion
      -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
      -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
      -- See the full "keymap" documentation for information on defining your own keymap.
      --
      -- Most importantly, use <C-y> to accept the completion and <C-e> to cancel it.
      keymap = {
        preset = "default",

        ["<C-k>"] = { "select_prev", "fallback" },
        ["<C-j>"] = { "select_next", "fallback" },
      },

      appearance = {
        -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        -- Adjusts spacing to ensure icons are aligned
        nerd_font_variant = "mono",
      },

      completion = {
        menu = {
          border = require("svim.config").border,
        },
        documentation = {
          auto_show = true,
          auto_show_delay_ms = 0,
          window = {
            border = require("svim.config").border,
          },
        },
        list = {
          selection = {
            -- When `true`, will automatically select the first item in the completion list
            preselect = false,
            -- preselect = function(ctx) return vim.bo.filetype ~= 'markdown' end,

            -- When `true`, inserts the completion item automatically when selecting it
            -- You may want to bind a key to the `cancel` command (default <C-e>) when using this option,
            -- which will both undo the selection and hide the completion menu
            auto_insert = true,
            -- auto_insert = function(ctx) return vim.bo.filetype ~= 'markdown' end
          },
        },
      },

      signature = {
        enabled = true,
        window = {
          border = require("svim.config").border,
        },
      },

      -- Default list of enabled providers defined so that you can extend it
      -- elsewhere in your config, without redefining it, due to `opts_extend`
      sources = {
        default = { "git", "lsp", "path", "snippets", "buffer" },
        providers = {
          git = {
            module = "blink-cmp-git",
            name = "Git",
            -- only enable this source when filetype is gitcommit, markdown, or 'octo'
            enabled = function()
              return vim.tbl_contains({ "octo", "gitcommit", "markdown" }, vim.bo.filetype)
            end,
            --- @module 'blink-cmp-git'
            --- @type blink-cmp-git.Options
            opts = {
              -- options for the blink-cmp-git
            },
          },
        },
      },

      cmdline = {
        enabled = true,
        keymap = {
          preset = "inherit",
          ["<C-k>"] = { "select_prev", "fallback" },
          ["<C-j>"] = { "select_next", "fallback" },
        },
        completion = {
          list = {
            selection = {
              preselect = false,
            },
          },
          menu = {
            auto_show = true,
          },
        },
      },
    },
    opts_extend = { "sources.default" },
  },
}

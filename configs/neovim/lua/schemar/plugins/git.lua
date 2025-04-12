return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("gitsigns").setup({
        current_line_blame = true,
        signs = {
          add = { text = "▎" },
          change = { text = "▎" },
          delete = { text = "▎" },
          topdelete = { text = "▎" },
          changedelete = { text = "▎" },
          untracked = { text = "▎" },
        },
        on_attach = require("schemar.config.keymaps").git_attach,
      })
    end,
  },
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim", -- required
      "sindrets/diffview.nvim", -- optional - Diff integration

      -- Only one of these is needed, not both.
      "nvim-telescope/telescope.nvim", -- optional
      -- "ibhagwan/fzf-lua", -- optional
    },
    cmd = "Neogit",
    config = function()
      local icons = require("schemar.config.options").icons.ui
      require("neogit").setup({
        -- disable_commit_confirmation = true,
        -- disable_builtin_notifications = true,
        integrations = {
          diffview = true,
        },
        signs = {
          -- { CLOSED, OPENED }
          section = { icons.ChevronShortRight, icons.ChevronShortDown },
          item = { icons.ChevronShortRight, icons.ChevronShortDown },
          hunk = { "", "" },
        },
        mappings = {
          status = {
            ["<c-t>"] = false,
          },
        },
      })
    end,
  },
  {
    "sindrets/diffview.nvim",
    dependencies = {
      "lewis6991/gitsigns.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    cmd = {
      "DiffviewOpen",
      "DiffviewFileHistory",
    },
    config = function()
      require("diffview").setup({
        default_args = {
          --imply-local   If a range rev is provided and either end of the range
          -- points to `HEAD`: point that end to local files
          -- instead (not created from git). This can be useful
          -- i.e. when using symmetric difference ranges
          -- (triple-dot), but you want to be able to utilize the
          -- LSP features that are not available while you're
          -- viewing files created from git.
          DiffviewOpen = { "--imply-local" },
        },
        keymaps = {
          disable_defaults = false, -- Disable the default keymaps
          view = {
            -- The `view` bindings are active in the diff buffers, only when the current
            -- tabpage is a Diffview.
            {
              "n",
              "q",
              "<cmd>DiffviewClose<cr>",
              { desc = "Close Diffview" },
            },
          },
          file_panel = {
            {
              "n",
              "q",
              "<cmd>DiffviewClose<cr>",
              { desc = "Close Diffview" },
            },
          },
          file_history_panel = {
            {
              "n",
              "q",
              "<cmd>DiffviewClose<cr>",
              { desc = "Close Diffview" },
            },
          },
        },
      })
    end,
  },
}

return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local icons = require("svim.config").icons.git
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
        on_attach = function(buffer)
          local gs = package.loaded.gitsigns

          local function map(mode, l, r, desc)
            vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
          end

          map("n", "]h", function()
            gs.nav_hunk("next")
          end, "Next Hunk")
          map("n", "[h", function()
            gs.nav_hunk("prev")
          end, "Prev Hunk")
          map("n", "]H", function()
            gs.nav_hunk("last")
          end, "Last Hunk")
          map("n", "[H", function()
            gs.nav_hunk("first")
          end, "First Hunk")
          map({ "n", "v" }, "<leader>gS", gs.stage_hunk, "Stage Hunk")
          map("n", "<leader>gU", gs.undo_stage_hunk, "Undo Stage Hunk")
          map("n", "<leader>gp", gs.preview_hunk, "Preview Hunk")
          map("n", "<leader>gP", gs.preview_hunk_inline, "Preview Hunk Inline")
          map("n", "<leader>gb", function()
            gs.blame_line({ full = true })
          end, "Blame Line")
          map("n", "<leader>gB", function()
            gs.blame()
          end, "Blame Buffer")
        end,
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
    keys = {
      {
        "<leader>gg",
        function()
          require("neogit").open()
        end,
        desc = "Neogit",
      },
    },
    config = function()
      local icons = require("svim.config").icons.ui
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
    keys = {
      { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diffview" },
      { "<leader>gh", "<cmd>DiffviewFileHistory<cr>", desc = "Branch history" },
      { "<leader>gf", "<cmd>DiffviewFileHistory %<cr>", desc = "File history" },
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
            {
              "n",
              "S",
              "<cmd>Gitsigns stage_hunk<cr>",
              { desc = "Close Diffview" },
            },
            {
              "n",
              "U",
              "<cmd>Gitsigns undo_stage_hunk<cr>",
              { desc = "Undo stage hunk" },
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

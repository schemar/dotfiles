return {
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local icons = require("svim.config").icons.git
      require("gitsigns").setup({
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
          map({ "n", "v" }, "<leader>gs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
          map({ "n", "v" }, "<leader>gr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
          map("n", "<leader>gS", gs.stage_buffer, "Stage Buffer")
          map("n", "<leader>gu", gs.undo_stage_hunk, "Undo Stage Hunk")
          map("n", "<leader>gR", gs.reset_buffer, "Reset Buffer")
          map("n", "<leader>gp", gs.preview_hunk_inline, "Preview Hunk Inline")
          map("n", "<leader>gb", function()
            gs.blame_line({ full = true })
          end, "Blame Line")
          map("n", "<leader>gd", gs.diffthis, "Diff This")
          map("n", "<leader>gD", function()
            gs.diffthis("~")
          end, "Diff This ~")
          map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
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
      -- "nvim-telescope/telescope.nvim", -- optional
      "ibhagwan/fzf-lua", -- optional
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
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      { "<leader>gh", "<cmd>DiffviewFileHistory<cr>", desc = "Branch history" },
      { "<leader>gf", "<cmd>DiffviewFileHistory %<cr>", desc = "File history" },
    },
    config = function()
      require("diffview").setup({
        default_args = {
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

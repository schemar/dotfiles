return {
  {
    "christoomey/vim-tmux-navigator", -- Switch windows/panes vim/tmux
    init = function()
      vim.g.tmux_navigator_no_mappings = 1
    end,
    cmd = {
      "TmuxNavigateLeft",
      "TmuxNavigateDown",
      "TmuxNavigateUp",
      "TmuxNavigateRight",
      "TmuxNavigatePrevious",
      "TmuxNavigatorProcessList",
    },
  },
  {
    -- Auto-disable hlsearch when moving cursor
    "asiryk/auto-hlsearch.nvim",
    event = "VeryLazy",
    name = "auto-hlsearch",
    opts = {},
  },
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    opts = {},
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {},
  },
  {
    "danilamihailov/beacon.nvim",
    event = { "CursorMoved", "WinEnter", "FocusGained" },
    opts = {
      enabled = function()
        local disabledFiletypes = {
          snacks_dashboard = true,
        }

        if vim.bo.ft:find("Neogit") or disabledFiletypes[vim.bo.ft] then
          return false
        end

        return true
      end,
      speed = 2, --- integer speed at wich animation goes
      width = 40, --- integer width of the beacon window
      winblend = 70, --- integer starting transparency of beacon window :h winblend
      fps = 60, --- integer how smooth the animation going to be
      min_jump = 10, --- integer what is considered a jump. Number of lines
      cursor_events = { "CursorMoved" }, -- table<string> what events trigger check for cursor moves
      window_events = { "WinEnter", "FocusGained" }, -- table<string> what events trigger cursor highlight
    },
  },
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      incremental = true,
    },
  },
}

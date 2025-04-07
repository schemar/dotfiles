return {
  {
    "numToStr/Comment.nvim",
    dependencies = { "JoosepAlviste/nvim-ts-context-commentstring" },
    event = { "VeryLazy" },
    config = function()
      require("Comment").setup({
        pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
      })
    end,
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = { "VeryLazy" },
    opts = {
      keywords = {
        TODOMS = {
          icon = require("schemar.config.options").icons.ui.Check .. " ",
          color = "info",
        },
      },
      highlight = {
        -- Remove required trailing colon:
        pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlighting (vim regex)
      },
      search = {
        -- regex that will be used to match keywords.
        -- don't replace the (KEYWORDS) placeholder
        -- pattern = [[\b(KEYWORDS):]], -- ripgrep regex
        pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
      },
    },
  },
}

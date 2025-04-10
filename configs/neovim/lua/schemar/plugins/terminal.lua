return {
  {
    "akinsho/toggleterm.nvim",
    cmd = { "ToggleTerm", "ToggleTermToggleAll", "TermExec", "TermNew", "TermSelect" },
    version = "*",
    opts = function()
      local border = require("schemar.config.options").border
      return {
        float_opts = {
          border = border,
        },
      }
    end,
  },
}

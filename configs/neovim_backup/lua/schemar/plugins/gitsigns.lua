return {
  "lewis6991/gitsigns.nvim",
  event = { "BufReadPost", "BufNewFile" },
  config = function()
    require("gitsigns").setup({
      current_line_blame = true,
    })
    -- This is for diagnostic signs on the line number column.
    -- Use this to beautify the plain E W signs.
    local signs = require("schemar.icons").diagnostics
    for type, icon in pairs(signs) do
      local hl = "DiagnosticSign" .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
    end
  end,
}

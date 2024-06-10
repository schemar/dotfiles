return {
  "folke/trouble.nvim", -- Better looking quicklist, diagnostics, etc.
  dependencies = { "nvim-tree/nvim-web-devicons" },
  cmd = "Trouble",
  config = function()
    require("trouble").setup({
      auto_fold = false, -- automatically fold a file trouble list at creation
    })

    -- Disable width ruler in trouble window:
    vim.api.nvim_create_autocmd("Filetype", {
      pattern = { "Trouble" },
      command = "set colorcolumn=0",
    })
  end,
}

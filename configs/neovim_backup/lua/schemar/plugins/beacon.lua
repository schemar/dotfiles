return {
  "danilamihailov/beacon.nvim",
  event = { "VeryLazy" },
  init = function()
    vim.g.beacon_ignore_filetypes = {
      "alpha",
      "Neogit",
      "NeogitStatus",
      "TelescopePrompt",
    }
  end,
}

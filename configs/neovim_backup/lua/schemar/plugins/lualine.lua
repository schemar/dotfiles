return {
  "nvim-lualine/lualine.nvim",
  name = "lualine",
  lazy = false,
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    options = {
      icons_enabled = true,
      theme = "catppuccin",
      disabled_filetypes = {
        statusline = {
          "aerial",
          "alpha",
          "NeogitCommitPopup",
          "NeogitStatus",
        },
        winbar = {},
      },
    },
    sections = {
      lualine_a = { "mode", "searchcount" },
      lualine_b = { "diff", { "diagnostics", sources = { "nvim_lsp", "nvim_diagnostic" } } },
      lualine_c = { { "filename", path = 1 } },
      lualine_x = { "encoding" },
      lualine_y = { "progress" },
      lualine_z = { "location" },
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = { { "filename", path = 1 } },
      lualine_x = { "location" },
      lualine_y = {},
      lualine_z = {},
    },
    extenstions = {
      "aerial",
      "lazy",
      "man",
      "nvim-tree",
      "toggleterm",
      "trouble",
    },
  },
}

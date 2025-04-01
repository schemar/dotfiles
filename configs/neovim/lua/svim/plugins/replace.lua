return {
  "nvim-pack/nvim-spectre",
  dependencies = { "nvim-lua/plenary.nvim" },
  lazy = true,
  keys = {
    {
      "<leader>sr",
      "<cmd>lua require('spectre').open()<CR>",
      desc = "Replace in files",
    },
    {
      "<leader>sR",
      "<cmd>lua require('spectre').open_visual({select_word=true})<CR>",
      desc = "Replace word under cursor",
    },
    {
      "<leader>sf",
      "<cmd>lua require('spectre').open_file_search()<CR>",
      desc = "Replace in current file",
    },
  },
  config = function()
    require("spectre").setup({
      default = {
        find = {
          cmd = "rg",
          options = { "ignore-case" },
        },
        replace = {
          cmd = "sd",
        },
      },
    })
  end,
}

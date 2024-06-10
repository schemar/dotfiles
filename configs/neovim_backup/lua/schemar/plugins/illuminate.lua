return {
  "RRethy/vim-illuminate", -- Highlight similar words (e.g. references with LSP)
  event = { "BufReadPost", "BufNewFile" },
  config = function()
    require("illuminate").configure({
      filetypes_denylist = {
        "dirvish",
        "fugitive",
        "aerial",
        "NvimTree",
        "NeogitCommitMessage",
        "NeogitStatus",
      },
    })
  end,
}

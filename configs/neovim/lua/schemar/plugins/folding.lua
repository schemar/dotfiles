return {
  "kevinhwang91/nvim-ufo",
  dependencies = { "kevinhwang91/promise-async" },
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    close_fold_kinds_for_ft = { default = { "imports" } },
  },
}

return {
  "kevinhwang91/nvim-ufo",
  dependencies = { "kevinhwang91/promise-async" },
  event = { "BufReadPost", "BufNewFile" },
  opts = {
    close_fold_kinds_for_ft = {
      default = {
        "imports", -- only works if the cursor is on the first line or after the imports; not within the imports
      },
    },
  },
}

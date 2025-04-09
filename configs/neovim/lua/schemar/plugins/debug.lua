return {
  "andrewferrier/debugprint.nvim",
  opts = {
    keymaps = {
      normal = {
        plain_below = "<leader>lgp",
        plain_above = "<leader>lgP",
        variable_below = "<leader>lgv",
        variable_above = "<leader>lgV",
        variable_below_alwaysprompt = "",
        variable_above_alwaysprompt = "",
        surround_plain = "<leader>lgsp",
        surround_variable = "<leader>lgsv",
        surround_variable_alwaysprompt = "",
        textobj_below = "<leader>lgo",
        textobj_above = "<leader>lgO",
        textobj_surround = "<leader>lgso",
        toggle_comment_debug_prints = "",
        delete_debug_prints = "<leader>lgD",
      },
      insert = {
        plain = "<C-G>p",
        variable = "<C-G>v",
      },
    },
  },
}

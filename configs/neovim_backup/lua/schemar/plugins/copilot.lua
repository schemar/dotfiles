return {
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    opts = {
      -- Disable these to use copilot-cmp instead:
      suggestion = { enabled = false },
      panel = { enabled = false },
    },
  },
  {
    "zbirenbaum/copilot-cmp",
    event = "InsertEnter",
    dependencies = { "zbirenbaum/copilot.lua", "hrsh7th/nvim-cmp" },
    opts = {},
  },
}

return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  lazy = true,
  opts = {
    flavour = "latte", -- latte, frappe, macchiato, mocha
    term_colors = true,
    integrations = {
      beacon = true,
      blink_cmp = true,
      copilot_vim = true,
      diffview = true,
      flash = true,
      gitsigns = true,
      lsp_trouble = true,
      markdown = true,
      mason = true,
      native_lsp = {
        enabled = true,
        virtual_text = {
          errors = { "italic" },
          hints = { "italic" },
          warnings = { "italic" },
          information = { "italic" },
          ok = { "italic" },
        },
        underlines = {
          errors = { "underline" },
          hints = { "underline" },
          warnings = { "underline" },
          information = { "underline" },
          ok = { "underline" },
        },
        inlay_hints = {
          background = true,
        },
      },
      neogit = true,
      nvimtree = true,
      render_markdown = true,
      snacks = {
        enabled = true,
        indent_scope_color = "lavender", -- catppuccin color (eg. `lavender`) Default: text
      },
      telescope = { enabled = true },
      treesitter = true,
      treesitter_context = true,
      ufo = true,
      which_key = true,
    },
  },
}

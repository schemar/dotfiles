return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  lazy = true,
  opts = {
    flavour = "macchiato", -- latte, frappe, macchiato, mocha
    integrations = {
      beacon = true,
      cmp = true,
      diffview = true,
      fidget = true,
      flash = true,
      gitsigns = true,
      illuminate = {
        enabled = true,
        lsp = false,
      },
      indent_blankline = {
        enabled = true,
        scope_color = "", -- catppuccin color (eg. `lavender`) Default: text
        -- colored_indent_levels enables char highlights per indent level. Follow the instructions here to set the latter up.
        colored_indent_levels = false,
      },
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
      telescope = { enabled = true },
      treesitter = true,
      treesitter_context = true,
      ufo = true,
      which_key = true,
    },
  },
}

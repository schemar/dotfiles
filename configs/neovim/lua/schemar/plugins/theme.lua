return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  lazy = false,
  opts = {
    flavour = "macchiato", -- latte, frappe, macchiato, mocha
    term_colors = true,
    highlight_overrides = {
      -- Increase contrast, which is not enough by default:
      latte = function(colors)
        return {
          ["@member"] = { fg = colors.blue },
          ["@variable"] = { fg = colors.blue },
          ["@variable.member"] = { fg = colors.blue }, -- For fields.
          ["@module"] = { fg = colors.blue }, -- For identifiers referring to modules an…
          ["@property"] = { fg = colors.blue }, -- Same as TSField.
          ["@property.css"] = { fg = colors.blue },
          ["@type.css"] = { fg = colors.blue },
          ["@property.typescript"] = { fg = colors.blue },
          ["@constructor.typescript"] = { fg = colors.blue },
          ["@constructor.tsx"] = { fg = colors.blue },
          Visual = { bg = colors.crust },
          LspReferenceText = { bg = colors.crust }, -- used for highlighting "text" references
          LspReferenceRead = { bg = colors.crust }, -- used for highlighting "read" references
          LspReferenceWrite = { bg = colors.crust }, -- used for highlighting "write" references
          illuminatedWord = { bg = colors.crust },
          illuminatedCurWord = { bg = colors.crust },
          IlluminatedWordText = { bg = colors.crust },
          IlluminatedWordRead = { bg = colors.crust },
          IlluminatedWordWrite = { bg = colors.crust },
        }
      end,
    },
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
  config = function(_, opts)
    require("catppuccin").setup(opts)
    vim.cmd([[colorscheme catppuccin]])
  end,
}

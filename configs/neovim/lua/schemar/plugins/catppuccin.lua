return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  opts = {
    flavour = "mocha",  -- auto, latte, frappe, macchiato, mocha
    dim_inactive = {
      enabled = true,   -- dims the background color of inactive window
      shade = "dark",
      percentage = 0.3, -- percentage of the shade to apply to the inactive window (lower number is darker)
    },
    highlight_overrides = {
      latte = function(colors)
        return {
          -- Make sure the telescope border is not dimmed as "inactive":
          TelescopeBorder = {
            fg = colors.blue,
            bg = colors.base,
          },
        }
      end,
    },
    integration_default = true,
    integrations = {
      aerial = true,
      alpha = true,
      beacon = true,
      cmp = true,
      colorful_winsep = {
        enabled = true,
        color = "yellow",
      },
      diffview = true,
      fidget = true,
      flash = true,
      gitsigns = true,
      illuminate = true,
      indent_blankline = {
        enabled = true,
        colored_indent_levels = true,
        scope_color = "lavender",
      },
      lsp_trouble = true,
      markdown = true,
      mason = true,
      mini = true,
      native_lsp = {
        enabled = true,
        virtual_text = {
          errors = { "italic" },
          hints = { "italic" },
          warnings = { "italic" },
          information = { "italic" },
        },
        underlines = {
          errors = { "underline" },
          hints = { "underline" },
          warnings = { "underline" },
          information = { "underline" },
        },
        inlay_hints = {
          background = true,
        },
      },
      neogit = true,
      notify = true,
      telescope = {
        enabled = true,
        -- style = "nvchad",
      },
      treesitter = true,
      treesitter_context = true,
      which_key = true,
    },
  },
}

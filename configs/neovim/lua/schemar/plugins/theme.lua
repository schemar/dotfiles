return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  lazy = false,
  opts = {
    flavour = "mocha", -- latte, frappe, macchiato, mocha
    term_colors = true,
    color_overrides = {
      frappe = {
        rosewater = "#7e3b08",
        flamingo = "#97480c",
        pink = "#6513cc",
        mauve = "#771ded",
        red = "#910956",
        maroon = "#aa0e66",
        peach = "#413d04",
        yellow = "#5e5908",
        green = "#2a4900",
        teal = "#395e08",
        sky = "#054821",
        sapphire = "#085e2d",
        blue = "#2527d0",
        lavender = "#2f3cee",
        text = "#24064b",
        subtext1 = "#2d085e",
        subtext0 = "#360971",
        overlay2 = "#414160",
        overlay1 = "#4e4e71",
        overlay0 = "#57577d",
        surface2 = "#e4cffa",
        surface1 = "#e7d4fb",
        surface0 = "#ebdbfb",
        base = "#f3edfa",
        mantle = "#f0e4fc",
        crust = "#eddffc",
      },
      mocha = {
        rosewater = "#da9464",
        flamingo = "#f1ccb6",
        pink = "#bc92f7",
        mauve = "#b07bf4",
        red = "#f155ae",
        maroon = "#f67dbd",
        peach = "#ffd69c",
        yellow = "#eae270",
        green = "#bfe1a0",
        teal = "#c6eba5",
        sky = "#a2dbb1",
        sapphire = "#39ef88",
        blue = "#aab1e1",
        lavender = "#8189d7",
        text = "#f4ecfd",
        subtext1 = "#efe2fc",
        subtext0 = "#ead9fb",
        overlay2 = "#dfd0f1",
        overlay1 = "#d5ccde",
        overlay0 = "#c8c8c8",
        surface2 = "#27252a",
        surface1 = "#251f2e",
        surface0 = "#22182e",
        base = "#090213",
        mantle = "#160a22",
        crust = "#22182e",
      },
    },
    highlight_overrides = {
      -- Increase contrast, which is not enough by default:
      latte = function(colors)
        return {
          -- Replace some "lavender" word fg with "blue":
          ["@variable.member"] = { fg = colors.blue }, -- For fields.
          ["@module"] = { fg = colors.blue }, -- For identifiers referring to modules an…
          ["@property"] = { fg = colors.blue }, -- Same as TSField.
          ["@property.css"] = { fg = colors.blue },
          ["@type.css"] = { fg = colors.blue },
          ["@property.typescript"] = { fg = colors.blue },
          ["@constructor.typescript"] = { fg = colors.blue },
          ["@constructor.tsx"] = { fg = colors.blue },

          -- Replace some "rosewater" word fg with "maroon":
          ["@string.special.url"] = { fg = colors.maroon },
          -- Replace cursor color to align with wezterm config:
          TermCursor = { bg = colors.mauve },

          -- Replace some "flamingo" word fg with "maroon":
          ["@lsp.type.interface"] = { fg = colors.maroon },
          ["@string.special.symbol"] = { fg = colors.maroon },
          Identifier = { fg = colors.maroon },
          markdownCode = { fg = colors.maroon },
          markdownCodeBlock = { fg = colors.maroon },

          -- Replace some "surface1" and "surface0" bg with "crust"
          Substitute = { bg = colors.crust }, -- used for substitution hints
          Visual = { bg = colors.crust }, -- used for highlighting visual selection
          VisualNOS = { bg = colors.crust }, -- Visual mode selection when vim is "Not Owning the Selection".
          LspReferenceText = { bg = colors.crust }, -- used for highlighting "text" references
          LspReferenceRead = { bg = colors.crust }, -- used for highlighting "read" references
          LspReferenceWrite = { bg = colors.crust }, -- used for highlighting "write" references
          LspSignatureActiveParameter = { bg = colors.crust },
          MatchParen = { bg = colors.crust }, -- The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|
          NeogitDiffContextHighlight = { bg = colors.crust },
          illuminatedWord = { bg = colors.crust },
          illuminatedCurWord = { bg = colors.crust },
          IlluminatedWordText = { bg = colors.crust },
          IlluminatedWordRead = { bg = colors.crust },
          IlluminatedWordWrite = { bg = colors.crust },
          RenderMarkdownCodeInline = { bg = colors.crust },
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

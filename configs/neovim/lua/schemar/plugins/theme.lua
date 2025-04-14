return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    lazy = false,
    opts = {
      flavour = "frappe", -- latte, frappe, macchiato, mocha
      term_colors = true,
      color_overrides = {
        frappe = {
          rosewater = "#a83734",
          flamingo = "#a83734",
          pink = "#704f97",
          mauve = "#704f97",
          red = "#94475e",
          maroon = "#94475e",
          peach = "#87540d",
          yellow = "#87540d",
          green = "#396b00",
          teal = "#396b00",
          sky = "#3c6764",
          sapphire = "#3c6764",
          blue = "#2c6195",
          lavender = "#2c6195",
          text = "#302d46",
          subtext1 = "#433e5f",
          subtext0 = "#575279",
          overlay2 = "#6c5e63",
          overlay1 = "#4d3e45",
          overlay0 = "#392c32",
          surface2 = "#cecacd",
          surface1 = "#dfdad9",
          surface0 = "#f4ede8",
          base = "#faf4ed",
          mantle = "#fffaf3",
          crust = "#f2e9e1",
        },
        mocha = {
          rosewater = "#ebbcba",
          flamingo = "#ebbcba",
          pink = "#c4a7e7",
          mauve = "#c4a7e7",
          red = "#ee91ab",
          maroon = "#ee91ab",
          peach = "#f8baae",
          yellow = "#f6c177",
          green = "#9bd680",
          teal = "#9bd680",
          sky = "#b5dde4",
          sapphire = "#b5dde4",
          blue = "#72b5cf",
          lavender = "#72b5cf",
          text = "#e0def4",
          subtext1 = "#908caa",
          subtext0 = "#6e6a86",
          overlay2 = "#b4a7ae",
          overlay1 = "#cdc3cb",
          overlay0 = "#e9e8e9",
          surface2 = "#332e47",
          surface1 = "#2a283e",
          surface0 = "#232031",
          base = "#191724",
          mantle = "#0b0a10",
          crust = "#242339",
        },
      },
      highlight_overrides = {
        frappe = function(colors)
          return {
            LineNr = { fg = colors.surface2 }, -- column where |signs| are displayed
          }
        end,
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
  },
}

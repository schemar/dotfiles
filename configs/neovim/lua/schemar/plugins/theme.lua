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
          rosewater = "#b6625d",
          flamingo = "#b6625d",
          pink = "#7776a7",
          mauve = "#7776a7",
          red = "#b36178",
          maroon = "#b36178",
          peach = "#b2732f",
          yellow = "#c79b16",
          green = "#3a8a53",
          teal = "#508b95",
          sky = "#508b95",
          sapphire = "#508b95",
          blue = "#3284a4",
          lavender = "#3284a4",
          text = "#45405e",
          subtext1 = "#706c89",
          subtext0 = "#8d8aa3",
          overlay2 = "#8d8d8d",
          overlay1 = "#7a7a7a",
          overlay0 = "#707070",
          surface2 = "#c5baba",
          surface1 = "#d6cdcd",
          surface0 = "#e9ded8",
          base = "#faf4ed",
          mantle = "#fffaf3",
          crust = "#f2e9e1",
        },
        mocha = {
          rosewater = "#ea9a97",
          flamingo = "#ea9a97",
          pink = "#c4a7e7",
          mauve = "#c4a7e7",
          red = "#eb6f92",
          maroon = "#eb6f92",
          peach = "#f6c177",
          yellow = "#f6c177",
          green = "#6fc38a",
          teal = "#6fc38a",
          sky = "#9ccfd8",
          sapphire = "#9ccfd8",
          blue = "#aabcc5",
          lavender = "#aabcc5",
          text = "#e0def4",
          subtext1 = "#b5b1e7",
          subtext0 = "#8e8cde",
          overlay2 = "#979797",
          overlay1 = "#b8b8b8",
          overlay0 = "#cbcbcb",
          surface2 = "#56526e",
          surface1 = "#44415a",
          surface0 = "#2a283e",
          base = "#232136",
          mantle = "#2a283e",
          crust = "#393552",
        },
      },
      highlight_overrides = {
        frappe = function(colors)
          return {
            -- Lighter highlighting:
            Visual = { bg = colors.surface0 }, -- used for highlighting visual selection
            VisualNOS = { bg = colors.surface0 }, -- Visual mode selection when vim is "Not Owning the Selection".

            LspReferenceText = { bg = colors.surface0 }, -- used for highlighting "text" references
            LspReferenceRead = { bg = colors.surface0 }, -- used for highlighting "read" references
            LspReferenceWrite = { bg = colors.surface0 }, -- used for highlighting "write" references
            LspSignatureActiveParameter = { bg = colors.surface0 },
            illuminatedWord = { bg = colors.surface0 },
            illuminatedCurWord = { bg = colors.surface0 },
            IlluminatedWordText = { bg = colors.surface0 },
            IlluminatedWordRead = { bg = colors.surface0 },
            IlluminatedWordWrite = { bg = colors.surface0 },
            MatchParen = { bg = colors.surface0 }, -- The character under the cursor or just before it, if it is a paired bracket, and its match. |pi_paren.txt|

            -- More contrast for line numbers:
            LineNr = { fg = colors.overlay2 }, -- Line numbers when the cursor is not on it.
            TreesitterContextLineNumber = { fg = colors.overlay2 },

            -- Less yellow for code words:
            StorageClass = { fg = colors.flamingo }, -- static, register, volatile, etc.
            Structure = { fg = colors.flamingo }, --  struct, union, enum, etc.
            Type = { fg = colors.flamingo }, -- (preferred) int, long, char, etc.

            context_class = { fg = colors.flamingo },
            context_interface = { fg = colors.flamingo },

            terminal_color_3 = { fg = colors.flamingo },
            terminal_color_11 = { fg = colors.flamingo },

            CmpItemKindClass = { fg = colors.flamingo },
            CmpItemKindInterface = { fg = colors.flamingo },

            NavicIconsClass = { fg = colors.flamingo },
            NavicIconsInterface = { fg = colors.flamingo },

            AerialLine = { fg = colors.flamingo },

            LspKindClass = { fg = colors.flamingo },
            LspKindEnum = { fg = colors.flamingo },

            BlinkCmpKindClass = { fg = colors.flamingo },
            BlinkCmpKindInterface = { fg = colors.flamingo },

            ["@type.builtin"] = { fg = colors.flamingo },
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
          indent_scope_color = "subtext0", -- catppuccin color (eg. `lavender`) Default: text
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

return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    lazy = false,
    opts = {
      flavour = "frappe", -- latte, frappe, macchiato, mocha
      color_overrides = {
        frappe = {
          mauve = "#6A67B4",
          pink = "#6A67B4",
          flamingo = "#A352A0",
          rosewater = "#A352A0",
          red = "#C53F64",
          maroon = "#C53F64",
          yellow = "#8A7400",
          peach = "#AC591C",
          green = "#288043",
          teal = "#007E7D",
          sky = "#007E7D",
          sapphire = "#007E7D",
          blue = "#1675AB",
          lavender = "#1675AB",
          text = "#5B5B5B",
          subtext1 = "#908A84",
          subtext0 = "#908A84",
          overlay2 = "#707070",
          overlay1 = "#646464",
          overlay0 = "#585858",
          surface2 = "#9C8282",
          surface1 = "#e8dfd4",
          surface0 = "#f1ece5",
          base = "#faf4ed",
          mantle = "#FFFAF0",
          crust = "#FFFAF0",
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
            -- surface0 and 1 is used as a background color most of the time,
            -- but also as a foreground color in some cases. This makes it
            -- impossible to ensure contrast in all cases.
            -- For this reason, we replace all surface foreground colors with
            -- other surface colors to increase contrast.
            -- (surface2 is a rare color which is exclusively used as a
            -- foreground color)
            --
            -- surface0:
            SnacksIndent = { fg = colors.surface1 },
            IblIndent = { fg = colors.surface1 },

            -- surface1:
            SignColumn = { fg = colors.surface2 }, -- column where |signs| are displayed
            SignColumnSB = { fg = colors.surface2 }, -- column where |signs| are displayed

            LineNr = { fg = colors.surface2 }, -- Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' o…
            TreesitterContextLineNumber = { fg = colors.surface2 },

            DapUIUnavailable = { fg = colors.surface2 },

            GitSignsCurrentLineBlame = { fg = colors.surface2 },

            -- More contrast menus:
            Pmenu = { fg = colors.overlay0 }, -- Popup menu: normal item.

            -- More contrast for window separator:
            WinSeparator = { fg = colors.surface2 }, -- Separator between windows.
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

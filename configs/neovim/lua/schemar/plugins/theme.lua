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
          mauve = "#6A67B4",
          pink = "#6A67B4",
          flamingo = "#A352A0",
          rosewater = "#A352A0",
          red = "#B5274E",
          maroon = "#B5274E",
          yellow = "#C49400",
          peach = "#A2611A",
          green = "#288043",
          teal = "#007E7D",
          sky = "#007E7D",
          sapphire = "#007E7D",
          blue = "#1675AB",
          lavender = "#1675AB",
          text = "#5B5B5B",
          subtext1 = "#5B5B5B",
          subtext0 = "#605C75",
          overlay2 = "#707070",
          overlay1 = "#646464",
          overlay0 = "#585858",
          surface2 = "#e0d6c8",
          surface1 = "#e8dfd4",
          surface0 = "#f1ece5",
          base = "#faf4ed",
          mantle = "#fffaf3",
          crust = "#f2e9e1",
          --
          -- For less red tint use these instead:
          -- surface2 = "#dbd7d0",
          -- surface1 = "#e6dfd8",
          -- surface0 = "#efece9",
          -- base = "#f8f4f0",
          -- mantle = "#fdfaf7",
          -- crust = "#efeae6",
          --
          -- For no red tint use these instead:
          -- surface2 = "#d7d7d7",
          -- surface1 = "#e0e0e0",
          -- surface0 = "#ececec",
          -- base = "#f5f5f5",
          -- mantle = "#fbfbfb",
          -- crust = "#ebebeb",
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
            -- More contrast for line numbers:
            LineNr = { fg = colors.overlay2 }, -- Line numbers when the cursor is not on it.
            TreesitterContextLineNumber = { fg = colors.overlay2 },

            -- More contrast menus:
            Pmenu = { bg = colors.surface0 }, -- Popup menu: normal item.
            PmenuSel = { bg = colors.surface1 }, -- Popup menu: selected item.
            PmenuSbar = { bg = colors.surface1 }, -- Popup menu: scrollbar.
            PmenuThumb = { bg = colors.overlay0 }, -- Popup menu: Thumb of the scrollbar.

            -- More contrast other highlights:
            GitSignsCurrentLineBlame = { fg = colors.surface2 },

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
            ["@type.builtin.c"] = { fg = colors.flamingo },
            ["@property.class.css"] = { fg = colors.flamingo },
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

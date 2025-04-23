local color_overrides = {
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
}

local highlight_overrides = function(colors)
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
end

local M = {}

---@param flavor "mocha"|"macchiato"|"frappe"|"latte"
---@return table overrides to deep-merge into catppuccin config
function M.get_overrides(flavor)
  return {
    color_overrides = {
      [flavor] = color_overrides,
    },
    highlight_overrides = {
      [flavor] = highlight_overrides,
    },
  }
end

return M

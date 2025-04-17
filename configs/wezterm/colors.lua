local color_schemes = {}

local peach = {
  rosewater = "#97597b",
  flamingo = "#97597b",
  pink = "#6b6aa0",
  mauve = "#6b6aa0",
  red = "#b74e5f",
  maroon = "#b74e5f",
  peach = "#996329",
  yellow = "#ae8813",
  green = "#3a8a53",
  teal = "#467881",
  sky = "#467881",
  sapphire = "#467881",
  blue = "#2e7794",
  lavender = "#2e7794",
  text = "#45405e",
  subtext1 = "#5e5a72",
  subtext0 = "#706c89",
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
}

color_schemes.peach = {
  -- The default text color
  foreground = peach.text,
  -- The default background color
  background = peach.base,

  -- Overrides the cell background color when the current cell is occupied by the
  -- cursor and the cursor style is set to Block
  cursor_bg = peach.subtext1,
  -- Overrides the text color when the current cell is occupied by the cursor
  cursor_fg = peach.base,
  -- Specifies the border color of the cursor when the cursor style is set to Block,
  -- or the color of the vertical or horizontal bar when the cursor style is set to
  -- Bar or Underline.
  cursor_border = peach.subtext1,

  -- the foreground color of selected text
  selection_fg = peach.text,
  -- the background color of selected text
  selection_bg = peach.surface1,

  -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  scrollbar_thumb = peach.overlay0,

  -- The color of the split lines between panes
  split = peach.mauve,

  ansi = {
    peach.text, --"black",
    peach.maroon, --"maroon",
    peach.green, --"green",
    peach.peach, -- "olive",
    peach.blue, --"navy",
    peach.mauve, --"purple",
    peach.teal, --"teal",
    peach.overlay0, --"silver",
  },
  brights = {
    peach.text, --"grey",
    peach.red, --"red",
    peach.yellow, --"lime",
    peach.peach, --"yellow",
    peach.blue, --"blue",
    peach.rosewater, --"fuchsia",
    peach.sky, --"aqua",
    peach.overlay2, --"white",
  },

  -- Arbitrary colors of the palette in the range from 16 to 255
  -- indexed = { [136] = "#af8700" },

  -- Since: 20220319-142410-0fcdea07
  -- When the IME, a dead key or a leader key are being processed and are effectively
  -- holding input pending the result of input composition, change the cursor
  -- to this color to give a visual cue about the compose state.
  compose_cursor = peach.flamingo,

  -- Colors for copy_mode and quick_select
  -- available since: 20220807-113146-c2fee766
  -- In copy_mode, the color of the active text is:
  -- 1. copy_mode_active_highlight_* if additional text was selected using the mouse
  -- 2. selection_* otherwise
  -- copy_mode_active_highlight_bg = { Color = "#000000" },
  -- use `AnsiColor` to specify one of the ansi color palette values
  -- (index 0-15) using one of the names "Black", "Maroon", "Green",
  --  "Olive", "Navy", "Purple", "Teal", "Silver", "Grey", "Red", "Lime",
  -- "Yellow", "Blue", "Fuchsia", "Aqua" or "White".
  -- copy_mode_active_highlight_fg = { AnsiColor = "Black" },
  -- copy_mode_inactive_highlight_bg = { Color = "#52ad70" },
  -- copy_mode_inactive_highlight_fg = { AnsiColor = "White" },

  -- quick_select_label_bg = { Color = "peru" },
  -- quick_select_label_fg = { Color = "#ffffff" },
  -- quick_select_match_bg = { AnsiColor = "Navy" },
  -- quick_select_match_fg = { Color = "#ffffff" },

  -- input_selector_label_bg = { AnsiColor = "Black" }, -- (*Since: Nightly Builds Only*)
  -- input_selector_label_fg = { Color = "#ffffff" }, -- (*Since: Nightly Builds Only*)
}

return color_schemes

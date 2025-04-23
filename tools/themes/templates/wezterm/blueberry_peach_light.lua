local colors = {
  mauve = "{{mauve}}",
  pink = "{{pink}}",
  flamingo = "{{flamingo}}",
  rosewater = "{{rosewater}}",
  red = "{{red}}",
  maroon = "{{maroon}}",
  yellow = "{{yellow}}",
  peach = "{{peach}}",
  green = "{{green}}",
  teal = "{{teal}}",
  sky = "{{sky}}",
  sapphire = "{{sapphire}}",
  blue = "{{blue}}",
  lavender = "{{lavender}}",
  text = "{{text}}",
  subtext1 = "{{subtext1}}",
  subtext0 = "{{subtext0}}",
  overlay2 = "{{overlay2}}",
  overlay1 = "{{overlay1}}",
  overlay0 = "{{overlay0}}",
  surface2 = "{{surface2}}",
  surface1 = "{{surface1}}",
  surface0 = "{{surface0}}",
  base = "{{base}}",
  mantle = "{{mantle}}",
  crust = "{{crust}}",
}

return {
  -- The default text color
  foreground = colors.text,
  -- The default background color
  background = colors.base,

  -- Overrides the cell background color when the current cell is occupied by the
  -- cursor and the cursor style is set to Block
  cursor_bg = colors.teal,
  -- Overrides the text color when the current cell is occupied by the cursor
  cursor_fg = colors.base,
  -- Specifies the border color of the cursor when the cursor style is set to Block,
  -- or the color of the vertical or horizontal bar when the cursor style is set to
  -- Bar or Underline.
  cursor_border = colors.teal,

  -- the foreground color of selected text
  selection_fg = colors.text,
  -- the background color of selected text
  selection_bg = colors.surface1,

  -- The color of the scrollbar "thumb"; the portion that represents the current viewport
  scrollbar_thumb = colors.overlay0,

  -- The color of the split lines between panes
  split = colors.mauve,

  ansi = {
    colors.text, --"black",
    colors.maroon, --"maroon",
    colors.green, --"green",
    colors.peach, -- "olive",
    colors.blue, --"navy",
    colors.mauve, --"purple",
    colors.teal, --"teal",
    colors.overlay0, --"silver",
  },
  brights = {
    colors.text, --"grey",
    colors.red, --"red",
    colors.yellow, --"lime",
    colors.peach, --"yellow",
    colors.blue, --"blue",
    colors.rosewater, --"fuchsia",
    colors.sky, --"aqua",
    colors.overlay2, --"white",
  },

  -- Arbitrary colors of the palette in the range from 16 to 255
  -- indexed = { [136] = "#af8700" },

  -- Since: 20220319-142410-0fcdea07
  -- When the IME, a dead key or a leader key are being processed and are effectively
  -- holding input pending the result of input composition, change the cursor
  -- to this color to give a visual cue about the compose state.
  compose_cursor = colors.flamingo,

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

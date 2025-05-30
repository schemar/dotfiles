local wezterm = require("wezterm")

local theme_mode = "light" -- default to light mode
local theme_file = os.getenv("HOME") .. "/.config/current_theme"
local handle = io.popen(theme_file)
if not handle then
  wezterm.log_error("Could not read theme script: " .. theme_file)
else
  theme_mode = handle:read("*a")
  handle:close()
end

local color_scheme = theme_mode == "light" and "Blueberry Peach Light" or "Blueberry Peach Dark"

local config = {
  -- Allows to compose umlauts, etc. with left option key.
  -- send_composed_key_when_left_alt_is_pressed = true,
  -- send_composed_key_when_right_alt_is_pressed = false,

  font = wezterm.font_with_fallback({
    {
      family = "Monaspace Neon",
      weight = "Regular",
      harfbuzz_features = {
        "calt=1",
        "dlig=1",
        "cv01=2", -- Slashed zero
        "ss01=1",
        "ss02=1",
        "ss03=1",
        "ss04=1",
        "ss05=1",
        "ss06=1",
        "ss07=1",
        "ss08=1",
        "ss09=1",
        "ss10=1",
      },
    },
    {
      family = "Symbols Nerd Font Mono",
    },
  }),
  font_size = 13,

  color_scheme = color_scheme,

  hide_tab_bar_if_only_one_tab = true,

  window_decorations = "RESIZE",
  window_close_confirmation = "NeverPrompt",
}

return config

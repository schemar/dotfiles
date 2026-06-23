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
      family = "MonoLisaCode",
      weight = "Regular",
      harfbuzz_features = {
        "ss03=1",
        "ss13=1",
        "ss14=1",
        "cv01=1",
        "cv02=1",
        "cv03=1",
        "cv04=1",
        "cv05=1",
        "cv06=1",
        "cv07=1",
        "cv08=1",
        "cv09=1",
        "cv10=1",
        "cv11=1",
        "cv12=1",
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

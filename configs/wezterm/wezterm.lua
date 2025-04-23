local wezterm = require("wezterm")
local color_schemes = require("color_schemes")

local color_scheme = "blueberry_peach_light"
local cursor_bg = function()
  if color_scheme == "Catppuccin Macchiato" then
    return "#cdc3cb" -- Own text color overlay1; aligned with catppuccin neovim cursor setting.
  end

  -- Keep default cursor bg if not catppuccin latte:
  return nil
end

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

  color_schemes = color_schemes,
  color_scheme = color_scheme,

  colors = {
    cursor_bg = cursor_bg(),
    cursor_border = cursor_bg(),
  },

  hide_tab_bar_if_only_one_tab = true,

  window_decorations = "RESIZE",
  window_close_confirmation = "NeverPrompt",
}

return config

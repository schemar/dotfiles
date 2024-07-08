local wezterm = require("wezterm")

return {
  -- Allows to compose umlauts, etc. with left option key.
  -- Means a backup "real" Alt-key is the right option key.
  send_composed_key_when_left_alt_is_pressed = true,
  send_composed_key_when_right_alt_is_pressed = false,

  font = wezterm.font_with_fallback({
    {
      family = "Monaspace Neon",
      harfbuzz_features = {
        "calt=1",
        "dlig=1",
        "ss01=1",
        "ss02=1",
        "ss03=1",
        "ss04=1",
        "ss05=1",
        "ss06=1",
        "ss07=1",
        "ss08=1",
      },
      -- Disable all MonaLisa ligatures:
      -- harfbuzz_features = { "calt=0", "clig=0", "liga=0" },
    },
    {
      family = "Symbols Nerd Font Mono",
    },
  }),
  font_size = 13,

  color_scheme = "Catppuccin Latte",

  hide_tab_bar_if_only_one_tab = true,

  window_decorations = "RESIZE",
  window_close_confirmation = "NeverPrompt",
}

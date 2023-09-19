local wezterm = require("wezterm")

return {
	font = wezterm.font_with_fallback({
		{
			family = "MonoLisa",
			-- Disable all ligatures:
			harfbuzz_features = { "calt=0", "clig=0", "liga=0" },
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

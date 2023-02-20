local wezterm = require("wezterm")

return {
	font = wezterm.font_with_fallback({
		"MonoLisa",
		"Symbols Nerd Font Mono",
	}),
	font_size = 13,

	color_scheme = "Catppuccin Mocha",

	hide_tab_bar_if_only_one_tab = true,
	window_decorations = "RESIZE",
	window_close_confirmation = "NeverPrompt",
}

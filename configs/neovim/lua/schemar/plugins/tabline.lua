-- Configuration for the tabline.
-- Might be done with a plugin called buffer*.
-- This file is still about the tabline.

return {
	"akinsho/bufferline.nvim", -- Alternatives: "nanozuki/tabby.nvim"(, "kdheepak/tabline.nvim")
	version = "*",
	dependencies = { "catppuccin" },
	opts = {
		highlights = require("catppuccin.groups.integrations.bufferline").get(),
		options = {
			mode = "tabs", -- Only show tabpages, not all buffers
			numbers = "ordinal", -- "none" | "ordinal" | "buffer_id" | "both" | function({ ordinal, id, lower, raise }): string,
			diagnostics = "nvim_lsp",
		},
	},
}

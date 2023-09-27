-- Automatically convert from/to template strings based on presence of ${}
return {
	"chrisgrieser/nvim-puppeteer",
	dependencies = "nvim-treesitter/nvim-treesitter",
	lazy = false, -- plugin lazy-loads itself. Can also load on filetypes.
}

return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	opts = {
		-- Set up format-on-save
		format_on_save = { timeout_ms = 500, lsp_fallback = true },
		formatters_by_ft = {
			lua = { "stylua" },
			javascript = { "eslint_d", "prettier" },
			typescript = { "eslint_d", "prettier" },
			json = { "jq" },
			-- Conform will run multiple formatters sequentially
			-- python = { "isort", "black" },
			-- Use a sub-list to run only the first available formatter
			-- javascript = { { "prettierd", "prettier" } },
		},
		-- Customize formatters
		-- formatters = {
		-- 	shfmt = {
		-- 		prepend_args = { "-i", "2" },
		-- 	},
		-- },
	},
	-- init = function()
	--   -- If you want the formatexpr, here is the place to set it
	--   vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
	-- end,
}

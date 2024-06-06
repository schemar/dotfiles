return {
	{
		"ibhagwan/fzf-lua",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		keys = {
			{
				"<leader>f",
				function()
					require("fzf-lua").files()
				end,
				desc = "Files",
			},
			{
				"<leader>F",
				function()
					require("fzf-lua").oldfiles()
				end,
				desc = "Files (opened)",
			},
			{
				"<leader>b",
				function()
					require("fzf-lua").buffers()
				end,
				desc = "Buffers",
			},
			{
				"<leader>/",
				function()
					require("fzf-lua").live_grep()
				end,
				desc = "Grep Project",
			},
			{
				"<leader>*",
				function()
					require("fzf-lua").grep_cword()
				end,
				desc = "Grep project (word under cursor)",
			},
			{
				"<leader>r",
				function()
					require("fzf-lua").resume()
				end,
				desc = "Resume FZF",
			},
			{
				"gd",
				function()
					require("fzf-lua").lsp_definitions()
				end,
				desc = "Definitions",
			},
			{
				"gr",
				function()
					require("fzf-lua").lsp_references()
				end,
				desc = "References",
			},
		},
		config = function()
			-- Profiles
			-- Conveniently, fzf-lua comes with a set of preconfigured profiles, notably:
			--
			-- Profile	Details
			-- default	fzf-lua defaults, uses neovim "builtin" previewer and devicons (if available) for git/files/buffers
			-- fzf-native	utilizes fzf's native previewing ability in the terminal where possible using bat for previews
			-- fzf-tmux	similar to fzf-native and opens in a tmux popup (requires tmux > 3.2)
			-- fzf-vim	closest to fzf.vim's defaults (+icons), also sets up user commands (:Files, :Rg, etc)
			-- max-perf	similar to fzf-native and disables icons globally for max performance
			-- telescope	closest match to telescope defaults in look and feel and keybinds
			-- skim	uses skim as an fzf alternative, (requires the sk binary)
			-- Use :FzfLua profiles to experiment with the different profiles, once you've found what you like and wish to make the profile persist, send a string argument at the first index of the table sent to the setup function:
			--
			-- require('fzf-lua').setup({'fzf-native'})
			-- Note: setup can be called multiple times for profile "live" switching
			require("fzf-lua").setup({ "default" })
		end,
	},
	{
		"stevearc/aerial.nvim",
		-- Optional dependencies
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-tree/nvim-web-devicons",
		},
		keys = {
			{ "<leader>lo", "<cmd>AerialToggle<cr>", desc = "Aerial (Symbols)" },
		},
		config = function()
			local icons = vim.deepcopy(require("svim.config").icons.kinds)
			-- HACK: fix lua's weird choice for `Package` for control
			-- structures like if/else/for/etc.
			icons.lua = { Package = icons.Control }

			require("aerial").setup({
				attach_mode = "global",
				backends = { "lsp", "treesitter", "markdown", "man" },
				show_guides = true,
				layout = {
					resize_to_content = false,
					win_opts = {
						winhl = "Normal:NormalFloat,FloatBorder:NormalFloat,SignColumn:SignColumnSB",
						signcolumn = "yes",
						statuscolumn = " ",
					},
				},
				icons = icons,
				guides = {
					mid_item = "├╴",
					last_item = "└╴",
					nested_top = "│ ",
					whitespace = "  ",
				},
			})
		end,
	},
	{
		"christoomey/vim-tmux-navigator", -- Switch windows/panes vim/tmux
		event = { "VeryLazy" },
	},
	{
		"rgroli/other.nvim", -- Go to alternative file, e.g. ts<->vue or test
		name = "other-nvim",
		event = { "BufReadPost", "BufNewFile" },
		keys = {

			{ "<leader>o", "<cmd>Other<cr>", "Open 'other' file" },
			{ "<leader>O", "<cmd>OtherClear<cr><cmd>Other<cr>", "Open 'other' file (clear)" },
		},
		opts = {
			-- Should the window show files which do not exist yet based on
			-- pattern matching. Selecting the files will create the file.
			showMissingFiles = false,
			style = {
				border = "rounded",
			},
			-- Map files to other files.
			-- See documentation for details and more options.
			mappings = {
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.vue", context = "vue" },
				{ pattern = "(.*)/(.*).vue", target = "%1/%2.ts", context = "vue" },
				{ pattern = "(.*)/(.*).vue.gen.ts", target = "%1/%2.vue", context = "gen" },
				{ pattern = "(.*)/(.*).vue", target = "%1/%2.vue.gen.ts", context = "gen" },
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.spec.ts", context = "spec" },
				{ pattern = "(.*)/(.*).spec.ts", target = "%1/%2.ts", context = "spec" },
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.test.ts", context = "test" },
				{ pattern = "(.*)/(.*).test.ts", target = "%1/%2.ts", context = "test" },
				{ pattern = "(.*)/(.*).ts", target = "%1/%2.unit.test.ts", context = "test" },
				{ pattern = "(.*)/(.*).unit.test.ts", target = "%1/%2.ts", context = "test" },
			},
		},
	},
}

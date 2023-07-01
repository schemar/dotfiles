return {
	"nvim-telescope/telescope.nvim",
	branch = "0.1.x",
	cmd = { "Telescope" },
	dependencies = {
		"nvim-telescope/telescope-file-browser.nvim", -- Think Emacs directory browser
		{
			"nvim-telescope/telescope-fzf-native.nvim", -- FZF algorithm for telescope
			build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
		},
		"nvim-lua/plenary.nvim",
		{
			"danielfalk/smart-open.nvim",
			branch = "0.2.x",
			dependencies = {
				"kkharji/sqlite.lua",
				"nvim-tree/nvim-web-devicons",
				"nvim-telescope/telescope-fzf-native.nvim",
			},
		},
		"folke/trouble.nvim",
	},
	config = function()
		local telescope = require("telescope")
		local telescope_config = require("telescope.config")
		local actions = require("telescope.actions")
		local trouble = require("trouble.providers.telescope")

		-- Clone the default Telescope configuration
		local vimgrep_arguments = { unpack(telescope_config.values.vimgrep_arguments) }

		-- I want to search in hidden/dot files.
		table.insert(vimgrep_arguments, "--hidden")
		-- I don't want to search in the `.git` directory.
		table.insert(vimgrep_arguments, "--glob")
		table.insert(vimgrep_arguments, "!.git/*")

		telescope.setup({
			defaults = {
				-- `hidden = true` is not supported in text grep commands.
				vimgrep_arguments = vimgrep_arguments,
				mappings = {
					i = {
						["<c-j>"] = actions.move_selection_next,
						["<c-k>"] = actions.move_selection_previous,
						["<esc>"] = actions.close, -- Close on first press of esc. No "normal" mode.
						["<c-t>"] = trouble.open_with_trouble,
					},
					n = {
						["<c-t>"] = trouble.open_with_trouble,
					},
				},
				-- Themeing
				sorting_strategy = "ascending",
				layout_strategy = "horizontal",
				layout_config = {
					prompt_position = "top",
				},
			},
			pickers = {
				find_files = {
					-- `hidden = true` will still show the inside of `.git/` as it's not `.gitignore`d.
					find_command = { "rg", "--files", "--hidden", "--glob", "!.git/*" },
				},
				lsp_references = {
					show_line = false,
				},
			},
			extensions = {
				file_browser = { hijack_netrw = true },
				fzf = {
					fuzzy = true, -- false will only do exact matching
					override_generic_sorter = true, -- override the generic sorter
					override_file_sorter = true, -- override the file sorter
					case_mode = "smart_case", -- or "ignore_case" or "respect_case"
				},
				smart_open = {
					ignore_patterns = { "*.git/*", "*/tmp/*", "*/node_modules/*" },
					match_algorithm = "fzf",
				},
			},
		})
		telescope.load_extension("file_browser")
		telescope.load_extension("fzf")
		telescope.load_extension("smart_open")
	end,
}

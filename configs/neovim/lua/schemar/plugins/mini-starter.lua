return {
	"echasnovski/mini.starter",
	version = false,
	event = "VimEnter",
	config = function()
		local starter = require("mini.starter")
		starter.setup({
			evaluate_single = false,
			items = {
				starter.sections.recent_files(10, true),
				starter.sections.recent_files(10, false),
				starter.sections.builtin_actions(),
				-- Enable when mini.sessions is set up
				-- starter.sections.sessions(5, true),
			},
			content_hooks = {
				starter.gen_hook.adding_bullet(),
				starter.gen_hook.aligning("center", "top"),
				starter.gen_hook.padding(0, 5),
			},
			header = table.concat({
				" ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
				" ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
				" ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
				" ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
				" ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
				" ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
			}, "\n"),
			footer = function()
				return "🚀 Rocket science since " .. os.date("%Y-%m-%dT%H:%M:%S")
			end,
		})

		-- Use <C-j/k> to move cursor:
		vim.api.nvim_create_autocmd("User", {
			pattern = "MiniStarterOpened",
			callback = function(opts)
				vim.api.nvim_buf_set_keymap(
					opts.buf,
					"n",
					"<C-j>",
					"<Cmd>lua MiniStarter.update_current_item('next')<CR>",
					{ nowait = true, silent = true }
				)
				vim.api.nvim_buf_set_keymap(
					opts.buf,
					"n",
					"<C-k>",
					"<Cmd>lua MiniStarter.update_current_item('prev')<CR>",
					{ nowait = true, silent = true }
				)
			end,
		})
	end,
}

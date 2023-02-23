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
			footer = "\n🚀 Rocket Science",
		})
	end,
}

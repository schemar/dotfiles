return {
	"github/copilot.vim",
	event = "VeryLazy",
	config = function()
		vim.cmd([[imap <silent><script><expr> <F8> copilot#Accept("")]])
		vim.cmd([[let g:copilot_no_tab_map = v:true]])
	end,
}

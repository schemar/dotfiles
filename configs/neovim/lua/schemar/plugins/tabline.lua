-- Configuration for the tabline.
-- Might be done with a plugin called buffer*.
-- This file is still about the tabline.

return {
	"nanozuki/tabby.nvim", -- Alternatives: "akinsho/bufferline.nvim", "nanozuki/tabby.nvim"(, "kdheepak/tabline.nvim")
	dependencies = { "folke/which-key.nvim" },
	config = function()
		local wk = require("which-key")
		wk.register({
			t = {
				name = "Tab",
				c = { ":tabclose<CR>", "Close tab" },
				f = { ":tabfirst<CR>", "First tab" },
				l = { ":tablast<CR>", "Last tab" },
				m = { ":tabmove ", "Move tab to position (index)", silent = false },
				n = { ":tabnext<CR>", "Next tab (gt)" },
				p = { ":tabprevious<CR>", "Previous tab (gT)" },
				r = { ":TabRename ", "Rename tab", silent = false },
				s = { ":tab split<CR>", "Split tab with current buffer" },
				t = { ":tabnew<CR>", "New tab" },
			},
		}, { prefix = "<leader>" })

		local mocha = require("catppuccin.palettes").get_palette("mocha")
		local theme = {
			bg = { fg = mocha.text, bg = mocha.crust },
			fill = { fg = mocha.text, bg = mocha.crust },
			head = { fg = mocha.crust, bg = mocha.blue },
			current_tab = { fg = mocha.crust, bg = mocha.mauve },
			tab = { fg = mocha.text, bg = mocha.crust },
			current_win = { fg = mocha.crust, bg = mocha.blue },
			win = { fg = mocha.text, bg = mocha.crust },
			tail = { fg = mocha.crust, bg = mocha.mauve },
		}

		local icons = require("schemar.icons")
		require("tabby.tabline").set(function(line)
			return {
				hl = theme.fill,
				-- Below follow the sections of the tabline:
				{
					{ "  ", hl = theme.head }, -- Show something before the tabline
					line.sep("", theme.head, theme.fill),
				},
				line.tabs().foreach(function(tab)
					local hl = tab.is_current() and theme.current_tab or theme.tab
					return {
						line.sep("", theme.bg, hl),
						-- tab.is_current() and icons.ui.ChevronShortRight or "", -- Indicate current tab with an icon
						tab.number() - 1, -- The number of this tab, but starting at `0` to use index for moving tabs
						tab.name(),
						-- tab.close_btn(icons.ui.Close), -- Show a close button
						line.sep("", hl, theme.bg),
						hl = hl,
						margin = " ",
					}
				end),
				line.spacer(),
				line.wins_in_tab(line.api.get_current_tab()).foreach(function(win)
					local hl = win.is_current() and theme.current_win or theme.win
					return {
						line.sep("", hl, theme.bg),
						-- win.is_current() and icons.ui.ChevronShortRight or "", -- Indicate current window
						win.buf_name(),
						line.sep("", theme.bg, hl),
						hl = hl,
						margin = " ",
					}
				end),
				{
					line.sep("", theme.tail, theme.fill), -- Show something after the tabline
					{ " " .. icons.ui.Code .. " ", hl = theme.tail },
				},
			}
		end, {
			tab_name = {
				-- Copy of https://github.com/nanozuki/tabby.nvim/blob/9065c65138b59ea8182024216a5bbcf0d77baebb/lua/tabby/feature/tab_name.lua#L13
				-- Except for listing of additional windows, if present.
				name_fallback = function(tabid)
					local api = require("tabby.module.api")
					local buf_name = require("tabby.feature.buf_name")
					local wins = api.get_tab_wins(tabid)
					local cur_win = api.get_tab_current_win(tabid)
					local name = ""

					if api.is_float_win(cur_win) then
						name = "[Floating]"
					else
						name = buf_name.get(cur_win)
					end
					if #wins > 1 then
						name = string.format("%s +", name) -- Append a `+` if there are more windows
					end
					return name
				end,
			},
			buf_name = {
				mode = "unique",
			},
		})
	end,
}

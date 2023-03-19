-- Configuration for the tabline.
-- Might be done with a plugin called buffer*.
-- This file is still about the tabline.

return {
	"nanozuki/tabby.nvim", -- Alternatives: "akinsho/bufferline.nvim", "nanozuki/tabby.nvim"(, "kdheepak/tabline.nvim")
	config = function()
		local icons = require("schemar.icons")
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

		require("tabby.tabline").set(function(line)
			return {
				{
					{ "  ", hl = theme.head }, -- Show something before the tabline
					line.sep("", theme.head, theme.fill),
				},
				line.tabs().foreach(function(tab)
					local hl = tab.is_current() and theme.current_tab or theme.tab
					return {
						line.sep("", theme.bg, hl),
						-- tab.is_current() and icons.ui.ChevronShortRight or "", -- Indicate current tab with an icon
						tab.number(), -- The number of this tab.
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
				hl = theme.fill,
			}
		end)
	end,
}

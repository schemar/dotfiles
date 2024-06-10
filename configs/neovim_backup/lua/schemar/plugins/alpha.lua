return {
  "goolord/alpha-nvim",
  event = "VimEnter",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local alpha = require("alpha")
    local dashboard = require("alpha.themes.dashboard")

    local icons = require("schemar.icons").ui

    -- Set header
    dashboard.section.header.val = {
      "                                                     ",
      "  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ ",
      "  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║ ",
      "  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ ",
      "  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ ",
      "  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ ",
      "  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ ",
      "                                                     ",
    }

    -- Set menu
    dashboard.section.buttons.val = {
      dashboard.button(
        "f",
        "󰍉  " .. icons.ChevronShortRight .. " Search file",
        "<cmd>Telescope smart_open<cr>"
      ),
      dashboard.button(
        "r",
        "  " .. icons.ChevronShortRight .. " Recent",
        "<cmd>Telescope oldfiles<cr>"
      ),
      dashboard.button(
        "e",
        "  " .. icons.ChevronShortRight .. " New file",
        "<cmd>ene <bar> startinsert <cr>"
      ),
      dashboard.button(
        "s",
        "  " .. icons.ChevronShortRight .. " Settings",
        "<cmd>e $MYVIMRC | :cd %:p:h | split . | wincmd k <cr>"
      ),
      dashboard.button("q", "󰅙  " .. icons.ChevronShortRight .. " Quit NVIM", "<cmd>qa<cr>"),
    }

    -- Send config to alpha
    alpha.setup(dashboard.opts)

    -- Disable folding on alpha buffer
    vim.cmd([[autocmd FileType alpha setlocal nofoldenable]])
  end,
}

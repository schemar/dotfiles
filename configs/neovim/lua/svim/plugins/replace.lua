return {
  "nvim-pack/nvim-spectre",
  dependencies = { "nvim-lua/plenary.nvim" },
  lazy = true,
  cmd = "Spectre",
  keys = {
    {
      "<leader>sr",
      "<cmd>lua require('spectre').open()<CR>",
      desc = "Replace in files",
    },
    {
      "<leader>sR",
      "<cmd>lua require('spectre').open_visual({select_word=true})<CR>",
      desc = "Replace word under cursor",
    },
    {
      "<leader>sf",
      "<cmd>lua require('spectre').open_file_search()<CR>",
      desc = "Replace in current file",
    },
  },
  config = function()
    require("spectre").setup({
      default = {
        find = {
          cmd = "rg",
          options = { "ignore-case" },
        },
        replace = {
          cmd = "sd",
        },
      },
      use_trouble_qf = true,
      mapping = {
        ["q"] = {
          map = "q",
          cmd = "<cmd>lua require('spectre').close()<cr>",
          desc = "close",
        },
        ["tab"] = {
          map = "<Tab>",
          cmd = "<cmd>lua require('spectre').tab()<cr>",
          desc = "next query",
        },
        ["shift-tab"] = {
          map = "<S-Tab>",
          cmd = "<cmd>lua require('spectre').tab_shift()<cr>",
          desc = "previous query",
        },
        ["toggle_line"] = {
          map = "dd",
          cmd = "<cmd>lua require('spectre').toggle_line()<CR>",
          desc = "toggle item",
        },
        ["enter_file"] = {
          map = "<cr>",
          cmd = "<cmd>lua require('spectre.actions').select_entry()<CR>",
          desc = "open file",
        },
        ["send_to_qf"] = {
          map = "<leader>t",
          cmd = "<cmd>lua require('spectre.actions').send_to_qf()<CR>",
          desc = "send all items to trouble",
        },
        ["replace_cmd"] = {
          map = "<leader>c",
          cmd = "<cmd>lua require('spectre.actions').replace_cmd()<CR>",
          desc = "input replace command",
        },
        ["show_option_menu"] = {
          map = "<leader>o",
          cmd = "<cmd>lua require('spectre').show_options()<CR>",
          desc = "show options",
        },
        ["run_current_replace"] = {
          map = "<leader>r",
          cmd = "<cmd>lua require('spectre.actions').run_current_replace()<CR>",
          desc = "replace current line",
        },
        ["run_replace"] = {
          map = "<leader>R",
          cmd = "<cmd>lua require('spectre.actions').run_replace()<CR>",
          desc = "replace all",
        },
        ["change_view_mode"] = {
          map = "<leader>v",
          cmd = "<cmd>lua require('spectre').change_view()<CR>",
          desc = "change result view mode",
        },
        ["toggle_live_update"] = {
          map = "tu",
          cmd = "<cmd>lua require('spectre').toggle_live_update()<CR>",
          desc = "update when vim writes to file",
        },
        ["toggle_ignore_case"] = {
          map = "ti",
          cmd = "<cmd>lua require('spectre').change_options('ignore-case')<CR>",
          desc = "toggle ignore case",
        },
        ["toggle_ignore_hidden"] = {
          map = "th",
          cmd = "<cmd>lua require('spectre').change_options('hidden')<CR>",
          desc = "toggle search hidden",
        },
        ["resume_last_search"] = {
          map = "<leader>l",
          cmd = "<cmd>lua require('spectre').resume_last_search()<CR>",
          desc = "repeat last search",
        },
        ["delete_line"] = {
          map = "<leader>d",
          cmd = "<cmd>lua require('spectre.actions').run_delete_line()<CR>",
          desc = "delete line",
        },
        -- you can put your mapping here it only use normal mode
      },
    })
  end,
}

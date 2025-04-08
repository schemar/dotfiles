return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      preset = "helix",
      icons = {
        mappings = false,
      },
    },
  },
  {
    "folke/trouble.nvim",
    dependencies = { "folke/todo-comments.nvim" },
    opts = {
      auto_jump = true, -- auto jump to the item when there's only one
      auto_preview = true, -- automatically open preview when going through items in Trouble
      auto_refresh = false, -- auto refresh when open (update which references to show in Trouble when going to another item in the buffer)
      follow = true, -- follow the current item (move in Trouble list when going to another item in the buffer)
    }, -- for default options, refer to the configuration section for custom setup.
    cmd = "Trouble",
  },
  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "folke/trouble.nvim",
      "nvim-telescope/telescope-ui-select.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim", -- FZF algorithm for telescope
        build = "make",
      },
      {
        "danielfalk/smart-open.nvim",
        branch = "0.2.x",
        dependencies = {
          "kkharji/sqlite.lua",
          "nvim-tree/nvim-web-devicons",
          "nvim-telescope/telescope-fzf-native.nvim",
        },
      },
      "debugloop/telescope-undo.nvim",
      "pfdzm/graphite-picker",
    },
    cmd = { "Telescope" },
    config = function()
      local open_with_trouble = require("trouble.sources.telescope").open

      local telescope = require("telescope")
      local actions = require("telescope.actions")

      telescope.setup({
        defaults = {
          mappings = {
            i = {
              ["<c-j>"] = actions.move_selection_next,
              ["<c-k>"] = actions.move_selection_previous,
              ["<esc>"] = actions.close, -- Close on first press of esc. No "normal" mode.
              ["<c-t>"] = open_with_trouble,
            },
            n = { ["<c-t>"] = open_with_trouble },
          },
          -- Themeing
          sorting_strategy = "ascending", -- List from the top down
          layout_strategy = "horizontal",
          layout_config = {
            prompt_position = "top",
          },
        },
        pickers = {
          live_grep = {
            mappings = {
              -- Press `<c-f>` to restart search within the current results
              i = { ["<c-f>"] = actions.to_fuzzy_refine },
            },
          },
        },
        extensions = {
          fzf = {
            fuzzy = true, -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
            -- the default case_mode is "smart_case"
          },
          smart_open = {
            match_algorithm = "fzf",
            disable_devicons = false,
          },
          undo = {},
          ["ui-select"] = {
            require("telescope.themes").get_dropdown({}),
          },
        },
      })

      -- To get fzf loaded and working with telescope, you need to call
      -- load_extension, somewhere after setup function:
      require("telescope").load_extension("fzf")
      require("telescope").load_extension("smart_open")
      require("telescope").load_extension("undo")
      require("telescope").load_extension("graphite_picker")
      -- To get ui-select loaded and working with telescope, you need to call
      -- load_extension, somewhere after setup function:
      require("telescope").load_extension("ui-select")
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        theme = "catppuccin",
        disabled_filetypes = {
          statusline = {
            "NeogitStatus",
          },
          winbar = {},
        },
        -- When set to true, if you have configured lualine for displaying tabline
        -- then tabline will always show. If set to false, then tabline will be displayed
        -- only when there are more than 1 tab. (see :h showtabline)
        always_show_tabline = false,
        -- Single line for entire window, disregarding window splits.
        globalstatus = true,
      },
      sections = {
        lualine_a = { "mode", "searchcount" },
        lualine_b = { "diff", "diagnostics" },
        lualine_c = { { "filename", path = 1 } },
        lualine_x = {
          {
            "branch",
            fmt = function(str)
              if string.len(str) < 20 then
                return str
              else
                return string.sub(str, 0, 9) .. "..." .. string.sub(str, string.len(str) - 8)
              end
            end,
          },
          "filetype",
        },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { { "filename", path = 1 } },
        lualine_x = { "location" },
        lualine_y = {},
        lualine_z = {},
      },
      tabline = {
        lualine_a = { { "tabs", mode = 2, max_length = vim.o.columns } },
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
      extenstions = {
        "aerial",
        "lazy",
        "man",
        "mason",
        "nvim-tree",
        "trouble",
      },
    },
  },
  {
    "stevearc/aerial.nvim",
    -- Optional dependencies
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    cmd = {
      "AerialToggle",
      "AerialOpen",
      "AerialOpenAll",
      "AerialNavToggle",
      "AerialNavOpen",
    },
    config = function()
      local icons = vim.deepcopy(require("schemar.config.options").icons.kinds)
      -- Fix lua's weird choice for `Package` for control
      -- structures like if/else/for/etc.
      icons.lua = { Package = icons.Control }

      require("aerial").setup({
        attach_mode = "global",
        backends = { "treesitter", "lsp", "markdown", "asciidoc", "man" },
        close_automatic_events = { "switch_buffer" },
        icons = icons,
        show_guides = true,
        guides = {
          mid_item = "├╴",
          last_item = "└╴",
          nested_top = "│ ",
          whitespace = "  ",
        },
        filter_kind = false,
        -- Keymaps in aerial window. Can be any value that `vim.keymap.set` accepts OR a table of keymap
        -- options with a `callback` (e.g. { callback = function() ... end, desc = "", nowait = true })
        -- Additionally, if it is a string that matches "actions.<name>",
        -- it will use the mapping at require("aerial.actions").<name>
        -- Set to `false` to remove a keymap
        keymaps = {
          ["<TAB>"] = "actions.scroll",
          ["o"] = {
            callback = function()
              -- Temporarily set close_on_select to true so that aerial
              -- closes when we jump to a definition.
              require("aerial.config").close_on_select = true
              require("aerial").select()
              require("aerial.config").close_on_select = false
            end,
            desc = "Jump and quit",
            nowait = true,
          },
        },
        nav = {
          keymaps = {
            ["<CR>"] = "actions.jump",
            ["o"] = "actions.jump",
            ["<2-LeftMouse>"] = "actions.jump",
            ["<C-s>"] = "actions.jump_vsplit",
            ["<C-v>"] = "actions.jump_split",
            ["h"] = "actions.left",
            ["l"] = "actions.right",
            ["q"] = "actions.close",
            ["<C-c>"] = "actions.close",
            ["<C-e>"] = "actions.close",
          },
        },
      })
    end,
  },
}

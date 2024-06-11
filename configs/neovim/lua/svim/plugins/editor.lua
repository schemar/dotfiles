return {
  {
    "folke/trouble.nvim",
    dependencies = { "folke/todo-comments.nvim" },
    opts = {}, -- for default options, refer to the configuration section for custom setup.
    cmd = "Trouble",
    keys = {
      {
        "<leader>ld",
        "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics",
      },
      {
        "<leader>lD",
        "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
        desc = "Buffer Diagnostics",
      },
      {
        "<leader>lo",
        "<cmd>Trouble symbols toggle focus=false<cr>",
        desc = "Symbols",
      },
      {
        "<leader>ll",
        "<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
        desc = "LSP definitions, references, ...",
      },
      {
        "<leader>tt",
        "<cmd>Trouble todo focus=true<cr>", -- Trouble todo filter = {tag = {TODO,FIX,FIXME}}
        desc = "Todos",
      },
      {
        "<leader>tm",
        "<cmd>Trouble todo focus=true filter = {tag = {TODOMS}}<cr>", -- Trouble todo filter = {tag = {TODO,FIX,FIXME}}
        desc = "Todos MS",
      },
      {
        "<leader>tb",
        "<cmd>Trouble todo focus=true filter = {buf = 0}<cr>", -- Trouble todo filter = {tag = {TODO,FIX,FIXME}}
        desc = "Todos (curr. buffer)",
      },
      {
        "gd",
        "<cmd>Trouble lsp_definitions toggle focus=true<cr>",
        desc = "Definitions",
      },
      {
        "gD",
        "<cmd>Trouble lsp_type_definitions toggle focus=true<cr>",
        desc = "Type definitions",
      },
      {
        "gr",
        "<cmd>Trouble lsp_references toggle focus=true<cr>",
        desc = "References",
      },
    },
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    event = { "VeryLazy" },
    keys = {
      {
        "]t",
        function()
          require("todo-comments").jump_next()
        end,
        desc = "Next todo comment",
      },
      {
        "[t",
        function()
          require("todo-comments").jump_prev()
        end,
        desc = "Previous todo comment",
      },
    },
    opts = {
      keywords = {
        TODOMS = { icon = " ", color = "info" },
      },
      highlight = {
        -- Remove required trailing colon:
        pattern = [[.*<(KEYWORDS)\s*]], -- pattern or table of patterns, used for highlighting (vim regex)
      },
      search = {
        -- regex that will be used to match keywords.
        -- don't replace the (KEYWORDS) placeholder
        -- pattern = [[\b(KEYWORDS):]], -- ripgrep regex
        pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You'll likely get false positives
      },
    },
  },
  {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons", "folke/trouble.nvim" },
    config = function()
      -- Open results in Trouble:
      local config = require("fzf-lua.config")
      local actions = require("trouble.sources.fzf").actions
      config.defaults.actions.files["ctrl-t"] = actions.open

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
      require("fzf-lua").setup({ "default", fzf_colors = true })
    end,
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
        "<leader>?",
        function()
          require("fzf-lua").grep()
        end,
        desc = "Grep Project and FZF",
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
        "<leader>la",
        function()
          require("fzf-lua").lsp_code_actions()
        end,
        desc = "Code actions",
      },
    },
  },
  {
    "christoomey/vim-tmux-navigator", -- Switch windows/panes vim/tmux
    event = { "VeryLazy" },
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    opts = {
      indent = {
        char = "│", -- Examples: │ ┃ ┊ ┆ ┇ ┋ ╏
      },
      -- scope = {
      -- 	enabled = false,
      -- },
    },
  },
  {
    -- Auto-disable hlsearch when moving cursor
    "asiryk/auto-hlsearch.nvim",
    event = "VeryLazy",
    name = "auto-hlsearch",
    config = true,
    version = "*",
  },
  {
    "rgroli/other.nvim", -- Go to alternative file, e.g. ts<->vue or test
    name = "other-nvim",
    event = { "BufReadPost", "BufNewFile" },
    keys = {
      { "<leader>o", "<cmd>Other<cr>", desc = "Open 'other' file" },
      { "<leader>O", "<cmd>OtherClear<cr><cmd>Other<cr>", desc = "Open 'other' file (clear)" },
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
  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    keys = {
      {
        "<leader>e",
        "<cmd>NvimTreeToggle<cr>",
        desc = "Explorer",
      },
      {
        "<leader>E",
        "<cmd>NvimTreeFindFileToggle<cr>",
        desc = "Explorer (curr. dir.)",
      },
    },
    config = function()
      local function on_attach(bufnr)
        local api = require("nvim-tree.api")
        local function opts(desc)
          return {
            desc = "nvim-tree: " .. desc,
            buffer = bufnr,
            noremap = true,
            silent = true,
            nowait = true,
          }
        end

        -- BEGIN_DEFAULT_ON_ATTACH
        vim.keymap.set("n", "<C-]>", api.tree.change_root_to_node, opts("CD"))
        vim.keymap.set("n", "<C-e>", api.node.open.replace_tree_buffer, opts("Open: In Place"))
        vim.keymap.set("n", "<C-k>", api.node.show_info_popup, opts("Info"))
        vim.keymap.set("n", "<C-r>", api.fs.rename_sub, opts("Rename: Omit Filename"))
        vim.keymap.set("n", "<C-t>", api.node.open.tab, opts("Open: New Tab"))
        vim.keymap.set("n", "<C-v>", api.node.open.vertical, opts("Open: Vertical Split"))
        vim.keymap.set("n", "<C-x>", api.node.open.horizontal, opts("Open: Horizontal Split"))
        vim.keymap.set("n", "<BS>", api.node.navigate.parent_close, opts("Close Directory"))
        vim.keymap.set("n", "<CR>", api.node.open.edit, opts("Open"))
        vim.keymap.set("n", "<Tab>", api.node.open.preview, opts("Open Preview"))
        vim.keymap.set("n", ">", api.node.navigate.sibling.next, opts("Next Sibling"))
        vim.keymap.set("n", "<", api.node.navigate.sibling.prev, opts("Previous Sibling"))
        vim.keymap.set("n", ".", api.node.run.cmd, opts("Run Command"))
        vim.keymap.set("n", "-", api.tree.change_root_to_parent, opts("Up"))
        vim.keymap.set("n", "a", api.fs.create, opts("Create"))
        vim.keymap.set("n", "bmv", api.marks.bulk.move, opts("Move Bookmarked"))
        vim.keymap.set("n", "B", api.tree.toggle_no_buffer_filter, opts("Toggle No Buffer"))
        vim.keymap.set("n", "c", api.fs.copy.node, opts("Copy"))
        vim.keymap.set("n", "C", api.tree.toggle_git_clean_filter, opts("Toggle Git Clean"))
        vim.keymap.set("n", "[c", api.node.navigate.git.prev, opts("Prev Git"))
        vim.keymap.set("n", "]c", api.node.navigate.git.next, opts("Next Git"))
        vim.keymap.set("n", "d", api.fs.remove, opts("Delete"))
        vim.keymap.set("n", "D", api.fs.trash, opts("Trash"))
        vim.keymap.set("n", "E", api.tree.expand_all, opts("Expand All"))
        vim.keymap.set("n", "e", api.fs.rename_basename, opts("Rename: Basename"))
        vim.keymap.set("n", "]e", api.node.navigate.diagnostics.next, opts("Next Diagnostic"))
        vim.keymap.set("n", "[e", api.node.navigate.diagnostics.prev, opts("Prev Diagnostic"))
        vim.keymap.set("n", "F", api.live_filter.clear, opts("Clean Filter"))
        vim.keymap.set("n", "f", api.live_filter.start, opts("Filter"))
        vim.keymap.set("n", "g?", api.tree.toggle_help, opts("Help"))
        vim.keymap.set("n", "gy", api.fs.copy.absolute_path, opts("Copy Absolute Path"))
        vim.keymap.set("n", "H", api.tree.toggle_hidden_filter, opts("Toggle Dotfiles"))
        vim.keymap.set("n", "I", api.tree.toggle_gitignore_filter, opts("Toggle Git Ignore"))
        vim.keymap.set("n", "J", api.node.navigate.sibling.last, opts("Last Sibling"))
        vim.keymap.set("n", "K", api.node.navigate.sibling.first, opts("First Sibling"))
        vim.keymap.set("n", "m", api.marks.toggle, opts("Toggle Bookmark"))
        vim.keymap.set("n", "o", api.node.open.edit, opts("Open"))
        vim.keymap.set("n", "O", api.node.open.no_window_picker, opts("Open: No Window Picker"))
        vim.keymap.set("n", "p", api.fs.paste, opts("Paste"))
        vim.keymap.set("n", "P", api.node.navigate.parent, opts("Parent Directory"))
        vim.keymap.set("n", "q", api.tree.close, opts("Close"))
        vim.keymap.set("n", "r", api.fs.rename, opts("Rename"))
        vim.keymap.set("n", "R", api.tree.reload, opts("Refresh"))
        vim.keymap.set("n", "s", api.node.run.system, opts("Run System"))
        vim.keymap.set("n", "S", api.tree.search_node, opts("Search"))
        vim.keymap.set("n", "U", api.tree.toggle_custom_filter, opts("Toggle Hidden"))
        vim.keymap.set("n", "W", api.tree.collapse_all, opts("Collapse"))
        vim.keymap.set("n", "x", api.fs.cut, opts("Cut"))
        vim.keymap.set("n", "y", api.fs.copy.filename, opts("Copy Name"))
        vim.keymap.set("n", "Y", api.fs.copy.relative_path, opts("Copy Relative Path"))
        vim.keymap.set("n", "<2-LeftMouse>", api.node.open.edit, opts("Open"))
        vim.keymap.set("n", "<2-RightMouse>", api.tree.change_root_to_node, opts("CD"))
        -- END_DEFAULT_ON_ATTACH

        vim.keymap.set("n", "l", api.node.open.edit, opts("Open"))
        vim.keymap.set("n", "h", api.node.navigate.parent_close, opts("Close Directory"))
        vim.keymap.set("n", "o", function()
          api.node.open.edit()
          api.tree.close()
        end, opts("edit_and_close"))
      end

      local icons = require("svim.config").icons.git
      require("nvim-tree").setup({
        on_attach = on_attach,
        renderer = {
          icons = {
            git_placement = "after",
            glyphs = {
              git = {
                unstaged = icons.FileUnstaged,
                staged = icons.FileStaged,
                unmerged = icons.FileUnmerged,
                renamed = icons.FileRenamed,
                untracked = icons.FileUntracked,
                deleted = icons.FileDeleted,
                ignored = icons.FileIgnored,
              },
            },
          },
        },
        actions = {
          open_file = {
            quit_on_open = false,
          },
        },
      })
    end,
  },
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = true,
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = true,
  },
}

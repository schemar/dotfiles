{ ... }:
{
  programs.nixvim.plugins.which-key = {
    enable = true;

    settings = {
      delay = 300;
    };

    luaConfig.post = # lua
      ''
        -- Philosophy:
        -- - Float and pop-up windows (also sticky like nvim-tree):
        --   - `<cr>` to open and go to, but keep the window open
        --   - `o` to open and go to, and close the window
        --   - `tab` to open (preview), but keep cursor in the current window
        --   - `q` to close
        --   - `<c-e>` to close in insert mode
        --   - `<c-j/k>` to navigate
        local wk = require("which-key")

        wk.add({
          {
            "]",
            group = "Next ...",
            {
              {
                "]D",
                function()
                  vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR })
                end,
                desc = "Next error diagnostic",
              },
              {
                "]t",
                function()
                  require("todo-comments").jump_next()
                end,
                desc = "Next todo comment",
              },
              {
                "]w",
                function()
                  require("snacks").words.jump(1, true)
                end,
                desc = "Next matching word",
              },
            },
          },
          {
            "[",
            group = "Previous ...",
            {
              {
                "[D",
                function()
                  vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR })
                end,
                desc = "Previous error diagnostic",
              },
              {
                "[t",
                function()
                  require("todo-comments").jump_prev()
                end,
                desc = "Previous todo comment",
              },
            },
          },
          {
            "<c-j>",
            function()
              if is_hover_open() then
                require("hover").hover_switch("next")
              else
                -- Do something else when hover not open?
              end
            end,
            desc = "Hover next",
          },
          {
            "<c-k>",
            function()
              if is_hover_open() then
                require("hover").hover_switch("previous")
              else
                -- Do something else when hover not open?
              end
            end,
            desc = "Hover previous",
          },
          -- ⚠️ When creating the flash keymaps manually either use a lua function like
          -- `function() require("flash").jump() end` as the rhs, or a string like
          -- `<cmd>lua require("flash").jump()<cr>`.
          -- DO NOT use `:lua`, since that will break dot-repeat.
          {
            "<c-s>",
            mode = { "n", "x", "o" },
            function()
              require("flash").jump()
            end,
            desc = "Flash",
          },
          {
            "<c-s>",
            mode = { "c" },
            function()
              require("flash").toggle()
            end,
            desc = "Toggle Flash Search",
          },
          {
            "<c-t>",
            mode = { "n", "x", "o", "i", "t" },
            "<cmd>ToggleTerm direction=float<cr>",
            desc = "Toggle Terminal",
          },
          {
            "<c-w>",
            group = "Window",
            {
              {
                "<c-w>s",
                "<cmd>vsplit<cr>",
                desc = "Split window",
              },
              {
                "<c-w>v",
                "<cmd>split<cr>",
                desc = "Split window verticallly",
              },
            },
          },
          {
            "<c-x>",
            mode = { "n", "x", "o" },
            function()
              require("flash").treesitter()
            end,
            desc = "Flash Treesitter",
          },
          { "<m-h>", "<cmd>TmuxNavigateLeft<cr>" },
          { "<m-j>", "<cmd>TmuxNavigateDown<cr>" },
          { "<m-k>", "<cmd>TmuxNavigateUp<cr>" },
          { "<m-l>", "<cmd>TmuxNavigateRight<cr>" },
          { "<m-\\>", "<cmd>TmuxNavigatePrevious<cr>" },
          {
            "<m-n>",
            "<cmd>tabnext<cr>",
            desc = "Next",
          },
          {
            "<m-p>",
            "<cmd>tabprevious<cr>",
            desc = "Previous",
          },
          {
            "<m-s>",
            "<cmd>vsplit<cr>",
            desc = "Split window",
          },
          {
            "gd",
            "<cmd>Trouble lsp_definitions focus=true auto_refresh=false<cr>",
            desc = "Definitions",
          },
          {
            "gD",
            "<cmd>Trouble lsp_type_definitions focus=true auto_refresh=false<cr>",
            desc = "Type definitions",
          },
          {
            "gr",
            "<cmd>Trouble lsp_references focus=true auto_refresh=false<cr>",
            desc = "References",
          },
          {
            "K",
            function()
              -- vim.lsp.buf.hover({ border = require("schemar.config.options").border })
              require("hover").hover()
            end,
            desc = "Hover",
          },
          {
            "r",
            mode = { "o" },
            function()
              require("flash").remote()
            end,
            desc = "Remote Flash",
          },
          {
            "R",
            mode = { "o", "x" },
            function()
              require("flash").treesitter_search()
            end,
            desc = "Treesitter Search",
          },
          {
            "zM",
            function()
              require("ufo").closeAllFolds()
            end,
            desc = "Close all folds",
          },
          {
            "zr",
            function()
              require("ufo").closeFoldsWith()
            end,
            desc = "Open all folds",
          },
          {
            "zR",
            function()
              require("ufo").openAllFolds()
            end,
            desc = "Open all folds",
          },
          {
            "<leader>",
            group = "Leader",
            {
              {
                "<leader><leader>",
                "<cmd>Telescope smart_open<cr>",
                desc = "Open Files (smart)",
              },
              {
                "<leader>/",
                "<cmd>Telescope live_grep<cr>",
                desc = "Grep Project",
              },
              {
                "<leader>*",
                "<cmd>Telescope grep_string<cr>",
                desc = "Grep project (selection)",
              },
              {
                "<leader>b",
                group = "Buffers",
                {
                  {
                    "<leader>b/",
                    "<cmd>Telescope current_buffer_fuzzy_find<cr>",
                    desc = "Fuzzy find in buffer",
                  },
                  {
                    "<leader>bb",
                    "<cmd>Telescope buffers<cr>",
                    desc = "List buffers",
                  },
                  {
                    "<leader>bd",
                    function()
                      Snacks.bufdelete()
                    end,
                    desc = "Delete buffer",
                  },
                  {
                    "<leader>bD",
                    function()
                      Snacks.bufdelete.other()
                    end,
                    desc = "Delete other buffers",
                  },
                },
              },
              {
                "<leader>f",
                group = "Files",
                {
                  {
                    "<leader>Fe",
                    "<cmd>NvimTreeToggle<cr>",
                    desc = "Explorer",
                  },
                  {
                    "<leader>FE",
                    "<cmd>NvimTreeFindFile<cr>",
                    desc = "Explorer (curr. dir.)",
                  },
                  {
                    "<leader>fe",
                    "<cmd>Yazi<cr>",
                    desc = "Explorer (ccurr. file)",
                  },
                  {
                    "<leader>fE",
                    "<cmd>Yazi cwd<cr>",
                    desc = "Explorer (root)",
                  },
                  {
                    "<leader>ff",
                    "<cmd>Telescope find_files<cr>",
                    desc = "Files",
                  },
                  {
                    "<leader>fF",
                    "<cmd>Telescope oldfiles<cr>",
                    desc = "Files (opened)",
                  },
                  {
                    "<leader>fo",
                    "<cmd>Other<cr>",
                    desc = "Open 'other' file",
                  },
                  {
                    "<leader>fO",
                    "<cmd>OtherClear<cr><cmd>Other<cr>",
                    desc = "Open 'other' file (clear)",
                  },
                  {
                    "<leader>fy",
                    "<cmd>let @+ = expand('%')<cr>",
                    desc = "Yank relative file path",
                  },
                  {
                    "<leader>fY",
                    "<cmd>let @+ = expand('%:p')<cr>",
                    desc = "Yank absolute file path",
                  },
                },
              },
              {
                -- See also M.git_attach for further git mappings.
                "<leader>g",
                group = "Git",
                {
                  { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "Diffview" },
                  { "<leader>gf", "<cmd>DiffviewFileHistory %<cr>", desc = "File history" },
                  {
                    "<leader>gg",
                    function()
                      require("neogit").open()
                    end,
                    desc = "Neogit",
                  },
                  {
                    "<leader>gG",
                    -- Lazygit integration without extra plugins
                    -- ========================================

                    -- open lazygit in a new tab (terminal)
                    -- and auto-close that tab when lazygit exits
                    function()
                      -- Open a fresh tab and remember its buffer
                      vim.cmd('tabnew')
                      local buf = vim.api.nvim_get_current_buf()

                      -- Start lazygit inside a terminal
                      vim.fn.termopen('lazygit --use-config-file="$(\\lazygit --print-config-dir)/config.yml,$HOME/.config/lazygit/blueberry_peach_$(~/.config/current_theme).yml"', {
                        on_exit = function()
                          -- Defer buffer deletion to avoid messing with termopen's event loop
                          vim.schedule(function()
                            if vim.api.nvim_buf_is_valid(buf) then
                              -- Delete the terminal buffer; if it's the only window in the tab,
                              -- the tab will close as well.
                              pcall(vim.api.nvim_buf_delete, buf, { force = true })
                            end
                          end)
                        end,
                      })

                      -- Jump straight into terminal insert mode
                      vim.cmd('startinsert')
                    end,
                    desc = "Lazygit",
                  }, 
                  { "<leader>gh", "<cmd>DiffviewFileHistory<cr>", desc = "Branch history" },
                  {
                    "<leader>gt",
                    group = "Graphite",
                    {
                      {
                        "<leader>gtl",
                        "<cmd>!gt ls<cr>",
                        desc = "List stacks",
                      },
                      {
                        "<leader>gtc",
                        function()
                          split_term("gt create")
                        end,
                        desc = "Create",
                      },
                      {
                        "<leader>gts",
                        function()
                          split_term_and_edit("gt sync")
                        end,
                        desc = "Sync",
                      },
                      {
                        "<leader>gtp",
                        function()
                          split_term("gt submit --no-edit")
                        end,
                        desc = "Submit",
                      },
                      {
                        "<leader>gtP",
                        function()
                          split_term("gt submit --no-edit --publish")
                        end,
                        desc = "Submit (publish)",
                      },
                      {
                        "<leader>gtm",
                        function()
                          split_term("gt modify")
                        end,
                        desc = "Modify",
                      },
                      {
                        "<leader>gtM",
                        function()
                          split_term_and_edit("gt modify && gt sync && gt submit --no-edit")
                        end,
                        desc = "Modify, Sync, Submit",
                      },
                      {
                        "<leader>gto",
                        function()
                          split_term_and_edit("gt checkout")
                        end,
                        desc = "Checkout",
                      },
                      {
                        "<leader>gtu",
                        function()
                          gt_command_ls("up")
                        end,
                        desc = "Up",
                      },
                      {
                        "<leader>gtd",
                        function()
                          gt_command_ls("down")
                        end,
                        desc = "Down",
                      },
                    },
                  },
                },
              },
              {
                "<leader>j",
                group = "Tabs",
                {
                  {
                    "<leader>jc",
                    "<cmd>tabnew<cr>",
                    desc = "Create",
                  },
                  {
                    "<leader>jj",
                    "<cmd>tabnext<cr>",
                    desc = "Next",
                  },
                  {
                    "<leader>jJ",
                    "<cmd>tablast<cr>",
                    desc = "Last",
                  },
                  {
                    "<leader>jk",
                    "<cmd>tabprevious<cr>",
                    desc = "Previous",
                  },
                  {
                    "<leader>jK",
                    "<cmd>tabfirst<cr>",
                    desc = "First",
                  },
                  {
                    "<leader>jd",
                    "<cmd>tabclose<cr>",
                    desc = "Delete",
                  },
                },
              },
              {
                "<leader>l",
                group = "Code",
                {
                  {
                    "<leader>la",
                    vim.lsp.buf.code_action,
                    desc = "Code actions",
                  },
                  { "<leader>lc", "<cmd>Copilot<cr>", desc = "Copilot completions" },
                  {
                    "<leader>ld",
                    "<cmd>Trouble diagnostics focus=true<cr>",
                    desc = "Diagnostics",
                  },
                  {
                    "<leader>lD",
                    "<cmd>Trouble diagnostics filter.buf=0 focus=true<cr>",
                    desc = "Buffer Diagnostics",
                  },
                  {
                    "<leader>le",
                    vim.diagnostic.open_float,
                    desc = "Floating diagnostic",
                  },
                  {
                    -- Defined in plugins/debug.lua
                    "<leader>lg",
                    group = "Debug",
                    {
                      "<leader>lgs",
                      group = "Debug surround",
                    },
                  },
                  {
                    "<leader>ll",
                    "<cmd>Trouble lsp focus=false win={position=right, size=0.4, relative=win}<cr>",
                    desc = "LSP definitions, references, ...",
                  },
                  { "<leader>lo", "<cmd>AerialToggle<cr>", desc = "Aerial (Symbols)" },
                  { "<leader>lO", "<cmd>AerialNavToggle<cr>", desc = "Aerial (Symbols)" },
                  { "<leader>lr", vim.lsp.buf.rename, desc = "Rename" },
                  {
                    "<leader>ls",
                    "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
                    desc = "Workspace symbols",
                  },
                },
              },
              {
                "<leader>m",
                "<cmd>Telescope marks<cr>",
                desc = "Marks",
              },
              {
                "<leader>r",
                "<cmd>Telescope resume<cr>",
                desc = "Resume Telescope",
              },
              {
                "<leader>s",
                group = "Terminal",
                {
                  {
                    "<leader>ss",
                    "<cmd>ToggleTerm direction=float<cr>",
                    desc = "Split terminal",
                  },
                },
              },
              {
                "<leader>t",
                desc = "Todos",
                {
                  {
                    "<leader>tt",
                    "<cmd>Trouble todo focus=true<cr>", -- Trouble todo filter = {tag = {TODO,FIX,FIXME}}
                    desc = "Todos",
                  },
                  {
                    "<leader>tm",
                    "<cmd>Trouble todo filter.tag={TODOMS} focus=true<cr>", -- Trouble todo filter = {tag = {TODO,FIX,FIXME}}
                    desc = "Todos MS",
                  },
                  {
                    "<leader>tb",
                    "<cmd>Trouble todo filter.buf=0 focus=true<cr>", -- Trouble todo filter = {tag = {TODO,FIX,FIXME}}
                    desc = "Todos (curr. buffer)",
                  },
                },
              },
            },
          },
        })
      '';
  };
}

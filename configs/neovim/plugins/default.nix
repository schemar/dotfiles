{ pkgs, ... }:
{
  programs.nixvim =
    let
      border = "single";
    in
    {

      extraPlugins = [
        pkgs.vimPlugins.other-nvim
        pkgs.vimPlugins.smart-open-nvim
        pkgs.vimPlugins.hover-nvim
        pkgs.vimPlugins.auto-hlsearch-nvim
      ];

      extraConfigLua = # lua
        ''
          require("other-nvim").setup({
            -- Should the window show files which do not exist yet based on
            -- pattern matching. Selecting the files will create the file.
            showMissingFiles = false,
            style = {
              border = "${border}",
            },
            -- Map files to other files.
            -- See documentation for details and more options.
            mappings = {
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.vue",
                context = "vue",
              },
              {
                pattern = "(.*)/(.*).vue",
                target = "%1/%2.ts",
                context = "vue",
              },
              {
                pattern = "(.*)/(.*).vue.gen.ts",
                target = "%1/%2.vue",
                context = "gen",
              },
              {
                pattern = "(.*)/(.*).vue",
                target = "%1/%2.vue.gen.ts",
                context = "gen",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.spec.ts",
                context = "spec",
              },
              {
                pattern = "(.*)/(.*).spec.ts",
                target = "%1/%2.ts",
                context = "spec",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.unit.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).unit.test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.firestore.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).firestore.test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.temporal.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).temporal.test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
            },
          })

          require("hover").setup({
            init = function()
              -- Require providers
              require("hover.providers.lsp")
              -- require("hover.providers.gh")
              -- require("hover.providers.gh_user")
              -- require('hover.providers.jira')
              -- require('hover.providers.dap')
              require("hover.providers.fold_preview")
              require("hover.providers.diagnostic")
              require("hover.providers.man")
              -- require('hover.providers.dictionary')
              -- require("hover.providers.highlight")
            end,
            preview_opts = {
              border = "${border}",
            },
            -- Whether the contents of a currently open hover window should be moved
            -- to a :h preview-window when pressing the hover keymap.
            preview_window = false,
            title = true,
            mouse_providers = {},
          })

          require("auto-hlsearch").setup()
        '';

      plugins = {
        web-devicons.enable = true;
        indent-blankline = {
          enable = true;

          settings = {
            indent = {
              char = "┃";
            };
            scope = {
              show_start = false;
              show_end = false;
            };
          };
        };
        illuminate = {
          enable = true;
        };
        marks = {
          enable = true;
        };
        tmux-navigator = {
          enable = true;

          settings = {
            no_mappings = 1;
          };
        };
        which-key = {
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
        yazi = {
          enable = true;
        };
        nvim-tree = {
          enable = true;

          settings = {
            actions = {
              open_file = {
                quit_on_open = false;
              };
            };
            renderer = {
              icons = {
                git_placement = "after";
                glyphs = {
                  folder = {
                    arrow_open.__raw = "icons.ui.ChevronShortDown";
                    arrow_closed.__raw = "icons.ui.ChevronShortRight";
                  };
                  git = {
                    unstaged.__raw = "icons.git.FileUnstaged";
                    staged.__raw = "icons.git.FileStaged";
                    unmerged.__raw = "icons.git.FileUnmerged";
                    renamed.__raw = "icons.git.FileRenamed";
                    untracked.__raw = "icons.git.FileUntracked";
                    deleted.__raw = "icons.git.FileDeleted";
                    ignored.__raw = "icons.git.FileIgnored";
                  };
                };
              };
            };

            on_attach.__raw = # lua
              ''
                function(bufnr)
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
                  vim.keymap.set("n", "o", function(node)
                    api.node.open.edit(node)
                    api.tree.close()
                  end, opts("edit_and_close"))
                end

              '';
          };
        };
        telescope = {
          enable = true;

          extensions.fzf-native.enable = true;
          extensions.ui-select.enable = true;
          enabledExtensions = [ "smart_open" ];

          luaConfig.pre = # lua
            ''
              local open_with_trouble = require("trouble.sources.telescope").open

              local telescope = require("telescope")
              local actions = require("telescope.actions")
            '';
          settings.__raw = # lua
            ''
              {
                defaults = {
                  mappings = {
                    i = {
                      ["<c-e>"] = actions.close,
                      ["<c-y>"] = actions.select_default,
                      ["<c-j>"] = actions.move_selection_next,
                      ["<c-k>"] = actions.move_selection_previous,
                      ["<c-t>"] = open_with_trouble,
                    },
                    n = {
                      ["<c-e>"] = actions.close,
                      ["<c-t>"] = open_with_trouble,
                      ["o"] = actions.select_default,
                      ["q"] = actions.close,
                    },
                  },
                  -- Themeing
                  sorting_strategy = "ascending", -- List from the top down
                  layout_strategy = "horizontal",
                  layout_config = {
                    prompt_position = "top",
                  },
                  borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" }
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
                    mappings = {
                      i = {
                        ["<c-w>"] = function()
                          vim.api.nvim_input("<c-s-w>")
                        end,
                      },
                    },
                  },
                  ["ui-select"] = {
                    require("telescope.themes").get_dropdown({}),
                  },
                },
              }
            '';
        };
        blink-cmp = {
          enable = true;
          settings.__raw = # lua
            ''
              {
                -- For details, see:
                -- 1. Recipes: https://cmp.saghen.dev/recipes.html
                -- 2. Sources: https://cmp.saghen.dev/configuration/sources.html

                -- 'default' for mappings similar to built-in completion
                -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
                -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
                -- See the full "keymap" documentation for information on defining your own keymap.
                --
                -- Most importantly, use <C-y> to accept the completion and <C-e> to cancel it.
                keymap = {
                  preset = "default",

                  ["<C-k>"] = { "select_prev", "fallback" },
                  ["<C-j>"] = { "select_next", "fallback" },
                  ["<CR>"] = { "accept", "fallback" },
                },

                appearance = {
                  -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
                  -- Adjusts spacing to ensure icons are aligned
                  nerd_font_variant = "mono",
                },

                completion = {
                  menu = {
                    border = "${border}",
                  },
                  documentation = {
                    auto_show = true,
                    auto_show_delay_ms = 0,
                    window = {
                      border = "${border}",
                    },
                  },
                  list = {
                    selection = {
                      -- When `true`, will automatically select the first item in the completion list
                      preselect = false,
                      -- preselect = function(ctx) return vim.bo.filetype ~= 'markdown' end,

                      -- When `true`, inserts the completion item automatically when selecting it
                      -- You may want to bind a key to the `cancel` command (default <C-e>) when using this option,
                      -- which will both undo the selection and hide the completion menu
                      auto_insert = true,
                      -- auto_insert = function(ctx) return vim.bo.filetype ~= 'markdown' end
                    },
                  },
                },

                signature = {
                  enabled = true,
                  window = {
                    border = "${border}",
                  },
                },

                -- Default list of enabled providers defined so that you can extend it
                -- elsewhere in your config, without redefining it, due to `opts_extend`
                sources = {
                  default = { "lsp", "path", "snippets", "buffer" },
                },

                cmdline = {
                  enabled = true,
                  keymap = {
                    preset = "cmdline",
                    ["<C-k>"] = { "select_prev", "fallback" },
                    ["<C-j>"] = { "select_next", "fallback" },
                  },
                  completion = {
                    list = {
                      selection = {
                        preselect = false,
                      },
                    },
                    menu = {
                      auto_show = true,
                    },
                  },
                },
              }
            '';
        };
        fidget = {
          enable = true;

          settings = {
            notification = {
              window = {
                winblend = 0;
              };
            };
          };
        };
        nvim-ufo = {
          enable = true;

          settings = {
            close_fold_kinds_for_ft = {
              default = [ "imports" ];
            };
          };
        };
        aerial = {
          enable = true;

          settings.__raw = # lua
            ''
              {
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
              }
            '';
        };
        codecompanion = {
          enable = true;
        };
        nvim-surround.enable = true;
        nvim-autopairs.enable = true;
        flash = {
          enable = true;

          settings = {
            incremental = true;
          };
        };
        copilot-vim.enable = true;
        todo-comments = {
          enable = true;
          settings = {
            keywords = {
              TODOMS = {
                icon = " ";
                color = "info";
              };
            };
          };
        };
        comment = {
          enable = true;
          settings = {
            pre_hook = "require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook()";
          };
        };
        ts-context-commentstring = {
          enable = true;

          settings = {
            enable_autocmd = false;
          };
        };
        treesitter = {
          enable = true;

          settings = {
            indent.enable = true;
            highlight = {
              enable = true;
              additional_vim_regex_highlighting = false;
            };
          };
        };
        treesitter-context.enable = true;
        ts-autotag.enable = true;
        trouble = {
          enable = true;

          settings.__raw = # lua
            ''
              {
                auto_jump = true, -- auto jump to the item when there's only one
                auto_preview = true, -- automatically open preview when going through items in Trouble
                auto_refresh = true, -- auto refresh when open (update which references to show in Trouble when going to another item in the buffer)
                follow = true, -- follow the current item (move in Trouble list when going to another item in the buffer)
                -- The LSP base mode for:
                -- * lsp_definitions, lsp_references, lsp_implementations
                -- * lsp_type_definitions, lsp_declarations, lsp_command
                lsp_base = {
                  params = {
                    --  include the current location in the results
                    include_current = true,
                  },
                },
                icons = {
                  indent = {
                    fold_open = icons.ui.ChevronShortDown,
                    fold_closed = icons.ui.ChevronShortRight,
                  },
                  fold_closed = icons.ui.Folder,
                  fold_open = icons.ui.FolderOpen,
                  kinds = icons.kinds,
                },
              }
            '';
        };
        lualine = {
          enable = true;

          settings.__raw = # lua
            ''
              {
                options = {
                  theme = "catppuccin",
                  component_separators = { left = "", right = "" },
                  section_separators = { left = "", right = "" },
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
                  globalstatus = false,
                },
                sections = {
                  lualine_a = { "mode", "searchcount" },
                  lualine_b = { "diff", "diagnostics" },
                  lualine_c = {
                    {
                      "filename",
                      path = 1,
                    },
                  },
                  lualine_x = {
                    -- {
                    --   "branch",
                    --   fmt = function(str)
                    --     if string.len(str) <= 15 then
                    --       return str
                    --     elseif string.len(str) <= 15 + 13 then
                    --       return string.sub(str, 13, string.len(str))
                    --     else
                    --       return string.sub(str, 13, 22) .. "…" .. string.sub(str, string.len(str) - 5)
                    --     end
                    --   end,
                    -- },
                    {
                      "filetype",
                      icon_only = true,
                    },
                  },
                  lualine_y = { "progress" },
                  lualine_z = { "location" },
                },
                inactive_sections = {
                  lualine_a = {},
                  lualine_b = {},
                  lualine_c = {
                    {
                      "filename",
                      path = 1,
                    },
                  },
                  lualine_x = {},
                  lualine_y = {},
                  lualine_z = { "location" },
                },
                tabline = {
                  lualine_a = {
                    {
                      "tabs",
                      mode = 2,
                      max_length = vim.o.columns,
                    },
                  },
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
              }
            '';
        };
        gitsigns = {
          enable = true;

          settings.__raw = # lua
            ''
              {
                current_line_blame = true,
                on_attach = function(buffer)
                  local gs = require("gitsigns")
                  
                  local function map(mode, l, r, desc)
                    vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
                  end
                  
                  map("n", "]h", function()
                    gs.nav_hunk("next")
                  end, "Next Hunk")
                  map("n", "[h", function()
                    gs.nav_hunk("prev")
                  end, "Prev Hunk")
                  map("n", "]H", function()
                    gs.nav_hunk("last")
                  end, "Last Hunk")
                  map("n", "[H", function()
                    gs.nav_hunk("first")
                  end, "First Hunk")
                  map({ "n", "v" }, "<leader>gS", gs.stage_hunk, "(Un)Stage Hunk")
                  map("n", "<leader>gp", gs.preview_hunk, "Preview Hunk")
                  map("n", "<leader>gP", gs.preview_hunk_inline, "Preview Hunk Inline")
                  map("n", "<leader>gb", function()
                    gs.blame_line({ full = true })
                  end, "Blame Line")
                  map("n", "<leader>gB", function()
                    gs.blame()
                  end, "Blame Buffer")
                  map("n", "<leader>gx", gs.reset_hunk, "Reset Hunk")
                end
              }
            '';
        };
        neogit = {
          enable = true;

          settings.__raw = # lua
            ''
              {
                -- disable_commit_confirmation = true,
                -- disable_builtin_notifications = true,
                integrations = {
                  diffview = true,
                },
                signs = {
                  -- { CLOSED, OPENED }
                  section = { icons.ui.ChevronShortRight, icons.ui.ChevronShortDown },
                  item = { icons.ui.ChevronShortRight, icons.ui.ChevronShortDown },
                  hunk = { "", "" },
                },
                mappings = {
                  status = {
                    ["<c-t>"] = false,
                  },
                },
              }
            '';
        };
        diffview = {
          enable = true;

          settings = {
            default_args = {
              diffview_open = [ "--imply-local" ];
            };

            keymaps = {
              disable_defaults = false;
              view = [
                {
                  action = "<cmd>DiffviewClose<cr>";
                  description = "Close Diffview";
                  key = "q";
                  mode = "n";
                }
              ];
              file_panel = [
                {
                  action = "<cmd>DiffviewClose<cr>";
                  description = "Close Diffview";
                  key = "q";
                  mode = "n";
                }
              ];
              file_history_panel = [
                {
                  action = "<cmd>DiffviewClose<cr>";
                  description = "Close Diffview";
                  key = "q";
                  mode = "n";
                }
              ];
            };
          };
        };
        toggleterm = {
          enable = true;
          settings = {
            float_opts = {
              border = "${border}";
            };
          };
        };
      };
    };
}

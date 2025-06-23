{ inputs, pkgs, ... }:
{
  imports = [ inputs.nixvim.homeManagerModules.nixvim ];

  xdg.configFile."nvim/lua/blueberry_peach/light.lua" = {
    source = "${inputs.blueberry-peach}/ports/neovim/blueberry_peach_light.lua";
  };
  xdg.configFile."nvim/lua/blueberry_peach/dark.lua" = {
    source = "${inputs.blueberry-peach}/ports/neovim/blueberry_peach_dark.lua";
  };

  programs.nixvim =
    let
      border = "single";
    in
    {
      enable = true;
      defaultEditor = true;
      vimdiffAlias = true;

      nixpkgs.config.allowUnfree = true;

      globals = {
        mapleader = " ";
        maplocalleader = "\\";
      };

      opts = {
        # [[ Folding ]]
        foldcolumn = "0"; # '0' to hide or '1' to show
        foldlevel = 99; # Using ufo provider need a large value, feel free to decrease the value
        foldlevelstart = 99;
        foldenable = true;

        # [[ Misc ]]
        timeout = true;
        timeoutlen = 300; # num: Timeout, e.g. for which-key
        updatetime = 1000; # num: Timeout for "cursor hold" event
        clipboard = "unnamedplus"; # str: Clipboard integration with macOS
        splitkeep = "cursor"; # The default "screen" moves the cursor wrongly, which leads to problems, e.g. with Trouble

        # [[ Context ]]
        colorcolumn = "80"; # str: Show col for max line length
        number = true; # bool: Show line numbers
        scrolloff = 5; # int: Min num lines of context
        signcolumn = "yes"; # str: Show the sign column

        # [[ Filetypes ]]
        encoding = "utf8"; # str: String encoding to use
        fileencoding = "utf8"; # str: File encoding to use

        # [[ Theme ]]
        syntax = "ON"; # str: Allow syntax highlighting
        termguicolors = true; # bool: If term supports ui color then enable
        cursorline = true; # bool: Highlight current line
        # listchars = "space:·,tab:>~,trail:~,extends:>,precedes:<,eol:󰌑"
        listchars = "tab:~~,trail:~";
        list = true;

        # [[ Search ]]
        ignorecase = true; # bool: Ignore case in search patterns
        smartcase = true; # bool: Override ignorecase if search contains capitals
        incsearch = true; # bool: Use incremental search

        # [[ Whitespace ]]
        expandtab = true; # bool: Use spaces instead of tabs
        shiftwidth = 2; # num: Size of an indent
        softtabstop = 2; # num: Number of spaces tabs count for in insert mode
        tabstop = 2; # num: Number of spaces tabs count for
      };

      autoCmd = [
        {
          # Start terminal in insert mode:
          command = "startinsert";
          pattern = "*";
          event = "TermOpen";
        }
      ];

      diagnostic.settings = {
        virtual_lines = false;
        float = {
          border = border;
        };
        signs = {
          text.__raw = # lua
            ''
              {
                [vim.diagnostic.severity.ERROR] = "󰅚";
                [vim.diagnostic.severity.WARN] = "󰀪";
                [vim.diagnostic.severity.INFO] = "󰋽";
                [vim.diagnostic.severity.HINT] = "󰌶";
              }
            '';
          numhl.__raw = # lua
            ''
              {
                [vim.diagnostic.severity.ERROR] = "DiagnosticSignError";
                [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn";
                [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo";
                [vim.diagnostic.severity.HINT] = "DiagnosticSignHint";
              }
            '';
        };
      };

      files = {
        "ftplugin/gdscript.lua" = {
          opts = {
            expandtab = false;
          };
        };
        "ftplugin/typescript.lua" = {
          keymaps = [
            {
              action = "<cmd>VtsExec source_actions<cr>";
              key = "<leader>la";
              options = {
                noremap = true;
                silent = true;
                desc = "Code actions";
              };
            }
          ];
        };
      };

      colorschemes = {
        catppuccin = {
          enable = true;
          autoLoad = true;
          luaConfig = {
            pre = # lua
              ''
                ---Selects the catppuccin flavour or falls back to the given fallback.
                ---@param fallback "latte"|"frappe"|"macchiato"|"mocha"
                ---@return "latte"|"frappe"|"macchiato"|"mocha"
                function get_theme(fallback)
                  local xdg_config_home = vim.env.XDG_CONFIG_HOME
                    or os.getenv("XDG_CONFIG_HOME")
                    or (vim.env.HOME .. "/.config")
                  local theme_path = xdg_config_home .. "/current_theme_store"

                  local file, err = io.open(theme_path, "r")
                  if not file or err then
                    vim.notify("Could not read theme file " .. theme_path, vim.log.levels.WARN)
                    return fallback
                  end

                  local content = file:read("*a")
                  file:close()

                  -- Validate content
                  if content ~= "light" and content ~= "dark" then
                    vim.notify("Invalid theme value: " .. content, vim.log.levels.ERROR)
                    return fallback
                  end

                  return content == "light" and "frappe" or "mocha"
                end

                local catppuccin_config = {
                  flavour = get_theme("frappe"), -- latte, frappe, macchiato, mocha
                  integrations = {
                    beacon = true,
                    blink_cmp = true,
                    copilot_vim = true,
                    diffview = true,
                    fidget = true,
                    flash = true,
                    gitsigns = true,
                    illuminate = {
                      enabled = true,
                      lsp = true,
                    },
                    indent_blankline = {
                      enabled = true,
                    },
                    lsp_trouble = true,
                    markdown = true,
                    mason = true,
                    native_lsp = {
                      enabled = true,
                      virtual_text = {
                        errors = { "italic" },
                        hints = { "italic" },
                        warnings = { "italic" },
                        information = { "italic" },
                        ok = { "italic" },
                      },
                      underlines = {
                        errors = { "underline" },
                        hints = { "underline" },
                        warnings = { "underline" },
                        information = { "underline" },
                        ok = { "underline" },
                      },
                      inlay_hints = {
                        background = true,
                      },
                    },
                    neogit = true,
                    nvimtree = true,
                    render_markdown = true,
                    telescope = { enabled = true },
                    treesitter = true,
                    treesitter_context = true,
                    ufo = true,
                    which_key = true,
                  }
                }

                -- Extend with overrides for blueberry-peach theme
                local blueberry_peach_light = require("blueberry_peach.light")
                catppuccin_config = vim.tbl_deep_extend("force", catppuccin_config, blueberry_peach_light.get_overrides("frappe"))
                local blueberry_peach_dark = require("blueberry_peach.dark")
                catppuccin_config = vim.tbl_deep_extend("force", catppuccin_config, blueberry_peach_dark.get_overrides("mocha"))
              '';
          };
          settings = {
            __raw = "catppuccin_config";
          };
        };
      };

      extraConfigLuaPre = # lua
        ''
          vim.env.PATH = vim.env.HOME .. '/.local/share/mise/shims:' .. vim.env.PATH

          local icons = {
            misc = {
              dots = "󰇘",
            },
            ft = {
              octo = "",
            },
            dap = {
              Stopped = { "󰁕 ", "DiagnosticWarn", "DapStoppedLine" },
              Breakpoint = " ",
              BreakpointCondition = " ",
              BreakpointRejected = { " ", "DiagnosticError" },
              LogPoint = ".>",
            },
            diagnostics = {
              BoldError = "",
              Error = "󰅚",
              BoldWarn = "",
              Warn = "󰀪",
              BoldInfo = "",
              Info = "󰋽",
              BoldQuestion = "",
              Question = "",
              BoldHint = "󰌵",
              Hint = "󰌶",
              Debug = "",
              Trace = "✎",
              Ok = "",
            },
            git = {
              LineAdded = "",
              LineModified = "",
              LineRemoved = "",
              FileDeleted = "",
              FileIgnored = "◌",
              FileRenamed = "➜",
              FileStaged = "S",
              FileUnmerged = "",
              FileUnstaged = "",
              FileUntracked = "U",
              Diff = "",
              Repo = "",
              Octoface = "",
              Branch = "",
            },
            kinds = {
              Array = "",
              Boolean = "󰨙",
              Class = "",
              Codeium = "󰘦",
              Color = "",
              Control = "",
              Collapsed = "",
              Constant = "󰏿",
              Constructor = "",
              Copilot = "",
              Enum = "",
              EnumMember = "",
              Event = "",
              Field = "",
              File = "",
              Folder = "",
              Function = "󰊕",
              Interface = "",
              Key = "",
              Keyword = "",
              Method = "󰊕",
              Module = "",
              Namespace = "󰦮",
              Null = "",
              Number = "󰎠",
              Object = "",
              Operator = "",
              Package = "",
              Property = "",
              Reference = "",
              Snippet = "",
              String = "",
              Struct = "󰆼",
              TabNine = "󰏚",
              Text = "",
              TypeParameter = "",
              Unit = "",
              Value = "",
              Variable = "󰀫",
            },
            ui = {
              ArrowCircleDown = "",
              ArrowCircleLeft = "",
              ArrowCircleRight = "",
              ArrowCircleUp = "",
              BoldArrowDown = "",
              BoldArrowLeft = "",
              BoldArrowRight = "",
              BoldArrowUp = "",
              BoldClose = "",
              BoldDividerLeft = "",
              BoldDividerRight = "",
              BoldLineLeft = "▎",
              BookMark = "",
              BoxChecked = "",
              Bug = "",
              Stacks = "",
              Scopes = "",
              Watches = "󰂥",
              DebugConsole = "",
              Calendar = "",
              Check = "",
              ChevronShortDown = "",
              ChevronShortLeft = "",
              ChevronShortRight = "",
              ChevronShortUp = "",
              Circle = "",
              Close = "󰅖",
              CloudDownload = "",
              Code = "",
              Comment = "󰅺",
              Dashboard = "",
              DividerLeft = "",
              DividerRight = "",
              DoubleChevronRight = "»",
              Ellipsis = "…",
              EmptyFolder = "",
              EmptyFolderOpen = "",
              File = "󰈔",
              FileSymlink = "",
              Files = "󰈢",
              FindFile = "󰈞",
              FindText = "󰊄",
              Fire = "",
              Folder = "󰉋",
              FolderOpen = "",
              FolderSymlink = "",
              Forward = "",
              Gear = "",
              History = "󰄉",
              Lightbulb = "󰌵",
              LineLeft = "▏",
              LineMiddle = "│",
              List = "",
              Lock = "󰍁",
              NewFile = "",
              Note = "󰎞",
              Package = "",
              Pencil = "󰏫",
              Plus = "",
              Project = "",
              Search = "󰍉",
              SignIn = "",
              SignOut = "",
              Tab = "󰌒",
              Table = "",
              Target = "󰀘",
              Telescope = "",
              Text = "",
              Tree = "",
              Triangle = "契",
              TriangleShortArrowDown = "",
              TriangleShortArrowLeft = "",
              TriangleShortArrowRight = "",
              TriangleShortArrowUp = "",
            },
          }

          ---Shallow deduplication of the keys in the given table.
          ---@param input_table table The table to deduplicate.
          ---@return table result A new table with unique keys.
          local function dedup(input_table)
            local seen = {}
            local result = {}

            for _, value in ipairs(input_table) do
              if not seen[value] then
                seen[value] = true
                table.insert(result, value)
              end
            end

            return result
          end

          ---Check if the given buffer is a file buffer.
          ---Is not 100% reliable, but should work for most cases.
          ---@param bufnr number The number of the buffer to check.
          ---@return boolean is_file_buffer True if the buffer is a file buffer, false otherwise.
          local function is_file_buffer(bufnr)
            -- A file buffer typically has a name (file) and no type (like a special buffer)
            local buftype = vim.api.nvim_get_option_value("buftype", { buf = bufnr })
            local bufname = vim.api.nvim_buf_get_name(bufnr)

            return buftype == "" and bufname ~= ""
          end

          ---Opens a terminal in a vertical split and runs the given command.
          ---@param command? string The command to run in the terminal.
          ---@return nil
          local function split_term(command)
            command = command or ""
            vim.cmd("split | terminal " .. command)
          end

          ---Opens a terminal in a vertical split and runs the given command.
          ---If the previous buffer was a file buffer, it will be reloaded after the terminal closes.
          ---@param command? string The command to run in the terminal.
          ---@return nil
          local function split_term_and_edit(command)
            local prev_bufnr = vim.api.nvim_get_current_buf()

            -- Open a terminal and run the command
            split_term(command)
            local term_bufnr = vim.api.nvim_get_current_buf()

            -- React when the terminal command finishes
            vim.api.nvim_create_autocmd("TermClose", {
              buffer = term_bufnr,
              once = true,
              callback = function()
                -- Force close the current (terminal) window
                vim.api.nvim_win_close(0, true)
                -- Force close the terminal buffer
                vim.api.nvim_buf_delete(term_bufnr, { force = true })
                -- Go back to the previous buffer
                vim.api.nvim_set_current_buf(prev_bufnr)
                -- Call `edit` to re-load file content
                if is_file_buffer(prev_bufnr) then
                  vim.cmd("edit")
                end
              end,
            })
          end

          ---Check if a hover.nvim window is open in the given buffer.
          ---@param bufnr? number The buffer number to check. If nil, checks the current buffer.
          ---@return boolean is_hover True if the hover window is open, false otherwise.
          local function is_hover_open(bufnr)
            bufnr = bufnr or 0
            -- From https://github.com/lewis6991/hover.nvim/blob/1a8282fe3933c0c6f2769d7d6a9b7bab49984aee/lua/hover/actions.lua#L216
            local cur_hover = vim.b[bufnr].hover_preview
            if cur_hover and vim.api.nvim_win_is_valid(cur_hover) then
              return true
            end

            return false
          end

          local function gt_command_ls(command)
            vim.cmd("!gt " .. command .. " --no-interactive --quiet")

            -- Show where we are:
            vim.cmd("!gt ls")

            -- Edit a file buffer (0 is current buffer):
            if is_file_buffer(0) then
              vim.cmd("edit")
            end
          end
        '';

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
                          "<leader>fe",
                          "<cmd>NvimTreeToggle<cr>",
                          desc = "Explorer",
                        },
                        {
                          "<leader>fE",
                          "<cmd>NvimTreeFindFile<cr>",
                          desc = "Explorer (curr. dir.)",
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
                    {
                      "<leader>u",
                      "<cmd>Telescope undo<cr>",
                      desc = "Undo tree",
                    },
                  },
                },
              })
            '';
        };
        nvim-tree = {
          enable = true;

          actions = {
            openFile = {
              quitOnOpen = false;
            };
          };
          renderer = {
            icons = {
              gitPlacement = "after";
              glyphs = {
                folder = {
                  arrowOpen.__raw = "icons.ui.ChevronShortDown";
                  arrowClosed.__raw = "icons.ui.ChevronShortRight";
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

          onAttach.__raw = # lua
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
                      ["<c-j>"] = actions.move_selection_next,
                      ["<c-k>"] = actions.move_selection_previous,
                      ["<c-t>"] = open_with_trouble,
                    },
                    n = {
                      ["<c-e>"] = actions.close,
                      ["<c-t>"] = open_with_trouble,
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
        nvim-surround.enable = true;
        nvim-autopairs.enable = true;
        flash = {
          enable = true;

          settings = {
            incremental = true;
          };
        };
        copilot-vim.enable = true;
        lspconfig.enable = true;
        none-ls = {
          enable = true;

          sources = {
            diagnostics = {
              stylelint = {
                enable = true;
                settings = {
                  extra_filetypes = [ "vue" ];
                };
              };
            };
            formatting = {
              nixfmt = {
                enable = true;

                package = pkgs.nixfmt-rfc-style;
              };
              prettier = {
                enable = true;
                settings = {
                  # Always format open files, even if they are ignored.
                  # Setting `ignore-path` to empty string means "ignore nothing".
                  # By default, prettier will ignore files in .gitignore and .prettierignore.
                  extra_args = [
                    "--ignore-path"
                    ""
                  ];
                };
              };
              stylelint = {
                enable = true;
                settings = {
                  extra_filetypes = [ "vue" ];
                };
              };
              stylua = {
                enable = true;
              };
            };
          };
        };
        schemastore.enable = true;
        lsp-format = {
          enable = true;

          lspServersToEnable = [
            "bashls"
            "cssls"
            "eslint"
            "jsonls"
            "yamlls"
            "null-ls"
          ];

          settings = {
            html = {
              order = [ "null-ls" ];
              sync = true;
              # Format HTML only with prettier.
              exclude = [ "html" ];
            };
            javascript = {
              order = [
                "eslint"
                "null-ls"
              ];
              sync = true;
              # Format JS only with eslint and prettier.
              exclude = [ "vtsls" ];
            };
            typescript = {
              order = [
                "eslint"
                "null-ls"
              ];
              sync = true;
              # Format JS only with eslint and prettier.
              exclude = [ "vtsls" ];
            };
            json = {
              order = [
                "jsonls"
                "null-ls"
              ];
              sync = true;
            };
            yamlls = {
              order = [
                "yamlls"
                "null-ls"
              ];
              sync = true;
            };
            lua = {
              order = [ "null-ls" ];
              sync = true;
            };
            nix = {
              order = [ "null-ls" ];
              sync = true;
            };
            scss = {
              order = [
                "cssls"
                "null-ls"
              ];
              sync = true;
            };
            vue = {
              order = [ "null-ls" ];
              sync = true;
              # Format only with stylelint and prettier.
              exclude = [ "html" ];
            };
            sh = {
              order = [ "bashls" ];
              sync = true;
            };
            zsh = {
              order = [ "bashls" ];
              sync = true;
            };
          };
        };
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

          extraOptions = {
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

          defaultArgs = {
            diffviewOpen = [ "--imply-local" ];
          };

          keymaps = {
            disableDefaults = false;
            view = [
              {
                action = "<cmd>DiffviewClose<cr>";
                description = "Close Diffview";
                key = "q";
                mode = "n";
              }
            ];
            filePanel = [
              {
                action = "<cmd>DiffviewClose<cr>";
                description = "Close Diffview";
                key = "q";
                mode = "n";
              }
            ];
            fileHistoryPanel = [
              {
                action = "<cmd>DiffviewClose<cr>";
                description = "Close Diffview";
                key = "q";
                mode = "n";
              }
            ];
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

      lsp = {
        servers = {
          bashls.enable = true;
          cssls.enable = true;
          eslint.enable = true;
          lua_ls.enable = true;
          nixd.enable = true;
          vtsls.enable = true;

          html = {
            enable = true;
            settings = {
              filetypes = [
                "html"
                "templ"
                "vue"
              ];
            };
          };

          jsonls.enable = true;
          yamlls.enable = true;
        };
      };
    };
}

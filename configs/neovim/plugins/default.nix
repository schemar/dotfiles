{ pkgs, ... }:
{
  imports = [
    ./aerial.nix
    ./blink.nix
    ./nvimtree.nix
    ./other.nix
    ./telescope.nix
    ./whichkey.nix
  ];

  programs.nixvim =
    let
      border = "single";
    in
    {

      extraPlugins = [
        pkgs.vimPlugins.smart-open-nvim
        pkgs.vimPlugins.hover-nvim
        pkgs.vimPlugins.auto-hlsearch-nvim
      ];

      extraConfigLua = # lua
        ''
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
        yazi = {
          enable = true;
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

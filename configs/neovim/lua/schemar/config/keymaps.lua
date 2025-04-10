local M = {}

function M.init()
  local wk = require("which-key")
  local utils = require("schemar.utils")

  local gt_command_ls = function(command)
    vim.cmd("!gt " .. command .. " --no-interactive --quiet")

    -- Show where we are:
    vim.cmd("!gt ls")

    -- Edit a file buffer (0 is current buffer):
    if utils.is_file_buffer(0) then
      vim.cmd("edit")
    end
  end

  wk.add({
    {
      "]",
      group = "Next ...",
      {
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
          "[t",
          function()
            require("todo-comments").jump_prev()
          end,
          desc = "Previous todo comment",
        },
      },
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
        vim.lsp.buf.hover({ border = require("schemar.config.options").border })
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
                    utils.split_term("gt create")
                  end,
                  desc = "Create",
                },
                {
                  "<leader>gts",
                  function()
                    utils.split_term_and_edit("gt sync")
                  end,
                  desc = "Sync",
                },
                {
                  "<leader>gtp",
                  function()
                    utils.split_term("gt submit --no-edit")
                  end,
                  desc = "Submit",
                },
                {
                  "<leader>gtP",
                  function()
                    utils.split_term("gt submit --no-edit --publish")
                  end,
                  desc = "Submit (publish)",
                },
                {
                  "<leader>gtm",
                  function()
                    utils.split_term("gt modify")
                  end,
                  desc = "Modify",
                },
                {
                  "<leader>gtM",
                  function()
                    utils.split_term_and_edit("gt modify && gt sync && gt submit --no-edit")
                  end,
                  desc = "Modify, Sync, Submit",
                },
                {
                  "<leader>gto",
                  function()
                    utils.split_term_and_edit("gt checkout")
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
end

function M.git_attach(buffer)
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

function M.dashboard()
  return {
    {
      key = "f",
      icon = "",
      desc = "File",
      action = "<cmd>Telescope smart_open<cr>",
    },
    {
      key = "e",
      icon = "󰙅",
      desc = "Explorer",
      action = "<cmd>NvimTreeOpen<cr>",
    },
    {
      key = "/",
      icon = "󰍉",
      desc = "Grep",
      action = "<cmd>Telescope live_grep<cr>",
    },
    {
      key = "g",
      icon = "",
      desc = "Git",
      action = "<cmd>Neogit<cr>",
    },
    {
      key = "l",
      icon = "󰒲",
      desc = "Lazy",
      action = "<cmd>Lazy<cr>",
    },
    {
      key = "m",
      icon = "󰣪",
      desc = "Mason",
      action = "<cmd>Mason<cr>",
    },
    {
      key = "t",
      icon = "",
      desc = "TSUpdate",
      action = "<cmd>TSUpdate<cr>",
    },
    {
      key = "q",
      icon = "󰅙",
      desc = "Quit",
      action = "<cmd>qa<cr>",
    },
  }
end

return M

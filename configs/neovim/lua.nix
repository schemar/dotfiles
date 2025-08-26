{ ... }:
{
  programs.nixvim = {
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
  };
}

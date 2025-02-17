local M = {
  border = "rounded",
  -- icons used by other plugins
  icons = {
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
      Array = " ",
      Boolean = "󰨙 ",
      Class = " ",
      Codeium = "󰘦 ",
      Color = " ",
      Control = " ",
      Collapsed = " ",
      Constant = "󰏿 ",
      Constructor = " ",
      Copilot = " ",
      Enum = " ",
      EnumMember = " ",
      Event = " ",
      Field = " ",
      File = " ",
      Folder = " ",
      Function = "󰊕 ",
      Interface = " ",
      Key = " ",
      Keyword = " ",
      Method = "󰊕 ",
      Module = " ",
      Namespace = "󰦮 ",
      Null = " ",
      Number = "󰎠 ",
      Object = " ",
      Operator = " ",
      Package = " ",
      Property = " ",
      Reference = " ",
      Snippet = " ",
      String = " ",
      Struct = "󰆼 ",
      TabNine = "󰏚 ",
      Text = " ",
      TypeParameter = " ",
      Unit = " ",
      Value = " ",
      Variable = "󰀫 ",
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
      Check = "󰄬",
      ChevronRight = ">",
      ChevronShortDown = "",
      ChevronShortLeft = "",
      ChevronShortRight = "",
      ChevronShortUp = "",
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
  },
}

-- Options.
local opt = vim.opt

-- [[ Misc ]]
opt.timeout = true
opt.timeoutlen = 300 -- num: Timeout, e.g. for which-key
opt.updatetime = 1000 -- num: Timeout for "cursor hold" event
opt.clipboard = "unnamedplus" -- str: Clipboard integration with macOS
opt.splitkeep = "cursor" -- The default "screen" moves the cursor wrongly, which leads to problems, e.g. with Trouble

-- [[ Context ]]
opt.colorcolumn = "80" -- str: Show col for max line length
opt.number = true -- bool: Show line numbers
opt.scrolloff = 5 -- int: Min num lines of context
opt.signcolumn = "yes" -- str: Show the sign column

-- [[ Filetypes ]]
opt.encoding = "utf8" -- str: String encoding to use
opt.fileencoding = "utf8" -- str: File encoding to use

-- [[ Theme ]]
opt.syntax = "ON" -- str: Allow syntax highlighting
opt.termguicolors = true -- bool: If term supports ui color then enable
opt.cursorline = true -- bool: Highlight current line
-- opt.listchars = "space:·,tab:>~,trail:~,extends:>,precedes:<,eol:󰌑"
opt.listchars = "tab:~~,trail:~"
opt.list = true

-- [[ Search ]]
opt.ignorecase = true -- bool: Ignore case in search patterns
opt.smartcase = true -- bool: Override ignorecase if search contains capitals
opt.incsearch = true -- bool: Use incremental search

-- [[ Whitespace ]]
opt.expandtab = true -- bool: Use spaces instead of tabs
opt.shiftwidth = 2 -- num: Size of an indent
opt.softtabstop = 2 -- num: Number of spaces tabs count for in insert mode
opt.tabstop = 2 -- num: Number of spaces tabs count for

-- [[ Icons ]]
local icons = M.icons.diagnostics
vim.fn.sign_define("DiagnosticSignError", { text = icons.Error, texthl = "DiagnosticSignError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = icons.Warn, texthl = "DiagnosticSignWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = icons.Info, texthl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = icons.Hint, texthl = "DiagnosticSignHint" })

return M

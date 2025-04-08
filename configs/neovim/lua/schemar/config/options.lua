-- Options.
local error = "󰅚"
local warn = "󰀪"
local info = "󰋽"
local hint = "󰌶"

local arrow_right = ""

local border = "rounded"

local opt = vim.opt

-- [[ Leaders ]]
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- [[ Folding ]]
vim.o.foldcolumn = "0" -- '0' to hide or '1' to show
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

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

-- [[ LSP Settings ]]
vim.diagnostic.config({
  virtual_lines = false,
  float = {
    border = border,
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = error,
      [vim.diagnostic.severity.WARN] = warn,
      [vim.diagnostic.severity.INFO] = info,
      [vim.diagnostic.severity.HINT] = hint,
    },
    numhl = {
      [vim.diagnostic.severity.ERROR] = "DiagnosticSignError",
      [vim.diagnostic.severity.WARN] = "DiagnosticSignWarn",
      [vim.diagnostic.severity.INFO] = "DiagnosticSignInfo",
      [vim.diagnostic.severity.HINT] = "DiagnosticSignHint",
    },
  },
})

return {
  border = border,
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
      Error = error,
      BoldWarn = "",
      Warn = warn,
      BoldInfo = "",
      Info = info,
      BoldQuestion = "",
      Question = "",
      BoldHint = "󰌵",
      Hint = hint,
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
      Collapsed = arrow_right,
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
      ChevronShortRight = arrow_right,
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
  },
}

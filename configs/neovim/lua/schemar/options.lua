-- Options.
-- [[ opts.lua ]]
local opt = vim.opt

-- [[ Misc ]]
opt.timeoutlen = 300 -- num: Timeout, e.g. for which-key
opt.clipboard = 'unnamedplus' -- str: Clipboard integration with macOS

-- [[ Context ]]
opt.colorcolumn = '80' -- str: Show col for max line length
opt.number = true -- bool: Show line numbers
opt.scrolloff = 10 -- int: Min num lines of context
opt.signcolumn = 'yes' -- str: Show the sign column

-- [[ Filetypes ]]
opt.encoding = 'utf8' -- str: String encoding to use
opt.fileencoding = 'utf8' -- str: File encoding to use

-- [[ Theme ]]
opt.syntax = 'ON' -- str: Allow syntax highlighting
opt.termguicolors = true -- bool: If term supports ui color then enable
opt.cursorline = true -- bool: Highlight current line
opt.listchars = 'space:·,tab:>~,trail:~,extends:>,precedes:<'
opt.list = true

-- [[ Search ]]
opt.ignorecase = true -- bool: Ignore case in search patterns
opt.smartcase = true -- bool: Override ignorecase if search contains capitals
opt.incsearch = true -- bool: Use incremental search
opt.hlsearch = true -- bool: Highlight search matches

-- [[ Whitespace ]]
opt.expandtab = true -- bool: Use spaces instead of tabs
opt.shiftwidth = 2 -- num: Size of an indent
opt.softtabstop = 2 -- num: Number of spaces tabs count for in insert mode
opt.tabstop = 2 -- num: Number of spaces tabs count for

-- [[ Splits ]]
opt.splitright = true -- bool: Place new window to right of current one
opt.splitbelow = true

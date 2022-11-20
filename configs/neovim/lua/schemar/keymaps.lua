-- Note that additional keymaps are defined in the LSP and cmp files.
-- Switch meaning of vertical and horizontal split
vim.keymap.set('', '<C-w>s', '<C-w>v', {noremap = true})
vim.keymap.set('', '<C-w>v', '<C-w>s', {noremap = true})

--
-- Which-Key
local wk = require('which-key')
local telescope = require('telescope')
local builtin = require('telescope.builtin')
wk.register({
  ['['] = {
    name = 'Previous',
    d = {vim.diagnostic.goto_prev, 'Previous diagnostic'},
    b = {':bp<CR>', 'Previous buffer'},
  },
  [']'] = {
    name = 'Next',
    d = {vim.diagnostic.goto_next, 'Next diagnostic'},
    b = {':bn<CR>', 'Next buffer'},
  },
})
wk.register({
  ['<leader>'] = {builtin.find_files, 'Find file'},
  ['/'] = {builtin.live_grep, 'Grep directory'},
  b = {
    name = 'Buffer',
    b = {builtin.buffers, 'Find buffer'},
    d = {':bd<CR>', 'Delete buffer'},
    n = {':bn<CR>', 'Next buffer'},
    p = {':bp<CR>', 'Previous buffer'},
  },
  c = {
    name = 'Code',
    d = {':TroubleToggle document_diagnostics<CR>', 'Show diagnostics'},
    D = {
      ':TroubleToggle workspace_diagnostics<CR>', 'Show workspace diagnostics',
    },
    e = {vim.diagnostic.open_float, 'Floating diagnostic'},
  },
  f = {
    name = 'File',
    f = {
      function()
        telescope.extensions.file_browser.file_browser({
          hidden = true,
          grouped = true,
          initial_mode = 'normal',
        })
      end, 'Browse files',
    },
    s = {':w<CR>', 'Save'},
    S = {':wa<CR>', 'Save all'},
    t = {':NvimTreeToggle<CR>', 'Toggle file tree'},
  },
  g = {
    name = 'Git',
    b = {':Gitsigns toggle_current_line_blame<CR>', 'Toggle blame'},
    g = {':Neogit<CR>', 'Neogit'},
  },
  h = {builtin.help_tags, 'Help'},
  w = {
    name = 'Window',
    c = {':close<CR>', 'Close window'},
    s = {':vsplit<CR>', 'Split window'},
    v = {':split<CR>', 'Vertically split window'},
  },
  q = {name = 'Quit', q = {':quitall<CR>', 'Quit all'}},
}, {prefix = '<leader>'})

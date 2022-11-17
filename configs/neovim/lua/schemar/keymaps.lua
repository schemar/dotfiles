-- Switch meaning of vertical and horizontal split
vim.keymap.set('', '<C-w>s', '<C-w>v', { noremap = true })
vim.keymap.set('', '<C-w>v', '<C-w>s', { noremap = true })


--
-- Which-Key
local wk = require('which-key')
local telescope = require('telescope')
local builtin = require('telescope.builtin')
wk.register({
  ['['] = {
    name = 'Previous',
    d = {vim.diagnostic.goto_prev, 'Previous diagnostic'}
  },
  [']'] = {
    name = 'Next',
    d = {vim.diagnostic.goto_next, 'Next diagnostic'}
  },
})
wk.register({
  ['<leader>'] = {builtin.find_files, 'Find file'},
  ['/'] = {builtin.live_grep, 'Grep directory'},
  b = {
    name = 'Buffer',
    b = {builtin.buffers, 'Find buffer'},
  },
  c = {
    name = 'Code',
    d = {vim.diagnostic.setloclist, 'Show diagnostics'}
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
      end,
      'Browse files',
    },
    t = {':NvimTreeToggle<CR>', 'Toggle file tree'},
  },
  g = {
    name = 'Git',
    b = {':Gitsigns toggle_current_line_blame<CR>', 'Toggle blame'},
    g = {':Neogit<CR>', 'Neogit'},
  },
  h = {builtin.help_tags, 'Help'},
}, {prefix = '<leader>'})

local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim' -- Package manager
  use 'nvim-lua/plenary.nvim' -- Plugin with util functions required by other plugins
  use 'arcticicestudio/nord-vim' -- Color theme
  use 'christoomey/vim-tmux-navigator' -- Switch windows/panes vim/tmux
  use 'neovim/nvim-lspconfig' -- Configurations for Nvim LSP
  use 'L3MON4D3/LuaSnip' -- Snippets plugin
  use 'hrsh7th/cmp-nvim-lsp' -- LSP source for nvim-cmp
  use 'hrsh7th/cmp-buffer' -- LSP source for nvim-cmp
  use 'hrsh7th/cmp-path' -- LSP source for nvim-cmp
  use 'hrsh7th/cmp-cmdline' -- LSP source for nvim-cmp
  use 'saadparwaiz1/cmp_luasnip' -- Snippets source for nvim-cmp
  use 'onsails/lspkind.nvim' -- Icons in completion dialogue
  use 'hrsh7th/nvim-cmp' -- Autocompletion plugin
  use 'jose-elias-alvarez/null-ls.nvim' -- NeoVim as LSP server
  use 'MunifTanjim/prettier.nvim' -- Prettier for TS/JS formatting
  use 'nvim-tree/nvim-web-devicons' -- Fancy icons in pop-ups
  use 'nvim-lualine/lualine.nvim' -- Modeline
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use 'lukas-reineke/indent-blankline.nvim' -- Indent guides
  use { 'nvim-telescope/telescope.nvim', branch = '0.1.x' } -- Fancy picker (think fzf)
  use 'nvim-telescope/telescope-file-browser.nvim' -- Think Emacs directory browser
  use {
    'windwp/nvim-autopairs', -- Auto-pair tags, etc.
    config = function() require("nvim-autopairs").setup {} end
  }
  use 'windwp/nvim-ts-autotag' -- Auto-tags for HTML, Vue, etc.
  use 'norcalli/nvim-colorizer.lua' -- Show color-codes colored
  use 'lewis6991/gitsigns.nvim' -- Git gutter
  use 'TimUntersberger/neogit' -- Think magit
end)

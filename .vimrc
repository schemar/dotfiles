"
" Geeral setup
"

" Faster ESC reaction
set timeout timeoutlen=500 ttimeoutlen=15

" Indentation
filetype indent on
set autoindent
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

" Highlight search
set hlsearch
set incsearch
noh

" Show all whitespace characters
set listchars=tab:>·,trail:~,extends:>,precedes:<
set list

" Use mac clipboard
set clipboard=unnamed

" Allow non-written buffers to be hidden in the background
set hidden
" Show line numbers
set number
" more powerful backspacing
set backspace=indent,eol,start

" Keep context above and below cursor
set scrolloff=10

" Explorer
" Tree view
let g:netrw_liststyle = 3
" Remove banner
let g:netrw_banner = 0

" FZF
" If installed using Homebrew
set rtp+=/usr/local/opt/fzf
set rtp+=/home/linuxbrew/.linuxbrew/opt/fzf

" Ale
" Enable completion where available.
" This setting must be set before ALE is loaded.
" You should not turn this setting on if you wish to use ALE as a completion
" source for other completion plugins, like Deoplete.
let g:ale_completion_enabled = 1
" Fix formatting on save
let g:ale_fix_on_save = 1
" Auto-import TS
let g:ale_completion_tsserver_autoimport = 1
" Keep the sign gutter open
set signcolumn=yes
let g:ale_sign_column_always = 1
" Remove grey background from ALE gutter
highlight clear SignColumn
highlight clear ALEErrorSign
highlight clear ALEWarningSign
" Ale linters and fixers
let g:ale_linters = {
\   'typescript': ['tsserver', 'eslint'],
\}
let g:ale_fixers = {
\    'typescript': ['prettier', 'eslint'],
\}

"
" Plugins
"

" Specify a directory for plugins
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes
Plug 'https://github.com/junegunn/fzf.vim'
Plug 'https://github.com/dense-analysis/ale'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/lifepillar/vim-solarized8'
Plug 'https://github.com/leafgarland/typescript-vim'
Plug 'https://github.com/ianks/vim-tsx'
Plug 'https://github.com/airblade/vim-gitgutter'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/christoomey/vim-tmux-navigator'
Plug 'https://github.com/kien/rainbow_parentheses.vim'

" Initialize plugin system
call plug#end()

"
" Custom keybindings
"
let mapleader = ","
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>g :Rg<SPACE> " grep
nnoremap <leader>d :ALEGoToDefinition<CR>
nnoremap <leader>r :ALEFindReferences<CR>
nnoremap <leader>s :ALESymbolSearch<SPACE>
nnoremap <leader>2 :ALERename<CR>
nnoremap <leader>h :noh<CR> " disable search result highlights
nnoremap <leader>p :RainbowParanthesesToggle<CR>

"
" Always have rainbow parantheses
"
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"
" Powerline
"
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

set laststatus=2

" Colors
set termguicolors
syntax on
set background=dark
colorscheme solarized8_flat

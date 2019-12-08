"
" Geeral setup
"

" Indentation
filetype indent on
set autoindent
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

" Allow non-written buffers to be hidden in the background
set hidden
" Show line numbers
set number
" Syntax highlighting
syntax on

" Explorer
" Tree view
let g:netrw_liststyle = 3
" Remove banner
let g:netrw_banner = 0
" Resize explorer
let g:netrw_winsize = 25

" FZF
" If installed using Homebrew
set rtp+=/usr/local/opt/fzf

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
" Immediately open a list if there is an error
let g:ale_open_list = 1
" Remove grey background from ALE gutter
highlight clear SignColumn
highlight clear ALEErrorSign
highlight clear ALEWarningSign
" Ale linters and fixers
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'typescript': ['tsserver', 'eslint'],
\   'vue': ['eslint']
\}
let g:ale_fixers = {
\    'javascript': ['eslint'],
\    'typescript': ['prettier', 'eslint'],
\    'vue': ['eslint'],
\    'scss': ['prettier'],
\    'html': ['prettier']
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

" Initialize plugin system
call plug#end()

"
" Keybindings
"
nnoremap <C-P> :Files<CR>
nnoremap <C-N> :Rg<CR>
nnoremap <C-]> :ALEGoToDefinition<CR>
nnoremap ]<C-]> :ALEFindReferences<CR>
nnoremap <C-K> :ALESymbolSearch<SPACE>

"
" Powerline
"
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

set laststatus=2

" Colors
syntax enable
set background=dark
colorscheme solarized8_flat
set termguicolors

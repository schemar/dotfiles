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
set listchars=space:·,tab:>~,trail:~,extends:>,precedes:<
set list

" Use mac clipboard
set clipboard=unnamed

" Allow non-written buffers to be hidden in the background
set hidden

" Show hybrid line numbers if focused
set number relativenumber
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

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
" If installed using Linuxbrew
set rtp+=/home/linuxbrew/.linuxbrew/opt/fzf

" Ale
" Enable completion where available.
" This setting must be set before ALE is loaded.
" You should not turn this setting on if you wish to use ALE as a completion
" source for other completion plugins, like Deoplete.
let g:ale_completion_enabled = 1
" Set ale as omnicomplete to trigger manually if desired.
set omnifunc=ale#completion#OmniFunc
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
Plug 'https://github.com/leafgarland/typescript-vim'
Plug 'https://github.com/ianks/vim-tsx'
Plug 'https://github.com/airblade/vim-gitgutter'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/christoomey/vim-tmux-navigator'
Plug 'https://github.com/luochen1990/rainbow'
Plug 'https://github.com/vim-airline/vim-airline'
Plug 'https://github.com/vim-airline/vim-airline-themes'
Plug 'https://github.com/scrooloose/nerdtree'
Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'

" Color themes:
Plug 'https://github.com/lifepillar/vim-solarized8'

" Initialize plugin system
call plug#end()

"
" Custom keybindings
"
let mapleader = ","
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>g :Rg<SPACE>
nnoremap <leader>d :ALEGoToDefinition<CR>
nnoremap <leader>r :ALEFindReferences<CR>
nnoremap <leader>s :ALESymbolSearch<SPACE>
nnoremap <leader>2 :ALERename<CR>
nnoremap <leader>h :noh<CR> " disable search result highlights
nnoremap <leader>p :RainbowParanthesesToggle<CR>
nnoremap <leader>e :NERDTreeToggle<CR>

"
" Rainbow parantheses
"
let g:rainbow_active = 1
let g:rainbow_conf = {'guifgs': ['darkorange3', 'seagreen3', 'firebrick', 'lightblue', 'lightmagenta']}

"
" Airline
"
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'solarized'
let g:airline#extensions#ale#enabled = 1
set laststatus=2

" Colors
set termguicolors
syntax on
set background=dark
colorscheme solarized8_flat

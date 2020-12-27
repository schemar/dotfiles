set runtimepath^=$HOME/.vim runtimepath+=$HOME/.vim/after
let &packpath = &runtimepath

" Speedup python3
let g:python3_host_prog ='/usr/local/bin/python3'
" Disable python2
let g:loaded_python_provider = 0

source $HOME/.vimrc

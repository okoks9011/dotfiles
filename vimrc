" ==============================================================================
" Clear all settings
"set all&

" ==============================================================================
" to VIM default setting
set nocompatible
filetype plugin on

" ==============================================================================
" Vim-Plug
call plug#begin('~/.vim/plugged/')
Plug 'vim-airline/vim-airline'

call plug#end()

" ==============================================================================
" Custom confs
let mapleader = " "
nmap <leader>vr :e $MYVIMRC<cr>
nmap <leader>so :source $MYVIMRC<cr>

set encoding=utf-8
syntax on
set nu
set laststatus=2

" ==============================================================================
" Fix appearance
let g:airline_left_sep=''
let g:airline_right_sep=''
set fillchars=vert:\ ,fold:-

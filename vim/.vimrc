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
"Plug 'vim-airline/vim-airline'
"Plug 'godlygeek/tabular'
Plug 'junegunn/seoul256.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'

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

let g:seoul256_background = 236
colo seoul256

" ==============================================================================
" Fix appearance
let g:airline_left_sep=''
let g:airline_right_sep=''
set fillchars=vert:\ ,fold:-

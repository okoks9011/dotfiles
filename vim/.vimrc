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
nmap <leader>vr :tabe $MYVIMRC<cr>
nmap <leader>so :source $MYVIMRC<cr>

set encoding=utf-8
syntax on
set nu
set laststatus=2

let g:seoul256_background = 236
colo seoul256

" ==============================================================================
" Practical vim tips
cnoremap <C-p> <up>
cnoremap <C-n> <down>

cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [b :bprev<CR>
nnoremap <silent> ]B :blast<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]a :next<CR>
nnoremap <silent> [a :prev<CR>
nnoremap <silent> ]A :last<CR>
nnoremap <silent> [A :first<CR>

" ==============================================================================
" Fix appearance
let g:airline_left_sep=''
let g:airline_right_sep=''
set fillchars=vert:\ ,fold:-

if has('gui_running')
  set guifont=D2Coding\ 13
  set guioptions-=m
  set guioptions-=T
  set guioptions-=L
  set guioptions-=r
endif

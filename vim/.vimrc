" ==============================================================================
" Clear all settings
"set all&

" ==============================================================================
" to VIM default setting
set nocompatible
filetype plugin on
" Enhance '%' by Practical vim tips
runtime  macros/matchit.vim

" ==============================================================================
" Vim-Plug
call plug#begin('~/.vim/plugged/')
Plug 'junegunn/seoul256.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'
Plug 'mileszs/ack.vim'
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'eagletmt/ghcmod-vim'
Plug 'junegunn/vim-easy-align'

call plug#end()

" ==============================================================================
" Custom confs
let mapleader = " "

set encoding=utf-8
syntax on
set nu
set laststatus=2
set listchars=tab:>·,trail:~
set list

let g:seoul256_background = 236
colo seoul256

let g:ackprg = 'ag --vimgrep'

xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

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
if has('gui_running')
  if has('gui_macvim')
    set guifont=D2Coding:h16
  else
    set guifont=D2Coding\ 13
  endif
  set guioptions-=m
  set guioptions-=T
  set guioptions-=L
  set guioptions-=r
endif

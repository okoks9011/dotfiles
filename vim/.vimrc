set nocompatible
filetype plugin on

" ==============================================================================
" vim-plug Settings
" ==============================================================================
call plug#begin('~/.vim/plugged/')
Plug 'junegunn/seoul256.vim'

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-abolish'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-lastpat'
Plug 'nelstrom/vim-visual-star-search'

Plug 'mileszs/ack.vim'

Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'eagletmt/ghcmod-vim'
Plug 'junegunn/vim-easy-align'

Plug 'nvie/vim-flake8'

Plug 'guns/vim-clojure-static'
Plug 'tpope/vim-fireplace'
call plug#end()

" ==============================================================================
" Plugin Auxiliary Settings
" ==============================================================================
" vim-easy-align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" ack.vim
let g:ackprg = 'ag --vimgrep'
let g:ackhighlight = 1

" vim-flake8
let g:flake8_show_in_file=1

" seoul256.vim
let g:seoul256_background=235
colo seoul256

" ==============================================================================
" Custom Settings
" ==============================================================================
let mapleader = " "

set encoding=utf-8
syntax on
set number
set listchars=tab:>·,trail:~
set list

let $PATH = $PATH . ':' . expand('~/.local/bin')

" ==============================================================================
" Practical vim tips
cnoremap <C-p> <up>
cnoremap <C-n> <down>

cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

nnoremap <silent> ]a :next<CR>
nnoremap <silent> [a :prev<CR>
nnoremap <silent> ]A :last<CR>
nnoremap <silent> [A :first<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> [b :bprev<CR>
nnoremap <silent> ]B :blast<CR>
nnoremap <silent> [B :bfirst<CR>
nnoremap <silent> ]c :cnext<CR>
nnoremap <silent> [c :cprev<CR>
nnoremap <silent> ]C :clast<CR>
nnoremap <silent> [C :cfirst<CR>

runtime macros/matchit.vim

set hlsearch
set incsearch
noremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

nnoremap & :&&<CR>
xnoremap & :&&<CR>

" ==============================================================================
" GUI Settings
" ==============================================================================
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
" ==============================================================================

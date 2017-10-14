set tabstop=2
set shiftwidth=2
set expandtab
set suffixesadd+=.hs

nmap <silent> <leader>ht :GhcModType<cr>
nmap <silent> <leader>hT :GhcModTypeInsert<cr>
nmap <silent> <leader>hl :GhcModLint<cr>
nmap <silent> <leader>hc :GhcModCheck<cr>
nmap <silent> <leader><cr> :GhcModTypeClear<cr>

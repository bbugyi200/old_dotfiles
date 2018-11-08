nnoremap <nowait> [ ?\v^\s*\zs[A-Z ]+$<CR>
nnoremap <nowait> ] /\v^\s*\zs[A-Z ]+$<CR>
nnoremap <nowait> { ?\v^[A-Z][A-Z ]*$<CR>
nnoremap <nowait> } /\v^[A-Z][A-Z ]*$<CR>
nnoremap <nowait> ( ?\v^[A-Z.]+\(\d\)<CR>zt
nnoremap <nowait> ) /\v^[A-Z.]+\(\d\)<CR>zt
nnoremap g/ /\v^\s*\zs

set scrolloff=0

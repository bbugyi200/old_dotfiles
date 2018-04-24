NeoIncludeMakeCache

" Man Pages
nnoremap <Leader>m :Man 3 <C-r><C-w><CR>/^[A-Z]\+<CR>gg

" Open Makefile
nnoremap <Leader>M :e Makefile<CR>

" Open header file
nnoremap <Leader>h :e <C-r>=expand('%:r')<CR>.h<CR>

nnoremap <Leader>N :NeoIncludeMakeCache<CR>

" Automatic wrap
set tw=93

" Enables * and - to be treated like bullets by autowrap
setlocal comments=fb:-,fb:*

" 'n' enables numbered lists to be formatted like bullets
setlocal formatoptions+=n

nmap <silent> Q gqap

" setlocal shiftwidth=3
" setlocal tabstop=3

" mapping to get word count from 's' mark to 'e' mark
nnoremap <Leader>w :'s,'e write !wc<CR>

" mapping to wrap paragraph
nnoremap <Leader>g gqap

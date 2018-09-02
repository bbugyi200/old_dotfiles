" Sets width to wrap at
set tw=93

" Disables Automatic Wrap
setlocal formatoptions-=t

" Does not allow automatic wrap in the middle of word
setlocal linebreak

" Enables * and - to be treated like bullets by autowrap
setlocal comments=fb:-,fb:*

" 'n' enables numbered lists to be formatted like bullets
setlocal formatoptions+=n

" setlocal shiftwidth=3
" setlocal tabstop=3

" mapping to get word count from 's' mark to 'e' mark
nnoremap <Leader>w :'s,'e write !wc<CR>

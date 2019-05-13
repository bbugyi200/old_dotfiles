if &buftype =~? 'nofile'
    nmap <buffer> u <Plug>(InfoUp)

    nmap <nowait> <buffer> p <C-]>
    nmap <nowait> <buffer> <CR> <C-]>

    nnoremap <buffer> <nowait> i ?\v(\*[Nn]ote\_.{-1,}(\:\:\|[\.,]))\|(^\*\s+.{-}\:(:\|\s+.{-}(,\|\. \|:\|	\|$)))<CR>
    nnoremap <buffer> <nowait> o /\v(\*[Nn]ote\_.{-1,}(\:\:\|[\.,]))\|(^\*\s+.{-}\:(:\|\s+.{-}(,\|\. \|:\|	\|$)))<CR>

    nnoremap <nowait> <buffer> 9 <C-u>
    nnoremap <nowait> <buffer> 0 <C-d>

    nmap <buffer> <nowait> ( <Plug>(InfoPrev)
    nmap <buffer> <nowait> ) <Plug>(InfoNext)

    nnoremap <buffer> x :qa!<CR>
    nnoremap <buffer> q :qa!<CR>

    nnoremap <nowait> <buffer> <BS> <C-o>
endif

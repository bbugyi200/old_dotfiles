function! EndOfBox()
    let c = col('.')
    let _ = cursor(line('.'), 100)
    while c < 97
        let c = col('.')
        execute "normal a "
    endwhile
    execute "normal a#"
endfunction

nnoremap <Leader># :call EndOfBox()<CR>

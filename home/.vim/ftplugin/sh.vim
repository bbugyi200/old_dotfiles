function! EndOfBox()
    execute "normal 0"
    let ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
    if ch != "#"
        execute "normal xi# "
    endif
    let _ = cursor(line('.'), 99)
    let c = col('.')
    let ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
    if ch == "#" || c == 99
        execute "normal D"
        let c = col('.')
    endif
    while c < 97
        let c = col('.')
        execute "normal a "
    endwhile
    execute "normal a#"
endfunction

function! MakeBox()
    execute "normal 0"
    let ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
    execute "normal l"
    let och = matchstr(getline('.'), '\%' . col('.') . 'c.')
    while ch != "#" || och != "#"
        execute "normal k0"
        let ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
        execute "normal l"
        let och = matchstr(getline('.'), '\%' . col('.') . 'c.')
    endwhile

    execute "normal j0"
    let ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
    execute "normal l"
    let och = matchstr(getline('.'), '\%' . col('.') . 'c.')
    while ch != "#" || och != "#"
        call EndOfBox()
        execute "normal j0"
        let ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
        execute "normal l"
        let och = matchstr(getline('.'), '\%' . col('.') . 'c.')
    endw
endfunction

nnoremap <Leader># :call EndOfBox()<CR>
nnoremap <Leader><Leader># :call MakeBox()<CR>

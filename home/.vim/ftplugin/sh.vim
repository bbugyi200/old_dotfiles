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

function! BoxBar()
    let _ = cursor(line('.'), 99)
    let c = col('.')
    while c != 99
        execute "normal a#"   
        let c = col('.')
    endw
endf

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

    call BoxBar()

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

    call BoxBar()
endfunction

nnoremap <Leader># :call EndOfBox()<CR>
nnoremap <Leader><Leader># :call MakeBox()<CR>
nnoremap <Leader><Leader><Leader># :call BoxBar()<CR>

command! -nargs=0 T exec ":e ~/Dropbox/scripts/templates/script-init/template.1.sh"

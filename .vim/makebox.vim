" Close comment box.
"
" Beginning comment bar and ending comment bar must both be defined already
" and the cursor needs to be between the bars when 'MakeBox' is called.
function! MakeBox()
    " Mark starting position
    normal! mm

    if index(['conf', 'bash', 'sh', 'python', 'ruby', 'snippets', 'gdb'], &ft) >= 0
        let g:comment_char = '#'
    elseif index(['c', 'cpp', 'java', 'javascript', 'php', 'txt'], &ft) >= 0
        let g:comment_char = '/'
    elseif index(['haskell', 'sql'], &ft) >= 0
        let g:comment_char = '-'
    elseif index(['tex'], &ft) >= 0
        let g:comment_char = '%'
    elseif index(['scheme'], &ft) >= 0
        let g:comment_char = ';'
    elseif index(['vim'], &ft) >= 0
        let g:comment_char = '"'
    endif

    let triple_comment = g:comment_char . g:comment_char . g:comment_char

    let curr_line = getline('.')
    while curr_line[0:2] != triple_comment
        normal! k
        let curr_line = getline('.')
    endw

    normal! $
    let max_line = col('.')

    call MakeBoxBar(max_line)

    normal! j
    let curr_line = getline('.')
    let first_line = 1
    while curr_line[0:2] != triple_comment
        if first_line == 1
            let first_line = 0
        else
            normal! j
        endif

        let curr_line = getline('.')
        call MakeBoxLine(max_line)
    endw

    call MakeBoxBar(max_line)

    " Return cursor to starting position
    normal! `mh
endfunction

" Construct one of the bars that goes at the beginning and end of a
" comment-box.
function! MakeBoxBar(max_line)
    let _ = cursor(line('.'), a:max_line)
    let column_number = col('.')
    while column_number != a:max_line
        execute "normal! a" . g:comment_char
        let column_number = col('.')
    endw
endf

" Format a single line inside of a comment box
function! MakeBoxLine(max_line)
    normal! 0
    let current_ch = matchstr(getline('.'), '\%' . col('.') . 'c.')

    if current_ch != g:comment_char
        execute "normal! i" . g:comment_char . " "
    endif

    let do_double = index(['/', '-', ';'], g:comment_char) >= 0

    if do_double
        normal! 0l
        let ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
        let column_number = col('.')
        if ch != g:comment_char
            execute "normal! i" . g:comment_char
        endif
    endif

    let _ = cursor(line('.'), a:max_line)
    let column_number = col('.')
    let current_ch = matchstr(getline('.'), '\%' . col('.') . 'c.')
    if column_number != 1 && (do_double != 1 || column_number != 2)
        if current_ch == g:comment_char || column_number == a:max_line
            normal! D
            let column_number = col('.')
        endif
    endif
    while column_number < (a:max_line - 1)
        execute "normal! a "
        let column_number = col('.')
    endwhile
    execute "normal! a" . g:comment_char

    if do_double
        execute "normal! hr" . g:comment_char
    endif
endfunction

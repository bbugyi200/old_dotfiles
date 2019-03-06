let b:run_cmd = "!pytest -v %"

" ------------------------------------- MULTILINE STRINGS -----------------------------------------
function! BreakMultlineString(quote)
    let w = &tw+1
    execute "normal " . w . "|"
    let c = virtcol('.')
    execute "normal F" . a:quote
    let a = virtcol('.')
    while c == w
        execute "normal " . w . "|"
        let c = virtcol('.')
        if c == w
            execute "normal " . w . "|Bi" . a:quote . "\<cr>" . a:quote . "\<esc>"
            normal ^
            let b = virtcol('.')
            if a > b
                let d = a-b
                execute "normal " . d . " "
            endif
        endif
    endwhile
endfunction

function! JoinMultilineString()
    let c = '"'
    while c == "'" || c == '"'
        normal ^
        let c = matchstr(getline('.'), '\%' . col('.') . 'c.')
        if c == "'" || c == '"'
            normal k
        endif
    endwhile

    let c = '"'
    while c == "'" || c == '"'
        normal gj^
        let c = matchstr(getline('.'), '\%' . col('.') . 'c.')
        normal k
        if c == "'" || c == '"'
            normal ,j
        endif
    endwhile
endfunction

" join lines
nmap <Leader>j Jds'ds"x
nnoremap <Leader>J :call JoinMultilineString()<CR>

" break lines
nnoremap <Leader>" 100\|Bi"<CR>"<Esc>
nnoremap <Leader><Leader>" :call JoinMultilineString()<CR>:call BreakMultlineString('"')<CR>
nnoremap <Leader>' 100\|Bi'<CR>'<Esc>
nnoremap <Leader><Leader>' :call JoinMultilineString()<CR>:call BreakMultlineString("'")<CR>

nmap <Leader>t :call SwitchToTest('py')<CR>
nmap <Leader><Leader>t :!pyinit -n -t <C-R>=expand('%:r')<CR><CR>
nmap <Leader>T :call VSwitchToTest('py')<CR>

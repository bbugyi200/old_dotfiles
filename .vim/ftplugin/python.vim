" -------------------------------------------------------------------------------------------------
if has('win32')
    command! -nargs=* Run !py "%"
    nmap <F8> :w<CR>:!py -i "%"<CR> 
else
    command! -nargs=* Run call VimuxRunCommandInDir("clear &&"
                    \ ."tmux select-pane -t bottom &&" 
                    \ ." python <args>", 1)
    nmap <F8> :w<CR>:Run -i<CR> 
endif

nnoremap <Leader>T :e ~/Dropbox/scripts/templates/template.py<CR>
nnoremap <Leader><Leader>T :e ~/Dropbox/scripts/templates/test_template.py<CR>

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

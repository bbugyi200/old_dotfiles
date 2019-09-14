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
nnoremap <Leader>" 80\|Bi"<CR>"<Esc>
nnoremap <Leader><Leader>" :call JoinMultilineString()<CR>:call BreakMultlineString('"')<CR>
nnoremap <Leader>' 80\|Bi'<CR>'<Esc>
nnoremap <Leader><Leader>' :call JoinMultilineString()<CR>:call BreakMultlineString("'")<CR>

nnoremap [op :let g:syntastic_python_checkers=['flake8', 'mypy']<CR>:e<CR>
nnoremap ]op :let g:syntastic_python_checkers=['flake8', 'mypy', 'pylint']<CR>:e<CR>
nnoremap [oP :let g:syntastic_python_pylint_args=''<CR>:let g:syntastic_python_checkers=['pylint']<CR>:e<CR>
nnoremap ]oP :let g:syntastic_python_pylint_args='--rcfile=~/.config/pylintrc'<CR>:let g:syntastic_python_checkers=['flake8', 'mypy', 'pylint']<CR>:e<CR>
nnoremap <Leader>0f :e ~/.config/flake8<CR>
nnoremap <Leader>0p :e ~/.config/pylintrc<CR>
nnoremap <Leader>b :call system('echo "b ' . expand('%:p') . ':' . line('.') . '" >> ~/.config/pudb/saved-breakpoints-2.7')<CR>
imap <Leader>( (<CR><Esc>O
nmap <Leader>K :!qutebrowser https://docs.python.org/3/library/<C-R><C-W> &> /dev/null &<CR><CR>
nmap <Leader>t :call SwitchToTest('py')<CR>
nmap <Leader><Leader>t :!pyinit -n -t <C-R>=expand('%:r')<CR><CR>
nmap <Leader>T :call VSwitchToTest('py')<CR>

" ----- pytest -----
nnoremap <Leader><C-]> /\v^\s*def <C-r><C-w><CR>

" ----- macros -----
let @c='A  # pragma: no cover'
let @o='F(af)ikA,'
let @t='yiwmm?from typingofrom typing pmtgg/from typingV`t:sort`m'

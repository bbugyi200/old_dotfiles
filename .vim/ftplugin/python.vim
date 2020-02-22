setlocal indentkeys-=<:>
setlocal indentkeys+==else

let b:run_cmd = ":ALEFix"

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
nnoremap <Leader>" 79\|Bi"<CR>"<Esc>
nnoremap <Leader><Leader>" :call JoinMultilineString()<CR>:call BreakMultlineString('"')<CR>
nnoremap <Leader>' 79\|Bi'<CR>'<Esc>
nnoremap <Leader><Leader>' :call JoinMultilineString()<CR>:call BreakMultlineString("'")<CR>

nnoremap [om :let g:ale_fix_on_save = 0<CR>
nnoremap ]om :let g:ale_fix_on_save = 1<CR>
nmap <Leader>S [om,s]om
nnoremap <Leader>0f :e ~/.config/flake8<CR>
nnoremap <Leader>0p :e ~/.config/pylintrc<CR>
nnoremap <Leader>b :call system('echo "b ' . expand('%:p') . ':' . line('.') . '" >> ~/.config/pudb/saved-breakpoints-2.7')<CR>
imap <Leader>( (<CR><Esc>O
nmap <Leader>K :!qutebrowser https://docs.python.org/3/library/<C-R><C-W> &> /dev/null &<CR><CR>
nmap <Leader>i <Leader>S:Silent isort --line-width=79 -fss -a "import " <C-R>=expand('%')<CR><C-b><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right>
nmap <Leader><Leader>i <Leader>S:Silent isort --line-width=79 -fss -a "from " <C-R>=expand('%')<CR><C-b><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right>
nmap <LocalLeader>i <Leader>S:Silent isort --line-width=79 -fss -a "from typing import " <C-R>=expand('%')<CR><C-b><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right><right>
nmap <Leader>t :call SwitchToTest('py')<CR>
nmap <Leader><Leader>t :!pyinit -n -t <C-R>=expand('%:r')<CR><CR>
nmap <Leader>T :call VSwitchToTest('py')<CR>

" ----- pytest -----
nnoremap <Leader><C-]> /\v^\s*def <C-r><C-w><CR>

" ----- macros -----
let @c='A  # pragma: no cover'
let @d='kJxJx'  " Join docstring
let @o='F(af)ikA,'
let @s='^lla$hhik^'  " Split docstring
let @t='yiwmm?from typingofrom typing pmtgg/from typingV`t:sort`m'

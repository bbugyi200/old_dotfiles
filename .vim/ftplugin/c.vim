packadd termdebug

let b:run_cmd = "!make"
let g:comment_char = '/'

command! -nargs=0 CacheHeaders !cache_cheaders

nnoremap <buffer> <Leader>; A;<Esc>
imap <buffer> <Leader>; <Esc><Leader>;
imap <buffer> <Leader>i <C-x><C-k><C-p>
imap <buffer> <Leader>I #include <><Left><Leader>i
nnoremap <buffer> <Leader>m :Man 3 <C-r><C-w><CR>/^[A-Z]\+<CR>gg
nnoremap <buffer> <Leader>M :e Makefile<CR>
nnoremap <buffer> <Leader>t :execute "find test_" . expand('%:t:r') . ".cc"<CR>
nnoremap <buffer> <Leader>T :execute "tabfind test_" . expand('%:t:r') . ".cc"<CR>

setlocal dictionary+=/home/bryan/Sync/var/cheaders.txt
setlocal iskeyword+=.,/,-

" Do NOT place anything else below this conditional.
if exists('*SwitchSourceHeader')
    finish
endif

function! SwitchToCTest()
    try
        call SwitchToTest('c')
    catch
        try
            call SwitchToTest('cc')
        catch
            call SwitchToTest('cpp')
        endtry
    endtry
endfunction

function! VSwitchToCTest()
    try
        call VSwitchToTest('c')
    catch
        try
            call VSwitchToTest('cc')
        catch
            call VSwitchToTest('cpp')
        endtry
    endtry
endfunction

" Opens corresponding .c / .cc  file if .h file is open.
" Opens corresponding .h  file if .c / .cc file is open.
" This function MUST be defined at the bottom of the file.
function! SwitchSourceHeader()
    let ext = expand('%:e')
    if ext == "cc" || ext == 'c' || ext == 'cpp'
        let header = expand('%:t:r') . ".h"
        if header =~ "test_.*"
            let header = split(header, "_")[1]
        endif
        exec "find " . header
    else
        try
            find %:t:r.c
        catch
            try
                find %:t:r.cc
            catch
                find %:t:r.cpp
            endtry
        endtry
    endif
endfunction

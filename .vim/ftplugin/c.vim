NeoIncludeMakeCache

" Man Pages
nnoremap <Leader>m :Man 3 <C-r><C-w><CR>/^[A-Z]\+<CR>gg

" Open Makefile
nnoremap <Leader>M :e Makefile<CR>

nnoremap <Leader>N :NeoIncludeMakeCache<CR>

nmap <Leader>h :call SwitchSourceHeader()<CR>

" Do NOT place anything except SwitchSourceHeader below this conditional.
if exists('*SwitchSourceHeader')
    finish
endif

" Opens corresponding .c / .cc  file if .h file is open.
" Opens corresponding .h  file if .c / .cc file is open.
" This function MUST be defined at the bottom of the file.
function! SwitchSourceHeader()
  let ext = expand('%:e')
  if ext == "cc" || ext == 'c' || ext == 'cpp'
    find %:t:r.h
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

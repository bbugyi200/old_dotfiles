" command! -nargs=0 Run call VimuxRunCommand("tmux select-pane -t bottom ;" 
"             \ ." cd \"" .expand("%:p:h") ."\" && pdflatex " .expand("%:t") 
"             \ ." && clear ; tmux select-pane -t top")<CR>

command! -nargs=0 Run cd %:p:h | exec ":!pdflatex -file-line-error " .expand('%:t')

command! -nargs=0 Run2 call VimuxRunCommand("(zathura \"" .expand("%:p:r") .".pdf\" &> /dev/null &) && exit")

setlocal errorformat=%f:%l:\ %m,%f:%l-%\\d%\\+:\ %m
if filereadable('Makefile')
  setlocal makeprg=make
else
  exec "setlocal makeprg=make\\ -f\\ $CONFIG/makefiles/latex.mk\\ " . substitute(bufname("%"),"tex$","pdf", "")
endif

inoremap <C-l> <esc>bi\<esc>ea<C-n>

set colorcolumn=

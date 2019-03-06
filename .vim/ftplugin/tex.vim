let b:run_cmd = "cd %:p:h | !pdflatex -file-line-error " . expand('%:t') . " && rm *.out *.aux *.log"
let b:Run_cmd = 'call VimuxRunCommand("(zathura \"' . expand("%:p:r") . '.pdf\" &> /dev/null &) && exit")'

setlocal errorformat=%f:%l:\ %m,%f:%l-%\\d%\\+:\ %m
if filereadable('Makefile')
  setlocal makeprg=make
else
  exec "setlocal makeprg=make\\ -f\\ $CONFIG/makefiles/latex.mk\\ " . substitute(bufname("%"),"tex$","pdf", "")
endif

inoremap <C-l> <esc>bi\<esc>ea<C-n>

set colorcolumn=

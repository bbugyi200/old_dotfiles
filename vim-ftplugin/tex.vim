command! -nargs=0 Run call VimuxRunCommand("tmux select-pane -t bottom ;" 
            \ ." cd \"" .expand("%:p:h") ."\" && pdflatex " .expand("%:t") 
            \ ." && clear ; tmux select-pane -t top")<CR>

command! -nargs=0 Run2 call VimuxRunCommand("gnome-terminal -x okular \"" .expand("%:p:r") .".pdf\"")

set colorcolumn=

setlocal spell spelllang=en_us

set tw=99
command! -nargs=0 Run exec ":!cd docs; make html && wmctrl -s 2 && sleep 0.25 && xdotool key r && wmctrl -s 1"
command! -nargs=0 Run2 exec ":!cd docs; qutebrowser _build/html/index.html"

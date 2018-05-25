if has('win32')
    command! -nargs=* Run !py "%"
    nmap <F8> :w<CR>:!py -i "%"<CR> 
else
    command! -nargs=* Run call VimuxRunCommandInDir("clear &&"
                    \ ."tmux select-pane -t bottom &&" 
                    \ ." python <args>", 1)
    nmap <F8> :w<CR>:Run -i<CR> 
endif

nnoremap <Leader>8 A  # noqa: E
nnoremap <Leader>T :e ~/Dropbox/scripts/templates/template.py<CR>
nnoremap <Leader>" i"<CR>"<Esc>
nnoremap <Leader>' i"<CR>'<Esc>

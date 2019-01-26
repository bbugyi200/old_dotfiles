set backspace=2
set clipboard=unnamedplus

nnoremap <CR> :wq<CR> | imap <CR> <Esc><CR>
nnoremap dS maviwo<Esc>hx<Esc>elx<Esc>`ah
nnoremap S maviwo<Esc>i"<Esc>ea"<Esc>`al
nnoremap ,q :s/\(-t \\|\/\)//g<CR><CR>
nnoremap ,/ :s/\(:open \(-t \)\?\)/\1\//<CR>A
nnoremap ] /\v:open (-t )?\zs.<CR>
nmap [ ]hi 

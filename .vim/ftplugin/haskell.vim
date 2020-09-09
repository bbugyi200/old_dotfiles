let b:run_cmd = ":ALEFix"
let g:comment_char = '-'

" for hacking on xmonad
autocmd BufRead,BufNewFile ~/.xmonad/* call s:add_xmonad_path()
function! s:add_xmonad_path()
  if !exists('b:ghcmod_ghc_options')
    let b:ghcmod_ghc_options = []
  endif
  call add(b:ghcmod_ghc_options, '-i' . expand('~/.xmonad/lib'))
  let pkgdbs = system("stack exec env 2>/dev/null | grep PACKAGE_SANDBOXES | sed 's/^.*=//' | sed 's/:/\\n/g' | head -n-1")
  for pkgdb in split(pkgdbs)
    call add(b:ghcmod_ghc_options, '-package-db ' . pkgdb)
  endfor
endfunction

" ghcmod-vim
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

" Tabular
nmap <Leader>a- :Tabularize /-><CR>
vmap <Leader>a- :Tabularize /-><CR>
nmap <Leader>a_ :Tabularize /--><CR>
vmap <Leader>a_ :Tabularize /--><CR>

set backspace=2

" yank to clipboard
if has("clipboard")
  set clipboard=unnamed " copy to the system clipboard

  if has("unnamedplus") " X11 support
    set clipboard+=unnamedplus
  endif
endif

nnoremap <CR> :wq<CR> | imap <CR> <Esc><CR>
nnoremap Y y$
nnoremap ,q :s/\(-t \\|\/\)//g<CR><CR>
nnoremap ,/ :s/\(:open \(-t \)\?\)/\1\//<CR>A
nnoremap ] /\v:open (-t )?\zs.<CR>
nmap [ ]hi 
nnoremap <C-]> 04W<C-a>

" #################### VIM PLUGIN CONFIGURATIONS ####################
set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'tpope/vim-surround'

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on

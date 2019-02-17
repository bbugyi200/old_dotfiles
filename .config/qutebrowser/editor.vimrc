set backspace=2
set clipboard=unnamedplus

nnoremap <CR> :wq<CR> | imap <CR> <Esc><CR>
nnoremap ,q :s/\(-t \\|\/\)//g<CR><CR>
nnoremap ,/ :s/\(:open \(-t \)\?\)/\1\//<CR>A
nnoremap ] /\v:open (-t )?\zs.<CR>
nmap [ ]hi 

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

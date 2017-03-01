" ------------------ BEGIN:: Vundle Configurations -----------------------------
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim    
call vundle#begin()

" -------------------------------------------------------------------------
" ------------------------------ Simple Plugins ---------------------------
" -------------------------------------------------------------------------
" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-surround'

Plugin 'scrooloose/nerdTree'

Plugin 'itspriddle/ZoomWin'

Plugin 'kien/ctrlp.vim'
let g:ctrlp_follow_symlinks=1

Plugin 'tpope/vim-commentary'

Plugin 'danro/rename.vim'

Plugin 'christoomey/vim-tmux-navigator'

Plugin 'benmills/vimux'

Plugin 'jamessan/vim-gnupg'

Plugin 'jez/vim-superman'

Plugin 'shime/vim-livedown'

Plugin 'tpope/vim-unimpaired'

Plugin 'tpope/vim-fugitive'

Plugin 'Raimondi/delimitMate'


" ------------------------------ Airline Status Bar Configs -------------------

" The following set commands are used to fix issue with delay in Airline
" picking up on change from Insert to Normal mode. BEWARE of possible issues
" caused by setting timeout too low.
set timeoutlen=1000 ttimeoutlen=10

set laststatus=2

" Powerline Status Bar
set rtp+=$POWERLINE_DIRECTORY/powerline/bindings/vim/
set t_Co=256

" ------------------------------ END:: Airline Status Bar Configs --------------

" -------- UltiSnips Configurations -------
"
Plugin 'SirVer/ultisnips'
" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

" Allows other directories to be searched for snippet files
let g:UltiSnipsSnippetDirectories=["UltiSnips", "$CONFIG/UltiSnips"]

" ---------- END:: UltiSnip Configs --------                                          

" -------------------------------------------------------------------------
" -------------------------- END:: Simple Plugins -------------------------
" -------------------------------------------------------------------------


" -------------------------------------------------------------------------
" ------------------------------ Complex Plugins --------------------------
" -------------------------------------------------------------------------
"

" ------------------ Syntastic Config ----------------------
if $VIMID=="Home" || $VIMID=="Laptop"

Plugin 'scrooloose/syntastic'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_python_exec = '/path/to/python3'

" Disables certain Python Flake8 Errors
let g:syntastic_python_checkers=['flake8']
let g:syntastic_python_flake8_args='--ignore=E701,E702'

" Forces Syntastic to check header files for errors
" By default, Syntastic does not check .h file types
let g:syntastic_c_include_dirs=['../../include','../include', 'include']
let g:syntastic_c_check_header=1

" Adds c++11 support to error checking logic
let g:syntastic_cpp_compiler_options = '-std=c++11'

endif
" ----------------- END:: Syntastic Config ------------------

" ------------------------ YCM/NeoComplete Configuration ----------------------
" -----------------------------------------------------------------------------
if $VIMID=="Home" || $VIMID=="Laptop"

" ------- UltiSnips Integration w/ Autocomplete --------------
function! g:UltiSnips_Complete()
    call UltiSnips#ExpandSnippet()
    if g:ulti_expand_res == 0
        if pumvisible()
            return "\<C-n>"
        else
            call UltiSnips#JumpForwards()
            if g:ulti_jump_forwards_res == 0
               return "\<TAB>"
            endif
        endif
    endif
    return ""
endfunction

au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsListSnippets="<c-e>"

" this mapping Enter key to <C-y> to chose the current highlight item 
" and close the selection list, same as other IDEs.
" CONFLICT with some plugins like tpope/Endwise

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" ------------ END:: Ultisnips Integration w/ Autocomplete -----------

Plugin 'Shougo/neocomplete.vim'

"""""""""""""""""""""""""""""""
"  Color Scheme of Drop-Down  "

hi Pmenu ctermbg=darkgrey
hi Pmenu ctermfg=white
"""""""""""""""""""""""""""""""


let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:neocomplete#enable_at_startup = 1


" -------- Recommended NeoComplete Config ---------

"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.

let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
"let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

" ------------- END:: Recommended Neocomplete Config ---------------

endif
" ------------------- END:: YCM/NeoComplete Configuration ----------------------
" -----------------------------------------------------------------------------

" -------------------------------------------------------------------------
" -------------------------- END:: Complex Plugins ------------------------
" -------------------------------------------------------------------------

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line"""
"
" -------------------- END:: Vundle Configurations -----------------------------


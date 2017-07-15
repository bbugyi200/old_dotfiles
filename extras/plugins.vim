" ------------------------------------------------------------------------------
" ------------------ BEGIN:: Vundle Configurations -----------------------------
filetype off                  " required
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

" ~~~~~ tpope ~~~~~
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'

" ~~~~~ Shougo ~~~~~
Plugin 'Shougo/neocomplete.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/neoinclude.vim'
Plugin 'Shougo/echodoc.vim'

" "~~~~~ MISC ~~~~~
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdTree'
Plugin 'itspriddle/ZoomWin'
Plugin 'kien/ctrlp.vim'
Plugin 'danro/rename.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'benmills/vimux'
Plugin 'jamessan/vim-gnupg'
Plugin 'shime/vim-livedown'
Plugin 'Raimondi/delimitMate'
Plugin 'vim-airline/vim-airline'
Plugin 'davidhalter/jedi-vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'artur-shaik/vim-javacomplete2'
Plugin 'Rip-Rip/clang_complete'
Plugin 'scrooloose/syntastic'

Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

Plugin 'vim-utils/vim-man'
Plugin 'jez/vim-superman'


" -------------------- END:: Vundle Configurations -----------------------------
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" --------------------------------- CTRLP --------------------------------------
let g:ctrlp_follow_symlinks=1

" ------------------------------- EchoDoc --------------------------------------
let g:echodoc#enable_at_startup=1

" ------------------------------- Airline Status Bar Configs -------------------

" The following set commands are used to fix issue with delay in Airline
" picking up on change from Insert to Normal mode. BEWARE of possible issues
" caused by setting timeout too low.
set timeoutlen=1000 ttimeoutlen=10

set laststatus=2

let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" Powerline Status Bar
set rtp+=$POWERLINE_DIRECTORY/powerline/bindings/vim/
set t_Co=256


" ------------------------------ UltiSnips Configurations ----------------------
"
" Allows other directories to be searched for snippet files
let g:UltiSnipsSnippetDirectories=["UltiSnips", "/home/bryan/Dropbox/dotfiles/extra/UltiSnips"]

" ------------------------------- Syntastic Config -----------------------------
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_python_exec = '/usr/bin/python3'

" Disables certain Python Flake8 Errors
let g:syntastic_python_checkers=['flake8']
let g:syntastic_python_flake8_args='--ignore=E701,E702'

" Forces Syntastic to check header files for errors
" By default, Syntastic does not check .h file types
let g:syntastic_c_include_dirs=['../../include','../include', 'include']
let g:syntastic_c_check_header=1

" Adds c++11 support to error checking logic
let g:syntastic_cpp_compiler_options = '-std=c++11'

" Enables Syntastic to work with Java
" let g:syntastic_java_checker = 'javac'
let g:syntastic_java_javac_classpath = "./lib/*.jar\n./src"


" ----------------------------- NeoComplete Configuration ----------------------
"
" let g:neocomplete#enable_auto_close_preview=1
set completeopt-=preview

"""""""""""""""""""""""""""""""
"  Color Scheme of Drop-Down  "

hi Pmenu ctermbg=darkgrey
hi Pmenu ctermfg=white
"""""""""""""""""""""""""""""""


" ~~~~~ UltiSnips Integration w/ Autocomplete ~~~~~
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

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" ~~~~~ Recommended NeoComplete Config ~~~~~

"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
"
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
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType java setlocal omnifunc=javacomplete#Complete
autocmd FileType c setlocal omnifunc=ClangComplete

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif

if !exists('g:neocomplete#force_omni_input_patterns')
	let g:neocomplete#force_omni_input_patterns = {}
endif
autocmd FileType python setlocal omnifunc=jedi#completions
let g:jedi#auto_vim_configuration = 0
let g:jedi#popup_on_dot = 0
let g:neocomplete#force_omni_input_patterns.python = '[^. \t]\.\w*'

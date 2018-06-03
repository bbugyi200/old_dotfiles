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
Plugin 'Shougo/unite.vim'

" ~~~~~ TaskWarrior ~~~~~
Plugin 'vimwiki/vimwiki'
Plugin 'tbabej/taskwiki'
Plugin 'blindFS/vim-taskwarrior'
Plugin 'powerman/vim-plugin-AnsiEsc'

" "~~~~~ MISC ~~~~~
Plugin 'VundleVim/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'danro/rename.vim'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'benmills/vimux'
Plugin 'jamessan/vim-gnupg'
Plugin 'shime/vim-livedown'
Plugin 'Raimondi/delimitMate'
Plugin 'davidhalter/jedi-vim'
Plugin 'altercation/vim-colors-solarized'
" Plugin 'artur-shaik/vim-javacomplete2'
Plugin 'Rip-Rip/clang_complete'
Plugin 'scrooloose/syntastic'
Plugin 'embear/vim-localvimrc'
Plugin 'dyng/ctrlsf.vim'
Plugin 'ap/vim-buftabline'
Plugin 'KabbAmine/zeavim.vim'
Plugin 'gu-fan/riv.vim'

Plugin 'eagletmt/ghcmod-vim'
Plugin 'neovimhaskell/haskell-vim'

Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

Plugin 'vim-utils/vim-man'
Plugin 'jez/vim-superman'


" -------------------- END:: Vundle Configurations -----------------------------
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

let g:task_rc_override = 'rc._forcecolor=off'
let g:riv_disable_folding = 1
autocmd FileType txt setlocal commentstring=//\ %s

" --------------------------- VIMWIKI ------------------------------------------
let g:vimwiki_list = [{'path': '~/.vimwiki/', 'path_html': '~/.vimwiki-html/'}]
" Disables ,swp mapping created by AnsiEsc plugin
let g:no_cecutil_maps = 1 

" -------------------------- BUFTABLINE ----------------------------------------
hi! link BufTabLineCurrent PmenuSel
hi! link BufTabLineActive TabLine
let g:buftabline_numbers = 1
let g:buftabline_indicators = 1

" ------------------------- CTRLSF ---------------------------------------------
let g:ctrlsf_regex_pattern = 1
let g:ctrlsf_default_root = 'project'

" ------------------------- JEDI-VIM -------------------------------------------
" Add the virtualenv's site-packages to vim path
if has('python')
py << EOF
import os.path
import sys
import vim
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    sys.path.insert(0, project_base_dir)
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF
endif

let g:jedi#show_call_signatures = 0
let g:jedi#goto_command = "<C-]>"
let g:jedi#goto_assignments_command = ""

" ----------------------------- NECO-GHC ---------------------------------------
" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
let g:necoghc_enable_detailed_browse = 1

" ----------------------------- HASKELL-VIM ------------------------------------
let g:haskell_indent_disable=1

" ------------------------------LOCALVIMRC -------------------------------------
let g:localvimrc_sandbox=0
let g:localvimrc_ask=0

" --------------------------------- CTRLP --------------------------------------
let g:ctrlp_follow_symlinks=1

" Setup some default ignores
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site|coverage|venv|var)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
\}

" Use the nearest .git directory as the cwd
" This makes a lot of sense if you are working on a project that is in version
" control. It also supports works with .svn, .hg, .bzr.
let g:ctrlp_working_path_mode = 'r'

" ------------------------------ UltiSnips Configurations ----------------------
"
" Allows other directories to be searched for snippet files
" let g:UltiSnipsSnippetDirectories=["UltiSnips", "/home/bryan/Dropbox/dotfiles/extra/UltiSnips"]
let g:UltiSnipsSnippetDirectories=["UltiSnips"]

" ------------------------------- Syntastic Config -----------------------------
let g:Tex_IgnoredWarnings =
            \ 'Command terminated with space.'."\n"
" set laststatus=2
" set statusline+=[%f]
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_python_exec = '/usr/bin/python3'

" Set checkers for specific filetypes
" You can disable specific warnings for <checker> by using
" the 'g:syntastic_<ext>_<checker>_args' variable
let g:syntastic_python_checkers=['flake8']
let g:syntastic_tex_checkers=['chktex']
let g:syntastic_sh_checkers=['shellcheck']
let g:syntastic_rst_checkers=['sphinx']

let g:syntastic_tex_chktex_args='-n 1'

" Forces Syntastic to check header files for errors
" By default, Syntastic does not check .h file types
let g:syntastic_c_include_dirs=['../../include','../include', 'include']
let g:syntastic_c_check_header=1

" Adds c++11 support to error checking logic
let g:syntastic_cpp_compiler_options = '-std=c++11'

" Enables Syntastic to work with Java
" let g:syntastic_java_checker = 'javac'
let g:syntastic_java_javac_classpath=fnamemodify(getcwd(), ':h')."/bin:".getcwd()


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
let g:neocomplete#force_omni_input_patterns.python = '[^. \t]\.\w*'

" ############################## VIM PLUGIN CONFIGURATIONS ##############################
""""""""""""""""
"  BufTabLine  "
""""""""""""""""
if PluginInstalled("buftabline")
    hi! link BufTabLineCurrent PmenuSel
    hi! link BufTabLineActive TabLine
    let g:buftabline_numbers = 1
    let g:buftabline_indicators = 1
endif

"""""""""""""""""""
"  ClangComplete  "
"""""""""""""""""""
if PluginInstalled("clang_complete")
    let g:clang_complete_auto = 1
    let g:clang_use_library=1
endif

"""""""""""
"  CtrlP  "
"""""""""""
if PluginInstalled("ctrlp")
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
    
    let g:ctrlp_prompt_mappings = {
        \ 'PrtSelectMove("j")':   ['<c-j>', '<c-n>'],
        \ 'PrtSelectMove("k")':   ['<c-k>', '<c-p>'],
        \ 'PrtHistory(-1)':       ['<down>'],
        \ 'PrtHistory(1)':        ['<up>'],
        \ }
endif

""""""""""""
"  CtrlSF  "
""""""""""""
if PluginInstalled("ctrlsf")
    let g:ctrlsf_position = 'top'
    let g:ctrlsf_regex_pattern = 1
    let g:ctrlsf_default_root = 'project'
    
    let g:ctrlsf_mapping = {
                \ "next": "n",
                \ "prev": "N",
                \ }
endif

""""""""""""""
"  Deoplete  "
""""""""""""""
if PluginInstalled("deoplete")
    " Disable AutoComplPop.
    let g:acp_enableAtStartup = 0
    " Use deoplete.
    let g:deoplete#enable_at_startup = 1
    " Use smartcase.
    let g:deoplete#enable_smart_case = 1

    let g:deoplete#sources#clang#libclang_path = "/usr/lib64/llvm/6/lib64/libclang.so"
    let g:deoplete#sources#clang#std#cpp = 'c++1z'
    let g:deoplete#sources#clang#sort_algo = 'priority'
    let g:deoplete#sources#clang#flags = [
        \ "-stdlib=libc++",
        \ ]
    
    " Set minimum syntax keyword length.
    let g:deoplete#sources#syntax#min_keyword_length = 3
    let g:deoplete#lock_buffer_name_pattern = '\*ku\*'
    
    " Define dictionary.
    let g:deoplete#sources#dictionary#dictionaries = {
        \ 'default' : '',
        \ 'vimshell' : $HOME.'/.vimshell_hist',
        \ 'scheme' : $HOME.'/.gosh_completions'
            \ }
    
    " Define keyword.
    if !exists('g:deoplete#keyword_patterns')
        let g:deoplete#keyword_patterns = {}
    endif
    let g:deoplete#keyword_patterns['default'] = '\h\w*'
    
    " Enable omni completion.
    autocmd FileType c,cpp,cc,h setlocal omnifunc=ClangComplete
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType java setlocal omnifunc=javacomplete#Complete
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=jedi#completions
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
    
    call g:deoplete#custom#option({
                \ 'auto_complete': v:false,
                \ })
endif

""""""""""""*
"  EasyTags "
"""""""""""""
if PluginInstalled("easytags")
    " doesn't work with universal-ctags
    let g:easytags_events = ['BufWritePost']
endif

""""""""""""""""
"  Haskell-vim "
""""""""""""""""
if filereadable("/usr/share/vim/vimfiles/after/ftplugin/haskell.vim")
    let g:haskell_indent_disable = 1
    let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
    let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
    let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
    let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
    let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
    let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
    let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
    let g:haskellmode_completion_ghc = 0  " Disable haskell-vim omnifunc
endif

""""""""""""""""""""""
"  Instant-Markdown  "
""""""""""""""""""""""
let g:instant_markdown_autostart = 0

""""""""""""""
"  Jedi-vim  "
""""""""""""""
if PluginInstalled("jedi")
    let g:jedi#auto_initialization = 1

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
    
    let g:jedi#popup_select_first = 0
    let g:jedi#show_call_signatures = 0
    let g:jedi#goto_command = "<C-]>"
    let g:jedi#goto_assignments_command = ""
    let g:jedi#rename_command = ""
endif

""""""""""""""""
"  LocalVimrc  "
""""""""""""""""
if PluginInstalled("localvimrc")
    let g:localvimrc_sandbox=0
    let g:localvimrc_ask=0
endif

""""""""""""""
"  Neco-GHC  "
""""""""""""""
if PluginInstalled("necoghc")
    let g:necoghc_enable_detailed_browse = 1
endif

"""""""""
"  Riv  "
"""""""""
if PluginInstalled("riv")
    let g:riv_disable_folding = 1
    let g:riv_ignored_imaps = "<Tab>,<S-Tab>"
    let g:riv_ignored_maps = "<CR>"
    autocmd FileType txt setlocal commentstring=//\ %s
endif

"""""""""""""""
"  Solarized  "
"""""""""""""""
if filereadable("/usr/share/vim/vimfiles/colors/solarized.vim")
    colorscheme solarized
endif

"""""""""""""""
"  Syntastic  "
"""""""""""""""
if PluginInstalled("syntastic")
    let g:Tex_IgnoredWarnings =
                \ 'Command terminated with space.'."\n"
    
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 0
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
    let g:syntastic_python_python_exec = '/usr/bin/python3'
    
    " Set checkers for specific filetypes
    " You can disable specific warnings for <checker> by using
    " the 'g:syntastic_<ext>_<checker>_args' variable
    let g:syntastic_cpp_checkers=['clang_check']
    let g:syntastic_python_checkers=['flake8']
    let g:syntastic_tex_checkers=['chktex']
    let g:syntastic_sh_checkers=['shellcheck']
    let g:syntastic_rst_checkers=['sphinx']

    let g:syntastic_cpp_cppcheck_args="--language=c++"
    let g:syntastic_tex_chktex_args='-n 1'
    
    " Forces Syntastic to check header files for errors
    " By default, Syntastic does not check .h file types
    let g:syntastic_c_include_dirs=['../../include','../include', 'include', 'src']
    let g:syntastic_c_check_header=1
    
    " Adds c++14 support to error checking logic
    let g:syntastic_cpp_compiler_options = '-std=c++14'
    
    " Enables Syntastic to work with Java
    " let g:syntastic_java_checker = 'javac'
    let g:syntastic_java_javac_classpath=fnamemodify(getcwd(), ':h')."/bin:".getcwd()
endif

"""""""""""""""""
"  TaskWarrior  "
"""""""""""""""""
if PluginInstalled("taskwarrior")
    let g:task_rc_override = 'rc._forcecolor=off'
endif

"""""""""""""""
"  UltiSnips  "
"""""""""""""""
" Facilitates Integration between UltiSnips and Autocomplete
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

if PluginInstalled("UltiSnips")
    " Allows other directories to be searched for snippet files
    " let g:UltiSnipsSnippetDirectories=["UltiSnips", "/home/bryan/Dropbox/dotfiles/extra/UltiSnips"]
    let g:UltiSnipsSnippetDirectories=["/home/bryan/Dropbox/dotfiles/extras/vim-snippets", getcwd()]

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
endif

"""""""""""""
"  Vimwiki  "
"""""""""""""
if PluginInstalled("vimwiki")
    let g:vimwiki_list = [{'path': '~/.vimwiki/', 'path_html': '~/.vimwiki-html/'}]
    " Disables ,swp mapping created by AnsiEsc plugin
    let g:no_cecutil_maps = 1 
endif

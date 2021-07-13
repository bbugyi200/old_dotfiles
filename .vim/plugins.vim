" ############################## VUNDLE CONFIGURATIONS#########################
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'airblade/vim-gitgutter'
Plugin 'Raimondi/delimitMate'
Plugin 'vim-scripts/taglist.vim'
Plugin 'mhinz/vim-startify'
Plugin 'chrisbra/csv.vim'
Plugin 'ap/vim-buftabline'
Plugin 'apeschel/vim-syntax-syslog-ng'
Plugin 'benmills/vimux'
Plugin 'cespare/vim-toml'
Plugin 'danro/rename.vim'
Plugin 'dense-analysis/ale'
Plugin 'dyng/ctrlsf.vim'
Plugin 'eagletmt/neco-ghc'  " Depends on external package: ghc-mod
Plugin 'embear/vim-localvimrc'
Plugin 'francoiscabrol/ranger.vim'
Plugin 'hiphish/info.vim'
Plugin 'jamessan/vim-gnupg'
Plugin 'jez/vim-superman'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'ludovicchabant/vim-gutentags'
Plugin 'manicmaniac/coconut.vim'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'pboettch/vim-cmake-syntax'
Plugin 'racer-rust/vim-racer'
Plugin 'Rip-Rip/clang_complete'  " Depends on external package: clang
Plugin 'rust-lang/rust.vim'
Plugin 'Shougo/neoinclude.vim'
Plugin 'SirVer/ultisnips'
Plugin 'tmux-plugins/vim-tmux-focus-events'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'vim-scripts/AnsiEsc.vim'
Plugin 'vim-scripts/applescript.vim'
Plugin 'vim-scripts/cecutil'
Plugin 'Vimjas/vim-python-pep8-indent'
Plugin 'VundleVim/Vundle.vim'
Plugin 'Konfekt/FastFold'
Plugin 'z0mbix/vim-shfmt'
Plugin 'tmhedberg/SimpylFold'
Plugin 'raimon49/requirements.txt.vim'
Plugin 'Glench/Vim-Jinja2-Syntax'
Plugin 'martinda/Jenkinsfile-vim-syntax'
Plugin 'neoclide/coc.nvim', {'branch': 'release'}

" All of your Plugins must be added before the following line
call vundle#end()
filetype plugin indent on


" ############################## PLUGIN CONFIGURATIONS#########################
" Predicate which checks if @plugin is installed
function! PluginInstalled(plugin)
    if filereadable("/usr/share/vim/vimfiles/plugin/" . a:plugin . ".vim")
        return 1
    endif

    if !empty(glob($HOME . "/.vim/bundle/" . a:plugin))
        return 1
    endif

    if !empty(glob($HOME . "/.vim/bundle/vim-" . a:plugin))
        return 1
    endif

    if !empty(glob($HOME . "/.vim/bundle/" . a:plugin . "-vim"))
        return 1
    endif

    if !empty(glob($HOME . "/.vim/bundle/" . a:plugin . ".vim"))
        return 1
    endif

    if !empty(glob($HOME . "/.vim/bundle/" . tolower(a:plugin)))
        return 1
    endif

    return 0
endfunction

"""""""
" ALE "
"""""""
if PluginInstalled("ale")
    let g:ale_fixers = {'python': ['black'], 'sh': ['shfmt'], 'haskell': ['stylish-haskell']}

    let ale_python_mypy_options = "--namespace-packages"
    if ! empty($VIRTUAL_ENV)
        let ale_python_mypy_options="--python-executable=python " . ale_python_mypy_options
    endif

    let g:ale_python_black_options = get(g:, "ale_python_black_options", "--line-length=79 --experimental-string-processing")
    let g:ale_python_mypy_options = get(g:, "ale_python_mypy_options", ale_python_mypy_options)
    let g:ale_python_pylint_options = get(g:, "ale_python_pylint_options", "--rcfile=~/.config/pylintrc")

    let g:ale_sh_shellcheck_options = get(g:, "ale_sh_shellcheck_options", "--shell=bash")

    let g:ale_fix_on_save = get(g:, "ale_fix_on_save", 0)
endif

"""""""""""
"  Black  "
"""""""""""
if PluginInstalled("black")
    let g:black_fast = 0
    let g:black_skip_string_normalization = 1
    let g:black_linelength = get(g:, "black_linelength", 79)
endif

""""""""""""""""
"  BufTabLine  "
""""""""""""""""
if PluginInstalled("buftabline")
    let g:buftabline_numbers = 1
    let g:buftabline_indicators = 1

    hi BufTabLineCurrent cterm=bold,underline ctermbg=lightgreen
    hi BufTabLineActive ctermbg=lightgreen
    hi BufTabLineHidden cterm=nocombine,none ctermbg=lightgrey
endif

"""""""""""""""""""
"  ClangComplete  "
"""""""""""""""""""
if PluginInstalled("clang_complete")
    let g:clang_complete_auto = 1
    let g:clang_use_library = 1
    let hostname = system("hostname")

    let g:clang_library_path = "/usr/lib/llvm-11/lib/libclang.so.1"
endif

""""""""""""""
"  coc.nvim  "
""""""""""""""
if PluginInstalled('coc.nvim')
    " Set internal encoding of vim, not needed on neovim, since coc.nvim using some
    " unicode characters in the file autoload/float.vim
    set encoding=utf-8

    " TextEdit might fail if hidden is not set.
    set hidden

    " Some servers have issues with backup files, see #649.
    set nobackup
    set nowritebackup

    " Give more space for displaying messages.
    set cmdheight=2

    " Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
    " delays and poor user experience.
    set updatetime=300

    " Don't pass messages to |ins-completion-menu|.
    set shortmess+=c

    " Always show the signcolumn, otherwise it would shift the text each time
    " diagnostics appear/become resolved.
    if has("nvim-0.5.0") || has("patch-8.1.1564")
      " Recently vim can merge signcolumn and number column into one
      set signcolumn=number
    else
      set signcolumn=yes
    endif

    " Use tab for trigger completion with characters ahead and navigate.
    " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
    " other plugin before putting this into your config.
    inoremap <silent><expr> <TAB>
          \ pumvisible() ? "\<C-n>" :
          \ <SID>check_back_space() ? "\<TAB>" :
          \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction

    " Use <c-space> to trigger completion.
    if has('nvim')
      inoremap <silent><expr> <c-space> coc#refresh()
    else
      inoremap <silent><expr> <c-@> coc#refresh()
    endif

    " Make <CR> auto-select the first completion item and notify coc.nvim to
    " format on enter, <cr> could be remapped by other vim plugin
    inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                                  \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

    " Use `[g` and `]g` to navigate diagnostics
    " Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
    nmap <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <silent> ]g <Plug>(coc-diagnostic-next)

    " GoTo code navigation.
    nmap <silent> <C-]> <Plug>(coc-definition)
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)

    " Use K to show documentation in preview window.
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
      else
        execute '!' . &keywordprg . " " . expand('<cword>')
      endif
    endfunction

    " Highlight the symbol and its references when holding the cursor.
    autocmd CursorHold * silent call CocActionAsync('highlight')

    " Symbol renaming.
    " nmap <leader>rn <Plug>(coc-rename)

    " Formatting selected code.
    xmap <leader>f  <Plug>(coc-format-selected)
    nmap <leader>f  <Plug>(coc-format-selected)

    augroup mygroup
      autocmd!
      " Setup formatexpr specified filetype(s).
      autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
      " Update signature help on jump placeholder.
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

    " Applying codeAction to the selected region.
    " Example: `<leader>aap` for current paragraph
    xmap <leader>a  <Plug>(coc-codeaction-selected)
    nmap <leader>a  <Plug>(coc-codeaction-selected)

    " Remap keys for applying codeAction to the current buffer.
    nmap <leader>ac  <Plug>(coc-codeaction)
    " Apply AutoFix to problem on the current line.
    nmap <leader>qf  <Plug>(coc-fix-current)

    " Map function and class text objects
    " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
    xmap if <Plug>(coc-funcobj-i)
    omap if <Plug>(coc-funcobj-i)
    xmap af <Plug>(coc-funcobj-a)
    omap af <Plug>(coc-funcobj-a)
    xmap ic <Plug>(coc-classobj-i)
    omap ic <Plug>(coc-classobj-i)
    xmap ac <Plug>(coc-classobj-a)
    omap ac <Plug>(coc-classobj-a)

    " Remap <C-f> and <C-b> for scroll float windows/popups.
    if has('nvim-0.4.0') || has('patch-8.2.0750')
      nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
      nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
      inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
      inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
      vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
      vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
    endif

    " Use CTRL-S for selections ranges.
    " Requires 'textDocument/selectionRange' support of language server.
    nmap <silent> <C-s> <Plug>(coc-range-select)
    xmap <silent> <C-s> <Plug>(coc-range-select)

    " Add `:Format` command to format current buffer.
    command! -nargs=0 Format :call CocAction('format')

    " Add `:Fold` command to fold current buffer.
    command! -nargs=? Fold :call     CocAction('fold', <f-args>)

    " Add `:OR` command for organize imports of the current buffer.
    command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

    " Add (Neo)Vim's native statusline support.
    " NOTE: Please see `:h coc-status` for integrations with external plugins that
    " provide custom statusline: lightline.vim, vim-airline.
    set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

    " Mappings for CoCList
    " Show all diagnostics.
    " nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
    " " Manage extensions.
    " nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
    " " Show commands.
    " nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
    " " Find symbol of current document.
    " nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
    " " Search workspace symbols.
    " nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
    " " Do default action for next item.
    " nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
    " " Do default action for previous item.
    " nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
    " " Resume latest coc list.
    " nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
endif

"""""""""""'""""
"  Conque-GDB  "
"""""""""""'""""
if PluginInstalled("Conque-GDB")
    let g:ConqueTerm_Color = 2         " 1: strip color after 200 lines, 2: always with color
    let g:ConqueTerm_CloseOnEnd = 0    " close conque when program ends running
    let g:ConqueTerm_StartMessages = 1 " display warning messages if conqueTerm is configured incorrectly

    let g:ConqueGdb_Leader = '<LocalLeader>'
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
                \ "quit": "Q",
                \ "pquit": "Q",
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

"""""""""""""""""
"  DelimitMate  "
"""""""""""""""""
if PluginInstalled('delimitMate')
endif

""""""""""""*
"  EasyTags "
"""""""""""""
if PluginInstalled("easytags")
    let g:easytags_suppress_ctags_warning = 1
    let g:easytags_events = ['BufWritePost']
endif

""""""""""""
" FastFold "
""""""""""""
if PluginInstalled("FastFold")
    let g:fastfold_savehook = 1
    let g:fastfold_fold_command_suffixes =  ['x','X']
    let g:fastfold_fold_movement_commands = []
endif

""""""""""""
" Fugitive "
""""""""""""
if PluginInstalled("vim-fugitive")
    hi DiffAdd cterm=BOLD ctermfg=black ctermbg=green
    hi DiffDelete cterm=BOLD ctermfg=black ctermbg=red
    hi DiffChange cterm=NONE ctermfg=NONE ctermbg=NONE
    hi DiffText cterm=BOLD ctermfg=black ctermbg=yellow
endif

""""""""""""*
"  fzf.vim  "
"""""""""""""
if PluginInstalled("fzf.vim")
    let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-w': 'split',
      \ 'ctrl-e': 'vsplit' }
endif

"""""""""""""
" GitGutter "
"""""""""""""
if PluginInstalled("vim-gitgutter")
    " autocmd BufWritePost * GitGutter
    let g:gitgutter_map_keys = 0

    function! GitGutterStatus()
        let [a, m, r] = GitGutterGetHunkSummary()
        if (a + m + r > 0)
            return printf(":+%d:~%d:-%d", a, m, r)
        else
            return ""
        endif
    endfunction

    function! GitGutterNextHunkCycle()
      let line = line('.')
      silent! GitGutterNextHunk
      if line('.') == line
        1
        GitGutterNextHunk
      endif
    endfunction

    function! GitGutterPrevHunkCycle()
      let line = line('.')
      silent! GitGutterPrevHunk
      if line('.') == line
        10000
        GitGutterPrevHunk
      endif
    endfunction

    nmap ]h :call GitGutterNextHunkCycle()<CR>
    nmap [h :call GitGutterPrevHunkCycle()<CR>
else
    function! GitGutterStatus()
        return ""
    endfunction
endif

""""""""""""""""
"  Haskell-vim "
""""""""""""""""
if PluginInstalled("haskell")
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
if PluginInstalled("instant-markdown")
    let g:instant_markdown_autostart = 0
endif

""""""""""""""
"  Jedi-vim  "
""""""""""""""
if PluginInstalled("jedi")
    " Fix color of jedi-vim's function signature highlighting.
    hi jediFat ctermbg=black ctermfg=green

    " Fix for Mac OS
    if has('macunix')
        py3 sys.executable='/usr/local/bin/python3'
    endif

    let g:jedi#auto_initialization = 1

    " Add the virtualenv's site-packages to vim path
    if has('python') || has('python3')
    py3 << EOF
import glob
import os.path
import sys
import vim

print(sys.path, file=open("/tmp/path.txt", "w"))
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    sys.path.insert(0, project_base_dir)
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    with open(activate_this) as f:
        code = compile(f.read(), activate_this, 'exec')
        exec(code, dict(__file__=activate_this))
EOF
    endif
    
    let g:jedi#completions_command = "<C-o>"
    let g:jedi#goto_assignments_command = ""
    let g:jedi#goto_command = ""
    let g:jedi#goto_stubs_command = ""
    let g:jedi#popup_select_first = 0
    let g:jedi#rename_command = ""
    let g:jedi#show_call_signatures = "1"
    let g:jedi#smart_auto_mappings = 1
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

""""""""""""
" NerdTree "
""""""""""""
if PluginInstalled("nerdtree")
    let NERDTreeIgnore = ['.pyc$']
endif

"""""""""
" Racer "
"""""""""
if PluginInstalled("vim-racer")
    let g:racer_experimental_completer = 1
endif

""""""""""""""
" Ranger.vim "
""""""""""""""
if PluginInstalled("ranger.vim")
    let g:ranger_map_keys = 0
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

""""""""""""
" rust.vim "
""""""""""""
if PluginInstalled("rust.vim")
   let g:rustfmt_command = $HOME . "/.cargo/bin/rustfmt" 
   let g:rustfmt_options = "--config-path " . $HOME . "/.config/rustfmt/rustfmt.toml"
endif

"""""""""""""""
"  Solarized  "
"""""""""""""""
if PluginInstalled("vim-colors-solarized")
    colorscheme solarized
endif

""""""""""""""
"  Startify  "
""""""""""""""
if PluginInstalled("vim-startify")
    let g:startify_change_to_dir = 0
endif

""""""""""""""""
"  SympylFold  "
""""""""""""""""
if PluginInstalled("SimpylFold")
    let g:SimpylFold_docstring_preview = 1
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
    let g:syntastic_aggregate_errors = 1
    
    " Set checkers for specific filetypes
    " You can disable specific warnings for <checker> by using
    " the 'g:syntastic_<ext>_<checker>_args' variable
    let g:syntastic_cpp_checkers=['clang_check']
    let g:syntastic_python_checkers=['flake8', 'pylint', 'mypy', ]
    let g:syntastic_tex_checkers=['chktex']
    let g:syntastic_sh_checkers=['shellcheck']
    let g:syntastic_rst_checkers=['sphinx']

    let g:syntastic_cpp_cppcheck_args="--language=c++"
    let g:syntastic_tex_chktex_args='-n 1'
    let g:syntastic_python_pylint_args='--rcfile=~/.config/pylintrc'
    
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

"""""""""""
" Taglist "
"""""""""""
if PluginInstalled("taglist.vim")
    let g:Tlist_Exit_OnlyWindow = 1
    let g:Tlist_GainFocus_On_ToggleOpen = 1
    let g:Tlist_WinWidth = 50
    let g:Tlist_Close_On_Select = 1
endif

"""""""""""""""""
"  TaskWarrior  "
"""""""""""""""""
if PluginInstalled("taskwarrior")
    let g:task_rc_override = 'rc._forcecolor=off'
endif

""""""""
" Toml "
""""""""
if PluginInstalled("vim-toml")
    hi tomlComment ctermfg=darkgrey
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
    " let g:UltiSnipsSnippetDirectories=["UltiSnips", $HOME . "/Sync/home/extra/UltiSnips"]
    let g:UltiSnipsSnippetDirectories=[$HOME . "/.vim/vim-snippets", getcwd() . "/snippets"]

    au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
    let g:UltiSnipsJumpForwardTrigger="<tab>"
    let g:UltiSnipsListSnippets="<c-u>"
    
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

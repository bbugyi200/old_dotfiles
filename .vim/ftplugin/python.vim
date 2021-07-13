function! DebugFoldExpr(lnum)
    return FoldExpr(a:lnum, "1")
endfunction


function! FoldExpr(lnum, debug)
    function! Ok(rv, msg) closure
        if a:debug == "1"
            echom "#" . a:lnum . ": " . a:msg . " [" . a:rv . "]"
        endif
        return a:rv
    endfunction

    let curr_line = getline(a:lnum)

    let curr_indent = indent(a:lnum)
    if curr_indent == 0 && strlen(curr_line) > 0
        return Ok('0', "Matched toplevel line.")
    endif

    let last_indent = curr_indent > 0 ? curr_indent : indent(a:lnum - 1)
    let lvl = last_indent <= 4 ? 1 : 2
    let RE_CLASS_OR_FUNC = '\v^(    )?(class [A-Za-z_0-9]+(\(.*\))?:|((async )?def [A-Za-z_0-9]+\(.*)?\)( -\> [^:]*)?:)(  # .*)?$'
    if curr_line =~ RE_CLASS_OR_FUNC
        let is_not_if_stmt_bug = IsNotIfStmt(a:lnum - 1)
        if is_not_if_stmt_bug
            return Ok(lvl, "Matched class or func!")
        endif
    endif

    let prev_nb_line = getline(PrevNonBlankLine(a:lnum))
    let prev_line = getline(a:lnum - 1)
    let prev_is_cls_or_def = (prev_line =~ RE_CLASS_OR_FUNC) ? 1 : 0

    let RE_TYPE_SIG = '\v^    (    )?# type: .*$'
    if curr_line =~ RE_TYPE_SIG && prev_is_cls_or_def
        return Ok(lvl - 1, "Matched type signature!")
    endif

    let prev_is_type = (prev_line =~ RE_TYPE_SIG) ? 1 : 0

    if curr_line =~ '\v^    (    )?("""|'''''')' && (prev_is_cls_or_def || prev_is_type)
        return Ok(lvl - 1, "Matched docstring!")
    endif

    let next_lnum = NextNonBlankLine(a:lnum)
    let next_indent = indent(next_lnum)

    let last_lvl = foldlevel(a:lnum - 1)

    " HACK: Prevents a bug where blank lines in docstrings get wrongly folded.
    let is_not_blank_in_docstring_bug = last_lvl > 0 || strlen(curr_line) > 0 || next_indent == 0
    let is_last_line = is_not_blank_in_docstring_bug && (next_indent == 0 || (lvl == 2 && next_indent == 4))

    if is_last_line
        let start_lvl = lvl - 1
    else
        let start_lvl = 'a1'
    endif

    " HACK: Prevents a bug where a blank line between a class and its docstring
    " causes the docstring to get folded.
    let is_not_blank_before_docstring_bug = (strlen(curr_line) > 0 || getline(next_lnum) !~ '\v^\s*("""|'''''').*$')
    if prev_is_cls_or_def && is_not_blank_before_docstring_bug
        let is_not_if_stmt_bug = IsNotIfStmt(a:lnum - 1)
        if is_not_if_stmt_bug
            return Ok(start_lvl, "Previous line matched class or func!")
        endif
    endif

    if prev_is_type
        return Ok(start_lvl, "Previous line matched type signature!")
    endif

    if prev_line =~ '\v^    (    )?("""|'''''').+("""|'''''')\s*$'
        return Ok(start_lvl, "Previous line matched FULL docstring!")
    endif

    if is_last_line && last_lvl != 0 && strlen(curr_line) == 0
        let blank_lvl = next_indent >= 4 ? 1 : 0
        return Ok(blank_lvl, "Blank line between folds!")
    endif

    if is_last_line && (last_lvl == lvl || last_lvl == -1)
        return Ok('s1', "Next line is on a higher fold level than last line <Last Lvl: " . last_lvl . ">.")
    endif

    let prev_prev_nb_line = getline(PrevNonBlankLine(a:lnum - 1))
    if (prev_line =~ '\v^    (    )?([^ ].*)?("""|'''''')\s*$') && (prev_prev_nb_line !~ RE_CLASS_OR_FUNC) && (prev_prev_nb_line !~ RE_TYPE_SIG)
        " HACK: Prevents a bug where assignments to triple-quoted strings look
        " like the end of docstrings.
        let is_not_docstring_assignment_bug = prev_line !~ '\v^.*\= ?[furbFURB]?("""|'''''')\s*$'
        if is_not_docstring_assignment_bug
            return Ok(start_lvl, "Previous line matched END of docstring!")
        endif
    endif

    return Ok(last_lvl >= 0 ? last_lvl : '=', "No matches were made! Default to last lines fold level!")
endfunction


" HACK: Prevents bug where a long if statement looks like a function
" definition.
function! IsNotIfStmt(prev_lnum)
    let prev_line = getline(a:prev_lnum)
    let indent = indent(a:prev_lnum)

    let spaces = indent == 4 ? "    " : ""

    if prev_line =~ '\v^' . spaces . '\):$'
        let n = a:prev_lnum - 1
        let ppline = getline(n)
        while n > 0 && ppline =~ '\v^    ' . spaces . '?.*$'
            let ppline = getline(n)
            let n -= 1
        endw

        if ppline !~ '\v^' . spaces . '(async )?def [A-Za-z_0-9]+\(.*$'
            return 0
        endif
    endif

    return 1
endfunction


function! NextNonBlankLine(lnum)
    let numlines = line('$')
    let current = a:lnum + 1

    while current <= numlines
        if getline(current) != ""
            return current
        endif

        let current += 1
    endwhile

    return -2
endfunction


function! PrevNonBlankLine(lnum)
    let current = a:lnum - 1

    while current > 0
        if getline(current) =~? '\v\S'
            return current
        endif

        let current -= 1
    endwhile

    return -2
endfunction


function! FromImport(pymod)
    let import_string = "from " . a:pymod . " import "
    let isort_cmd = GetISortCmd(import_string)
    call FeedCmd(isort_cmd)
endfunction


function! GetISortCmd(import_string)
    let isort_prefix = "Silent isort -a '"
    let isort_opts = "
                \ --line-width=79
                \ --lines-after-imports=2
                \ --force-sort-within-sections
                \ --combine-as
                \ --multi-line=3
                \ --trailing-comma
                \"

    let i = 0
    let right_keys = ""
    while i < (len(isort_prefix) + len(a:import_string))
        let right_keys = right_keys . "\<right>"
        let i += 1
    endw

    let isort_cmd = isort_prefix . a:import_string . "' " . isort_opts . " \<C-R>=expand('%')\<CR>\<C-b>" . right_keys
    return isort_cmd
endfunction


function! FeedCmd(cmd)
    call feedkeys(":" . a:cmd)
endfunction


function! MyBTags()
    if &foldlevel == "2"
        execute "BTags" | execute "normal zt"
    else
        execute "BTags" | execute "normal " . (foldlevel(line(".")) > 0 ? "zz" : "zt")
    endif
endfunction


let b:run_cmd = ":ALEFix"
let b:wrap_tw = 79

setlocal foldcolumn=3
setlocal foldexpr=FoldExpr(v:lnum, "0")
setlocal foldlevel=5
setlocal foldmethod=expr
setlocal indentkeys+==else
setlocal indentkeys-=<:>


" --- Symbol Mappings
nnoremap [om :let g:ale_fix_on_save = 0<CR>
nnoremap ]om :let g:ale_fix_on_save = 1<CR>
nnoremap <C-p>t :call MyBTags()<CR>
imap <Leader>( (<CR><Esc>O

" --- Alphanumeric Mappings
nnoremap <Leader>0f :e ~/.config/flake8<CR>
nnoremap <Leader>0p :e ~/.config/pylintrc<CR>
nnoremap <Leader>b :call system('echo "b ' . expand('%:p') . ':' . line('.') . '" >> ~/.config/pudb/saved-breakpoints-2.7')<CR>
inoremap <Leader>f <Esc>mmF"if<Esc>`mla
inoremap <Leader>F <Esc>mmF'if<Esc>`mla
nmap <Leader>i <Leader>S:call FeedCmd(GetISortCmd("import "))<CR>
nmap <LocalLeader>i <Leader>S:call FromImport("")<left><left>
nmap <LocalLeader>I <Leader>S:call FromImport("typing")<CR>
nmap <Leader>K :!qutebrowser https://docs.python.org/3/library/<C-R><C-W> &> /dev/null &<CR><CR>
nmap <Leader>S [om,s
nmap <Leader>t :call SwitchToTest('py')<CR>
nmap <Leader><Leader>t :!pyinit -n -t <C-R>=expand('%:r')<CR><CR>
nmap <Leader>T :call VSwitchToTest('py')<CR>
nnoremap <Leader>zz :call DebugFoldExpr(line("."))<CR>
nnoremap <Leader>zZ :debug call DebugFoldExpr(line("."))<CR>


" ----- macros -----
let @c='A  # pragma: no cover'
let @d='kJxJx'  " Join docstring
let @o='F(af)ikA,'
let @s='^lla$hhik^'  " Split docstring
let @t='yiwmm?from typingofrom typing pmtgg/from typingV`t:sort`m'

" ----- commands -----
command! Ipy execute "term ++close ipython"

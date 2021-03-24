" Vim universal .txt syntax file ErrorMsg

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn cluster txtContains add=Todo,BeginWS,Underlined,WildMenu

" Title
syn region WildMenu start="###" end="###$" contains=@NoSpell oneline

" Section
" syn match Statement "^[A-Z][^a-z]*[A-Z0-9)\]]$" contains=@txtContains,@NoSpell
" OLD --> [A-Z () 0-9 / \[\] : # -]

" Subsection
syn region Type start="^\s*===" end="===$" contains=@NoSpell oneline

" Subsubsection
syn region Function start="^\s*---" end="---$" contains=@NoSpell oneline

" Bullets
" syn match ModeMsg "^\s*\([*-]\|[A-Za-z0-9]\.\)" contains=@NoSpell

" Web Links
syn match Underlined "http\S*" contains=@NoSpell,EndP

" Comments
syn region Comment start="\/\/ " end="$" contains=@txtContains,@NoSpell oneline
syn region Comment start="# " end="$" contains=@txtContains,@NoSpell oneline
syn region Comment start="^#$" end="$" contains=@txtContains,@NoSpell oneline
syn region Comment start="\/\*" end="\*\/" contains=@txtContains,@NoSpell

" Highlights
" 'keepend' prevents contains items from extending the outer item
syn keyword Todo TODO NOTE FIXME
syn region Todo start="::" end="$" contains=@NoSpell oneline

syn region Todo start="<\[" end="\]>" oneline
" syn region ErrorMsg start="{" end="}" oneline

" HiLinks 
" Define the default highlighting. For version 5.7 and earlier: only when not done already For
" version 5.8 and later: only when an item doesn't have highlighting yet
if version < 508
    command -nargs=+ HiLink hi link <args>
else
    command -nargs=+ HiLink hi def link <args>
endif

HiLink BeginWS Cursor
HiLink EndP Cursor

delcommand HiLink


let b:current_syntax = "txt"

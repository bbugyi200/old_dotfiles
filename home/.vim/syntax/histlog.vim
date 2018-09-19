" ---------- WeeChat vim Syntax File ----------
" Used with 'vwchat' script to view custom WeeChat buffer logs

syn region Statement start="^\[" end="\]"
syn region Underlined start="(" end=")"
syn region Function start='/' end='$'
syn match None "^\t.*$"
syn match Comment "^#.*"

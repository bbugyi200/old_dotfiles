# ---------- Debian Aliases / Functions ----------
# def marker: DEBIAN
alias aptg='sudo apt-get'
alias atude='sudo aptitude'
alias del='sudo aptitude remove'
alias eqd='apt-cache rdepends --installed'
alias get='sudo aptitude install'
alias hibernate='debian-suspend -1'
alias pg='apt list --installed 2>/dev/null | grep -i'
alias pgh='pghist'
pghist() { grep --color=never -E "${1:-.*}" /var/log/dpkg.log; }
alias service='sudo service'

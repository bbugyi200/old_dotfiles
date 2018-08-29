####################################################
#  Gentoo / Portage Related Aliases and Functions  #
####################################################

alias del='sudo emerge --ask --rage-clean'
ebi() { sudo ebuild "$1" clean merge; }
ecd () { cd "$(find /usr/local/portage -type d -name "*$1*")" || return 1; }
alias edc='sudo dispatch-conf'
alias epu='sudo -i epuse'
alias epk='sudo vim /etc/portage/package.accept_keywords'
alias epm='sudo vim /etc/portage/package.mask'
alias epmk='sudo vim /etc/portage/make.conf /home/bryan/Dropbox/dotfiles/home/extras/make.conf'
alias epum='sudo vim /etc/portage/package.unmask'
alias erm='sudo repoman manifest'
alias eq='equery'
alias eqa='equery a'  # h(a)s                   list all packages for matching ENVIRONMENT data stored in /var/db/pkg
alias eqb='equery b'  # (b)elongs               list what package FILES belong to
alias eqc='equery c'  # (c)hanges               list changelog entries for ATOM
alias eqd='equery d'  # (d)epends               list all packages directly depending on ATOM
alias eqf='equery f'  # (f)iles                 list all files installed by PKG
alias eqg='equery g'  # dep(g)raph              display a tree of all dependencies for PKG
alias eqh='equery h'  # (h)asuse                list all packages that have USE flag
alias eqk='equery k'  # chec(k)                 verify checksums and timestamps for PKG
alias eql='equery l'  # (l)ist                  list package matching PKG
alias eqm='equery m'  # (m)eta                  display metadata about PKG
alias eqs='equery s'  # (s)ize                  display total size of all files owned by PKG
alias equ='equery u'  # (u)ses                  display USE flags for PKG
alias eqw='equery w'  # (w)hich                 print full path to ebuild for PKG
alias eqy='equery y'  # ke(y)words              display keywords for specified PKG
get() { sudo emerge --ask "$@" && nmw "The following packages are finished installing: $*."; }
alias pg='equery list "*" | grep'
alias rcs='sudo rc-service'
alias rcst='rc-status'
alias rcu='sudo rc-update'

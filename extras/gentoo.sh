####################################################
#  Gentoo / Portage Related Aliases and Functions  #
####################################################

alias del='sudo -i del'
Ebi() { eposync "$1" && sudo -i /usr/local/bin/get "$1"; }
Epatch() { cd "$(epatch "$@")" || return 1; }
ebi() { sudo repoman manifest && sudo ebuild "$1" clean merge; }
ecd () { cd "$(find /home/bryan/projects/portage-overlay -type d -name "*$1*" | head -n 1)" || return 1; }
egcd () { cd "$(find "$(portageq get_repo_path / gentoo)" -type d -name "*$1*" | head -n 1)" || return 1; }
alias edc='sudo dispatch-conf'
alias edep='sudo emerge --ask --depclean && sudo revdep-rebuild'
# shellcheck disable=SC2142
alias edsl='printf "$(hostname):%d,%d\n%s,%d" $(emanage -D local -u) $(emanage -D local -c) $(emanage -D remote -u) $(emanage -D remote -c | awk -F: "{print \$2}")'
alias epu='sudo -i epuse'
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
alias eup='sudo emerge --ask --update --deep --newuse @world'
alias ewdups='comm -12 <(sudo cat /etc/portage/sets/shared) <(sudo cat /var/lib/portage/world) 2> /dev/null'
alias get='sudo -i get'
alias pg='equery list "*" | grep -i'
rc-restart() { sudo /sbin/rc-service "$1" restart; };  compdef rc-restart=rc-service
alias rc-service='sudo /sbin/rc-service'
rc-start() { sudo /sbin/rc-service "$1" start; };  compdef rc-start=rc-service
rc-stop() { sudo /sbin/rc-service "$1" stop; };  compdef rc-stop=rc-service
alias rc-update='sudo /sbin/rc-update'
alias reboot='epcsync -q && sudo /sbin/reboot'
alias rstwifi='sudo rc-service net.wlo1 restart; sudo dhcpcd'
alias shutdown='epcsync -q && sudo shutdown -h now'
vblog() { vim "$(sudo find /var/tmp/portage -type f -regex ".*$1.*/build\.log")"; }
alias velog='vim /var/log/emerge.log'
alias vgentoo='sudo vim /etc/portage/make.{conf,shared} /etc/portage/package.{accept_keywords,mask,unmask,use}'
alias vj='sudo vim + /var/log/syslog*'
alias vmk='sudo vim /etc/portage/make.conf /etc/portage/make.shared'
alias vp='sudo vim /etc/portage/package.{accept_keywords,use,env,mask,unmask}'
alias vpe='sudo vim /etc/portage/package.env /etc/portage/env/*.conf'
alias vpk='sudo vim /etc/portage/package.accept_keywords'
alias vpm='sudo vim /etc/portage/package.mask'
alias vpu='sudo vim /etc/portage/package.use'
alias vw='sudo vim /etc/portage/sets/shared /etc/portage/sets/* /home/bryan/Dropbox/notes/untracked.txt /var/lib/portage/{world,world_sets}'

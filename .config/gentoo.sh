# ---------- Gentoo Aliases / Functions ----------
# def marker: GENTOO
alias del='sudo_del'
Ebi() { eposync "$1" && sudo -i /usr/local/bin/get "$1"; }
Epatch() { cd "$(epatch "$@")" || return 1; }
ebi() { sudo repoman manifest && sudo ebuild "$1" clean merge; }
ecd () { cd "$(find /home/bryan/projects/portage-overlay -type d -name "*$1*" | head -n 1)" || return 1; }
Ecd () { cd "$(find /var/db/pkg -type d -name "*$1*" | head -n 1)" || return 1; }
egcd () { cd "$(find "$(portageq get_repo_path / gentoo)" -type d -name "*$1*" | head -n 1)" || return 1; }
alias edc='sudo -E dispatch-conf'
alias edep='sudo emerge --ask --depclean && sudo revdep-rebuild'
# shellcheck disable=SC2142
alias epu='sudo epuse'
alias epum='sudo -E vim /etc/portage/package.unmask'
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
alias get='sudo_get'
alias pg='equery list "*" | grep -i'
pwg() { ag "$@" /etc/portage/sets; }
alias rc-service='sudo /sbin/rc-service'
alias rc-update='sudo /sbin/rc-update'
alias reboot='epcsync -q && sudo /sbin/reboot'
alias rstwifi='sudo rc-service net.wlo1 restart; sudo dhcpcd'
alias shutdown='epcsync -q && sudo shutdown -h now'
vblog() { vim "$(sudo find /var/tmp/portage -type f -regex ".*$1.*/build\.log")"; }
alias velog='vim /var/log/emerge.log'
alias vg='vgp /etc/portage/make.{conf,shared}'
alias vgmanage='vim ~/Dropbox/bin/gentoo/{emanage,eupdate,ecleanup,emaint_check}'
alias vgmk='sudo -E vim /etc/portage/make.conf /etc/portage/make.shared'
vgp() { sudo -E vim "$@" /etc/portage/package.{accept_keywords,use,env,mask,unmask} /etc/portage/make.{conf,shared} /etc/portage/env/*; }
alias vgpe='sudo -E vim /etc/portage/package.env /etc/portage/env/*.conf'
alias vgpk='sudo -E vim /etc/portage/package.accept_keywords'
alias vgpm='sudo -E vim /etc/portage/package.mask'
alias vgpu='sudo -E vim /etc/portage/package.use'
alias vgw='sudo -E vim /etc/portage/sets/shared /etc/portage/sets/* /home/bryan/Dropbox/var/notes/untracked.txt /var/lib/portage/{world,world_sets}'
alias vj='sudo -E vim + /var/log/syslog*'
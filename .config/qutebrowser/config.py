"""Qutebrowser Configuration"""

import functools
import re

import yaml

import searchengines as SE

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

# Load autoconfig.yml
config.load_autoconfig()


def bang_pttrn():
    """Returns regex pattern that matches DuckDuckGo bangs that I like to use."""
    one_letter_bangs = ['a', 'd', 'g', 'm', 't', 'w', ]
    two_letter_bangs = ['gm', 'ho', 'wa', 'yt', ]
    long_bangs = ['gt[A-z][A-z]+', 'ddg', 'bang', 'giphy', ]

    all_bangs = one_letter_bangs + two_letter_bangs + long_bangs

    bang_fmt = '^({}) '
    return bang_fmt.format('|'.join(all_bangs))


####################
#  Search Engines  #
####################
c.url.searchengines = {
    '2': 'https://www.google.com/maps/dir/417+Cripps+Dr,+Mt+Holly,+NJ+08060/{}',
    'A': 'https://www.amazon.com/gp/your-account/order-history/search?&search={}',
    'al': SE.static.google('arch linux {}'),
    'b': SE.static.stackoverflow(10, prefix='Bash'),
    'bb': SE.static.google('bloodborne {}'),
    'bmo': SE.SearchEngine(SE.static.google('best movies of 20{}'),
                           SE.OneIntURL(SE.static.google('best {1} movies of 20{0}'))),
    'c': SE.static.stackoverflow(7, prefix='C'),
    'cc': SE.static.stackoverflow(5, prefix='C\\+\\+'),
    'DEFAULT': SE.SearchEngine(SE.static.google('{}'),
                               SE.URL(SE.static.duckduckgo('{}'), '^!'),
                               SE.URL(SE.static.duckduckgo('!{}'), bang_pttrn()),
                               SE.LuckyURL('{}')),
    'de': SE.static.google('debian {}'),
    'ep': SE.SearchEngine(SE.static.google('{} episodes'),
                          SE.OneIntURL(SE.static.google('Season {0} {1} episodes'))),
    'ge': SE.static.google('gentoo {}'),
    'geb': 'https://bugs.gentoo.org/buglist.cgi?bug_status=__open__&content={}&list_id=4089892&order=Importance&query_format=specific',
    'gep': SE.SearchEngine(SE.static.site('packages.gentoo.org', 'gpo.zugaina.org'),
                           SE.LuckyURL('{} site:packages.gentoo.org')),
    'gh': SE.SearchEngine(SE.static.site('github.com'),
                          SE.LuckyURL('{} site:github.com'),
                          SE.URL('https://github.com/bbugyi200/{}',
                                 '^@',
                                 lambda x: x.replace(SE.utils.encode('@'), ''))),
    'ghi': SE.SearchEngine('https://github.com/bbugyi200/{}/issues',
                           SE.URL('https://github.com/bbugyi200/scripts/issues/{}', '^[0-9]+$'),
                           SE.OneIntURL('https://github.com/bbugyi200/{1}/issues/{0}'),
                           SE.LuckyURL('{0} site:github.com',
                                       '{}{}'.format(SE.LuckyURL.pattern, r'([A-z]| )+@'),
                                       lambda x: re.split(SE.utils.encode(' @'), SE.LuckyURL.filter(x), maxsplit=1),
                                       suffix='issues?&q=is%3Aissue+{1}'),
                           SE.LuckyURL('{0} site:github.com',
                                       '{}{}'.format(SE.LuckyURL.pattern, '([A-z]| )+#'),
                                       lambda x: re.split(SE.utils.encode(' #'), SE.LuckyURL.filter(x), maxsplit=1),
                                       suffix='issues/{1}'),
                           SE.LuckyURL('{} site:github.com', suffix='issues')),
    'gi': SE.static.stackoverflow(7, prefix='git'),
    'i': 'https://www.google.com/search?&tbm=isch&q={}',
    'j': 'https://www.google.com/search?q={}&ibp=htl;jobs#fpstate=tldetail',
    'js': 'https://www.google.com/search?q=Software+Engineer+{}&ibp=htl;jobs#fpstate=tldetail',
    'l': SE.static.stackoverflow(7, prefix='Linux'),
    'li': SE.SearchEngine(SE.static.site('linkedin.com'),
                          SE.URL(SE.static.site('linkedin.com', prefix='software'),
                                 '^@',
                                 lambda x: x.replace(SE.utils.encode('@'), ''))),
    'lib': 'http://libgen.io/search.php?req={}',
    'ma': SE.static.site('math.stackexchange.com', 'tex.stackexchange.com'),
    'p': SE.static.stackoverflow(7, prefix='Python'),
    'ps4': 'https://store.playstation.com/en-us/search/{}',
    'py': 'https://docs.python.org/3.6/library/{}',
    'r': SE.static.site('reddit.com'),
    'rl': SE.static.google('rocket league {}'),
    's0': SE.static.site('stackoverflow.com'),
    'st': SE.static.google('set timer for {}'),
    'sub': SE.SearchEngine(SE.static.google('{} inurl:english site:subscene.com'),
                           SE.LuckyURL('{0} inurl:english site:subscene.com'),
                           SE.LuckyURL('{2} S{0:02d}E{1:02d} inurl:english site:subscene.com',
                                       SE.TwoIntURL.pattern,
                                       SE.TwoIntURL.filter)),
    'T': SE.SearchEngine('https://1337x.unblocked.vet/search/{}/1/',
                         SE.TwoIntURL('https://1337x.unblocked.vet/search/{2} S{0:02d}E{1:02d}/1/')),
    'Tpb': SE.SearchEngine('https://thepiratebay3.com/search/{}',
                           SE.TwoIntURL('https://thepiratebay.org/search/{2} S{0:02d}E{1:02d}')),
    'ud': SE.static.site('idioms.thefreedictionary.com', 'en.wiktionary.org', 'urbandictionary.com'),
    'ytrl': 'https://www.youtube.com/results?search_query=rocket+league+{}',
    'ytt': 'https://www.youtube.com/results?search_query={}+Trailer'
}

for i in range(1, 11):
    c.url.searchengines['s{}'.format(i)] = SE.static.stackoverflow(i)


#############
#  Aliases  #
#############
aliases = {
    'G': 'spawn --userscript gmail',
    'get': 'jseval -q document.querySelector("h2").click()',  # click GET on libgen
    'lic': 'spawn --userscript linkedin_connect',
    'P': "spawn -v pockyt-add {url}",
    'rss': 'spawn --userscript openfeeds',
    'vs': 'open -w',
    'wt': 'spawn wtitle',
    'wtitle': 'spawn wtitle',
}

for k, v in aliases.items():
    c.aliases[k] = v


##############
#  Bindings  #
##############
########## Unbinds
unbound_nkeys = ['ad', 'b', 'B', 'co', 'd', 'D', 'gd', 'M', ]
for keys in unbound_nkeys:
    config.unbind(keys)

unbound_ikeys = ['<Ctrl-e>', ]
for keys in unbound_ikeys:
    config.unbind(keys, mode='insert')


########## Binds
def bind(keys, *commands, mode='normal'):
    config.bind(keys, ' ;; '.join(commands), mode=mode)


# bind functions for different modes
cbind = functools.partial(bind, mode='command')
ibind = functools.partial(bind, mode='insert')
pbind = functools.partial(bind, mode='prompt')
ptbind = functools.partial(bind, mode='passthrough')


# >>> INSERT
ibind('<Ctrl-f>', 'open-editor')
ibind('<Ctrl-i>', 'spawn -d qute-pass-add {url}')
ibind('<Alt-i>', 'spawn --userscript qute-pass')
ibind('<Ctrl-n>', 'fake-key -g <Down>')
ibind('<Alt-p>', 'spawn --userscript qute-pass --password-only')
ibind('<Ctrl-p>', 'fake-key -g <Up>')
ibind('<Alt-u>', 'spawn --userscript qute-pass --username-only')

# >>> PROMPT
pbind('<Ctrl-o>', 'prompt-open-download rifle {}')

# >>> COMMAND
cbind('<Ctrl-f>', 'edit-command --run')
cbind('<Ctrl-y>', 'fake-key --global <Return>V$y')

# >>> PASSTHROUGH
ptbind('<Escape>', 'leave-mode')
ptbind('<Ctrl-]>', 'fake-key <Escape>')
ptbind('[', 'spawn xdotool key super+shift+Tab')
ptbind(']', 'spawn xdotool key super+Tab')
ptbind('<Alt-[>', 'fake-key [')
ptbind('<Alt-]>', 'fake-key ]')

# >>> NORMAL
# ----- Alphanumeric -----
bind('A', 'set-cmd-text -s :quickmark-load -t')
bind('a', 'set-cmd-text -s :quickmark-load')
bind(',b', 'set-cmd-text :bookmark-add {url} "')
bind('b', 'quickmark-save')
bind('B', 'bookmark-add --toggle')
bind('cd', 'download-cancel')
bind('C', 'tab-clone')
bind('D', 'download')
bind(',e', 'spawn --userscript searchbar-command')
bind(',g', 'spawn --userscript google')
bind(',G', 'spawn --userscript google -t')
bind('gi', 'hint inputs')
bind(',h', 'set-cmd-text -s :help')
bind(',H', 'set-cmd-text -s :help -t')
bind(',m', 'spawn --userscript view_in_umpv -d')
bind(';m', 'hint all spawn -v qb_umpv {hint-url}', 'message-info "Select video to load with umpv."')
bind(';M', 'hint all spawn -v qb_umpv --append {hint-url}', 'message-info "Select video to append to umpv playlist."')
bind('m', 'enter-mode set_mark')
bind(',p', 'set-cmd-text :spawn -v wkhtmltopdf {url} /home/bryan/Downloads/')
bind(';P', "hint links spawn -v pockyt put -f '{link}' -i {hint-url}")
bind('p', 'open -- {clipboard}')
bind('P', 'open -t -- {clipboard}')
bind(',q', 'set-cmd-text :', 'run-with-count 2 command-history-prev', 'edit-command --run')
bind(',sd', 'set-cmd-text -s :session-delete')
bind(',sl', 'set-cmd-text -s :session-load -c')
bind(',ss', 'set-cmd-text -s :session-save -o')
bind(',S', 'session-save -c')
bind(',t', 'config-cycle tabs.position left top')
bind(';Tm', 'hint links spawn -d -v torrent {hint-url} -w /media/bryan/hercules/media/Entertainment/Movies', 'message-info "Select movie to torrent."')
bind(';TM', 'hint links spawn --userscript add-to-torrent-file movies.txt "{hint-url}"', 'message-info "Select movie to add to torrent list."')
bind(';Tt', 'hint links spawn -d -v torrent {hint-url} -w /media/bryan/hercules/media/Entertainment/TV', 'message-info "Select TV show to torrent."')
bind(';TT', 'hint links spawn --userscript add-to-torrent-file tv.txt "{hint-url}"', 'message-info "Select TV show to add to torrent list."')
bind('t-', 'tab-only')
bind('tt', 'set-cmd-text -s :tab-take')
bind('tg', 'set-cmd-text -s :tab-give')
bind('w-', 'window-only')
bind('x', 'tab-close')
bind('X', 'tab-close -o')
bind(',Y', 'spawn ytcast {url}', 'message-info "Casting YouTube to chromecast..."')
bind(';Y', 'hint links spawn -v ytcast {hint-url}', 'message-info "Casting YouTube to chromecast..."')
bind('Y', 'fake-key --global v$y')
# ----- Non-Alphanumeric -----
bind('\\', 'set-cmd-text :open /')
bind('|', 'set-cmd-text :open -t /')
bind('[', 'spawn xdotool key super+shift+Tab')
bind(']', 'spawn xdotool key super+Tab')
bind('(', 'navigate prev')
bind(')', 'navigate next')
bind('}', 'navigate next -t')
bind('{', 'navigate prev -t')
# ----- Miscellaneous -----
bind('<Alt-i>', 'enter-mode insert', 'spawn --userscript qute-pass')
bind('<Alt-p>', 'tab-pin')
bind('<Ctrl-p>', 'print')
bind('<Ctrl-l>', 'edit-url')
bind('<Ctrl-r>', 'restart')
bind('<Ctrl-y>', 'fake-key --global v$y')
bind('<Escape>', 'search', 'clear-messages')


######################
#  Load yaml Config  #
######################
with (config.configdir / 'config.yml').open() as f:
    yaml_data = yaml.load(f)


def dict_attrs(obj, path=''):
    if isinstance(obj, dict):
        for k, v in obj.items():
            yield from dict_attrs(v, '{}.{}'.format(path, k) if path else k)
    else:
        yield path, obj


for k, v in dict_attrs(yaml_data):
    config.set(k, v)

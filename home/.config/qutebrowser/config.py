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


####################
#  Search Engines  #
####################
def bang_pttrn():
    """Returns regex pattern that matches DuckDuckGo bangs that I like to use."""
    one_letter_bangs = ['a', 'd', 'g', 'm', 't', 'w', ]
    two_letter_bangs = ['gm', 'wa', 'yt', ]
    long_bangs = ['gt[A-z][A-z]+', 'ddg', 'bang', 'giphy', ]
    included_bangs = one_letter_bangs + two_letter_bangs + long_bangs

    bang_fmt = '^({}) '
    return bang_fmt.format('|'.join(included_bangs))


c.url.searchengines = {
    '2': 'https://www.google.com/maps/dir/417+Cripps+Dr,+Mt+Holly,+NJ+08060/{}',
    'A': 'https://www.amazon.com/gp/your-account/order-history/search?&search={}',
    'al': SE.static.google('arch linux {}'),
    'b': SE.static.stackoverflow(7, prefix='Bash'),
    'bmo': SE.SearchEngine(SE.static.google('best movies of 20{}'),
                           SE.OneIntURL(SE.static.google('best {1} movies of 20{0}'))),
    'cc': SE.static.stackoverflow(5, prefix='C++'),
    'DEFAULT': SE.SearchEngine(SE.static.google('{}'),
                               SE.URL(SE.static.duckduckgo('{}'), '^!'),
                               SE.URL(SE.static.duckduckgo('!{}'), bang_pttrn()),
                               SE.LuckyURL('{}')),
    'ep': SE.SearchEngine(SE.static.google('{} episodes'),
                          SE.OneIntURL(SE.static.google('Season {0} {1} episodes'))),
    'ge': SE.static.google('gentoo {}'),
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
    'l': SE.static.stackoverflow(7, prefix='Linux'),
    'li': SE.SearchEngine(SE.static.site('linkedin.com'),
                          SE.URL(SE.static.site('linkedin.com', prefix='software'),
                                 '^@',
                                 lambda x: x.replace(SE.utils.encode('@'), ''))),
    'lib': 'http://libgen.io/search.php?req={}',
    'ma': SE.static.site('math.stackexchange.com', 'tex.stackexchange.com'),
    'p': SE.static.stackoverflow(7, prefix='Python'),
    'py': 'https://docs.python.org/3.6/library/{}',
    'r': SE.static.site('reddit.com'),
    'so': SE.static.site('stackoverflow.com'),
    'sub': SE.SearchEngine(SE.static.google('{} inurl:english site:subscene.com'),
                           SE.LuckyURL('{0} inurl:english site:subscene.com'),
                           SE.LuckyURL('{2} S{0:02d}E{1:02d} inurl:english site:subscene.com',
                                       SE.TwoIntURL.pattern,
                                       SE.TwoIntURL.filter)),
    'tpb': SE.SearchEngine('https://thepiratebay.org/search/{}',
                           SE.TwoIntURL('https://thepiratebay.org/search/{2} S{0:02d}E{1:02d}')),
    'tub': SE.SearchEngine('https://1337x.unblocked.vet/search/{}/1/',
                           SE.TwoIntURL('https://1337x.unblocked.vet/search/{2} S{0:02d}E{1:02d}/1/')),
    'ud': SE.static.site('idioms.thefreedictionary.com', 'en.wiktionary.org', 'urbandictionary.com'),
    'ytt': 'https://www.youtube.com/results?search_query={}+Trailer'
}

for i in range(1, 10):
    c.url.searchengines['s{}'.format(i)] = SE.static.stackoverflow(i)


#############
#  Aliases  #
#############
c.aliases['libget'] = 'jseval -q document.querySelector("h2").click()'  # click GET on libgen
c.aliases['vs'] = 'open -w'


##############
#  Bindings  #
##############
def unbind(keys, mode='normal'):
    config.unbind(keys, mode=mode)


def bind(keys, *commands, mode='normal'):
    config.bind(keys, ' ;; '.join(commands), mode=mode)


# bind functions for different modes
ibind = functools.partial(bind, mode='insert')
pbind = functools.partial(bind, mode='prompt')
cbind = functools.partial(bind, mode='command')


########## Unbinds
unbound_nkeys = ['ad', 'b', 'B', 'co', 'd', 'D', 'gd', 'M', ]
for keys in unbound_nkeys:
    unbind(keys)

unbound_ikeys = ['<Ctrl-e>', ]
for keys in unbound_ikeys:
    unbind(keys, mode='insert')

########## Binds
# >>> INSERT
ibind('<Ctrl-f>', 'open-editor')
ibind('<Ctrl-i>', 'spawn -d qute-pass-add {url}')
ibind('<Ctrl-p>', 'spawn --userscript qute-pass')
ibind('<Ctrl-Shift-u>', 'spawn --userscript qute-pass --username-only')
ibind('<Ctrl-Shift-p>', 'spawn --userscript qute-pass --password-only')

# >>> PROMPT
pbind('<Ctrl-o>', 'prompt-open-download xdg-open {}')

# >>> COMMAND
cbind('<Ctrl-f>', 'edit-command --run')

# >>> NORMAL
bind(',1', 'buffer 1')
bind(',2', 'buffer 2')
bind(',3', 'buffer 3')
bind(',4', 'buffer 4')
bind(',5', 'buffer 5')
bind(',6', 'buffer 6')
bind(',7', 'buffer 7')
bind(',8', 'buffer 8')
bind(',9', 'buffer 9')
bind(',b', 'set-cmd-text :bookmark-add {url} "')
bind(',dp', 'spawn -v pockyt-delete {url}')
bind(',D', 'set-cmd-text -s :session-delete')
bind(',e', 'spawn --userscript searchbar-command')
bind(',h', 'set-cmd-text -s :help')
bind(',H', 'set-cmd-text -s :help -t')
bind(',L', 'set-cmd-text -s :session-load')
bind(',m', 'spawn --userscript view_in_umpv -d')
bind(',p', 'open -p')
bind(',P', 'set-cmd-text :spawn -v wkhtmltopdf {url} /home/bryan/Downloads/')
bind(',q', 'set-cmd-text :', 'run-with-count 2 command-history-prev', 'edit-command --run')
bind(',rss', 'spawn --userscript openfeeds')
bind(',sp', "spawn -v pockyt put -f '{link}' -i {url}")
bind(',S', 'set-cmd-text -s :session-save -o')
bind(',t', 'config-cycle tabs.position left top')
bind(',y', 'fake-key --global v$y')
bind(';m', 'hint links spawn -v umpv {hint-url}', 'message-info "Select video to load with umpv."')
bind(';M', 'hint links spawn -v umpv --append {hint-url}', 'message-info "Select video to append to umpv playlist."')
bind(';P', "hint links spawn -v pockyt put -f '{link}' -i {hint-url}")
bind(';T', 'hint links spawn -v torrent -d {hint-url}', 'message-info "Select magnet link to torrent."')
bind(';Y', 'hint links spawn -v ytcast {hint-url}', 'message-info "Casting YouTube to chromecast..."')
bind('<Ctrl-l>', 'edit-url')
bind('<Ctrl-r>', 'restart')
bind('<Ctrl-t>', 'spawn --userscript taskadd tags:inbox')
bind('a', 'set-cmd-text -s :quickmark-load')
bind('A', 'set-cmd-text -s :quickmark-load -t')
bind('b', 'quickmark-save')
bind('B', 'bookmark-add --toggle')
bind('cd', 'download-cancel')
bind('C', 'tab-clone')
bind('D', 'download')
bind('gi', 'hint inputs')
bind('m', 'enter-mode set_mark')
bind('p', 'open -- {clipboard}')
bind('P', 'open -t -- {clipboard}')
bind('t-', 'tab-only')
bind('tt', 'set-cmd-text -s :tab-take')
bind('tg', 'set-cmd-text -s :tab-give')
bind('x', 'tab-close')
bind('X', 'tab-close -o')
bind('w-', 'window-only')
bind('Y', 'spawn ytcast {url}', 'message-info "Casting YouTube to chromecast..."')

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

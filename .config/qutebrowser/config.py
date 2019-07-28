"""Qutebrowser Configuration"""

import functools
import platform
import os  # noqa
import sys  # noqa
import re
from typing import Any, Callable, Container, Dict, Generator, Iterable, Iterator, List, NoReturn, Optional, Sequence, Set, Tuple, Union  # noqa

import yaml

import searchengines as SE
import searchengines.utils as utils


os.environ['PATH'] = '{0}/.local/bin:/usr/local/bin:{1}'.format(os.environ['HOME'], os.environ['PATH'])

is_macos = False
if 'Darwin' in platform.version():
    is_macos = True


# pylint: disable=C0111
c = c  # type: ignore  # noqa: F821 pylint: disable=E0602,C0103
config = config  # type: ignore  # noqa: F821 pylint: disable=E0602,C0103

# Load autoconfig.yml
config.load_autoconfig()


######################
#  Search Aliases    #
######################
# These aliases will be substituted with their definitions when found
# anywhere in the query of an ':open' command.
search_aliases = {
    'al': 'Arch Linux',
    'b': 'Bash',
    'bb': 'Bloodborne',
    'bbt': 'Big Bang Theory',
    'bnn': 'Brooklyn Nine-Nine',
    'cl': 'command-line',
    'co': 'Compilers',
    'cod': 'Call of Duty: Black Ops 4',
    'cs': 'Computer Science',
    'de': 'Debian',
    'dep': 'Debian Buster',
    'dj': 'Django',
    'en': 'Evernote',
    'fcl': 'from the command-line',
    'ge': 'Gentoo',
    'gh': 'GitHub',
    'gl': 'GitLab',
    'gt': 'Georgia Tech',
    'gtest': '"Google Test" OR gtest',
    'ha': 'Haskell',
    'hzd': 'Horizon: Zero Dawn',
    'js': 'JavaScript',
    'ks': 'keyboard shortcuts',
    'lcl': 'Linux from the command-line',
    'lta': '"Life Time Athletic"',
    'lx': 'Linux',
    'mac': 'MacOS',
    'mf': 'Modern Family',
    'ml': 'Machine Learning',
    'ms': "master's degree",
    'n': 'AND',
    'nn': 'nixnote',
    'o': 'OR',
    'oms': "online master's degree",
    'py': 'Python',
    'qb': 'qutebrowser',
    'rdr': 'Red Dead Redemption 2',
    'rnm': 'Rick and Morty',
    'rl': 'Rocket League',
    'ru': 'Rutgers',
    'sal': 'average salary',
    'sd': 'San Diego',
    'se': 'Software Engineer',
    'sel': 'Selenium Python',
    'sg': 'Samsung Galaxy',
    'sgw': 'Samsung Galaxy Watch',
    'ta': 'tasker',
    'tb': 'Thunderbird',
    'tex': 'LaTeX',
    'tlou': 'The Last of Us',
    'v': 'vim',
    'ys': 'Young Sheldon',
}

# Google's AROUND(N) Search Operator
for i in range(1, 51):
    search_aliases['a{}'.format(i)] = 'AROUND({})'.format(i)

# Set the utils module's search alias dictionary.
utils.search_aliases = search_aliases


####################
#  Search Engines  #
####################
def bang_pttrn() -> str:
    """Returns regex pattern that matches DuckDuckGo bangs that I like to use."""
    one_letter_bangs = ['a', 'd', 'g', 'm', 't', ]
    two_letter_bangs = ['gm', 'ho', 'wa', 'yt', ]
    long_bangs = ['ddg', 'bang', 'giphy', ]

    all_bangs = one_letter_bangs + two_letter_bangs + long_bangs

    bang_fmt = '^({}) '
    return bang_fmt.format('|'.join(all_bangs))


c.url.searchengines = {
    '2': 'https://www.google.com/maps/dir/417+Cripps+Dr,+Mt+Holly,+NJ+08060/{}',
    '3': 'https://www.google.com/maps/dir/902+Carnegie+Center,+Princeton,+NJ+08540/{}',
    'A': 'https://www.amazon.com/gp/your-account/order-history/search?&search={}',
    'b': SE.static.stackoverflow(10, prefix='Bash'),
    'bmo': SE.SearchEngine(SE.static.google('best movies of 20{}'),
                           SE.OneIntURL(SE.static.google('best {1} movies of 20{0}'))),
    'c': SE.static.stackoverflow(7, prefix='C'),
    'cc': SE.static.stackoverflow(5, prefix='C\\+\\+'),
    'DEFAULT': SE.SearchEngine(SE.static.google('{}'),
                               SE.URL(SE.static.duckduckgo('{}'), '^!'),
                               SE.URL(SE.static.duckduckgo('!{}'), bang_pttrn()),
                               SE.LuckyURL('{}')),
    'dvd': SE.static.google('{} DVD release date'),
    'ei': SE.SearchEngine('https://gitlab.pr.edgelp.net/edgelp/prod/issues/{}'),
    'emp': 'https://gitlab.pr.edgelp.net/edgelp/prod/merge_requests/{}',
    'emw': 'https://gitlab.pr.edgelp.net/edgelp/website/merge_requests/{}',
    'ep': SE.SearchEngine(SE.static.google('{} episodes'),
                          SE.OneIntURL(SE.static.google('Season {0} {1} episodes'))),
    'ew': 'https://www.edgestreamlp.com/{}',
    'ews': 'https://edgestream-staging.herokuapp.com/{}',
    'g4g': SE.static.site('www.geeksforgeeks.org'),
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
                                       lambda x: re.split(SE.utils.encode(' @'),
                                                          SE.LuckyURL.filter(x),
                                                          maxsplit=1),
                                       suffix='issues?&q=is%3Aissue+{1}'),
                           SE.LuckyURL('{0} site:github.com',
                                       '{}{}'.format(SE.LuckyURL.pattern, '([A-z]| )+#'),
                                       lambda x: re.split(SE.utils.encode(' #'),
                                                          SE.LuckyURL.filter(x),
                                                          maxsplit=1),
                                       suffix='issues/{1}'),
                           SE.LuckyURL('{} site:github.com', suffix='issues')),
    'ght': 'https://github.com/bbugyi200/{}/graphs/traffic',
    'i': SE.SearchEngine('https://www.google.com/search?&tbm=isch&q={}'),
    'j': 'https://www.google.com/search?q={}&ibp=htl;jobs#fpstate=tldetail',
    'l': SE.static.stackoverflow(7, prefix='Linux'),
    'lh': 'http://127.0.0.1:8000/{}',
    'li': SE.SearchEngine(SE.static.site('linkedin.com'),
                          SE.URL(SE.static.site('linkedin.com', prefix='software'),
                                 '^@',
                                 lambda x: x.replace(SE.utils.encode('@'), ''))),
    'lib': 'http://libgen.is/search.php?req={}',
    'Lib': 'https://libgen.me/search?q={}',
    'ma': SE.static.site('math.stackexchange.com', 'tex.stackexchange.com'),
    'p': SE.static.stackoverflow(7, prefix='Python'),
    'pyl': 'https://docs.python.org/3/library/{}',
    'pss': 'https://store.playstation.com/en-us/search/{}',
    'r': SE.static.site('reddit.com'),
    'rlp': 'https://rocketleague.tracker.network/profile/ps/{}',
    'rpy': 'https://realpython.com/search?q={}',
    's0': SE.static.site('stackoverflow.com'),
    'shr': 'https://shop.shoprite.com/store/1627666/search?displayType=&query={}&recipe=0&sponsored=5',
    'st': SE.static.google('set timer for {}'),
    'sub': SE.SearchEngine(SE.static.google('{} inurl:english site:subscene.com'),
                           SE.LuckyURL('{0} inurl:english site:subscene.com'),
                           SE.LuckyURL('{2} S{0:02d}E{1:02d} inurl:english site:subscene.com',
                                       SE.TwoIntURL.pattern,
                                       SE.TwoIntURL.filter)),
    'T': SE.SearchEngine('https://1337x.unblocked.vet/search/{}/1/',
                         SE.TwoIntURL('https://1337x.unblocked.vet/search/{2} S{0:02d}E{1:02d}/1/'),
                         SE.OneIntURL('https://1337x.unblocked.vet/search/{1} Season/1/')),
    'TT': SE.SearchEngine('https://thepiratebay3.com/search/{}',
                         SE.TwoIntURL('https://thepiratebay.org/search/{2} S{0:02d}E{1:02d}'),
                         SE.OneIntURL('https://thepiratebay.org/search/{1} Season')),
    'ud': SE.static.site('idioms.thefreedictionary.com', 'en.wiktionary.org', 'urbandictionary.com'),
    'q': SE.static.google('"{}"'),
    'w': SE.static.site('en.wikipedia.org'),
    'W': SE.static.google('weather in {}'),
    'ytt': 'https://www.youtube.com/results?search_query={}+Trailer'
}

for i in range(1, 11):
    c.url.searchengines[f's{i}'] = SE.static.stackoverflow(i)
    c.url.searchengines[f'g{i}'] = SE.static.google('{}', max_years_old=i)


######################
#  Commande Aliases  #
######################
command_aliases = {
    'get': 'jseval -q document.querySelector("h2").click()',  # click GET on libgen
    'lic': 'spawn --userscript linkedin_connect',
    'mkpdf': 'set-cmd-text :spawn -v wkhtmltopdf {url} /home/bryan/Downloads/',
    'P': "spawn -v pockyt-add {url}",
    'rss': 'spawn --userscript openfeeds',
    'Tsub': 'spawn --userscript Tsub',
    'vs': 'open -w',
    'wt': 'spawn wtitle',
}

for i in range(1, 21):
    command_aliases['b{}'.format(i)] = 'buffer {}'.format(i)

for k, v in command_aliases.items():
    c.aliases[k] = v


##############
#  Bindings  #
##############
c.bindings.commands = {}  # Clears all previously set user bindings.

########## Unbinds
unbound_nkeys: List[str] = ['<Ctrl+h>', 'ad', 'b', 'B', 'co', 'd', 'D', 'gd', 'gf', 'gl', 'gr', 'M', ]
unbound_ikeys: List[str] = ['<Ctrl+e>']

for unbound_keys, mode in [(unbound_nkeys, 'normal'), (unbound_ikeys, 'insert')]:
    for keys in unbound_keys:
        config.unbind(keys, mode=mode)


########## Binds
def bind(keys: str, *commands: str, mode: str = 'normal') -> None:
    config.bind(keys, ' ;; '.join(commands), mode=mode)


# bind functions for different modes
cbind = functools.partial(bind, mode='command')
ibind = functools.partial(bind, mode='insert')
pbind = functools.partial(bind, mode='prompt')
ptbind = functools.partial(bind, mode='passthrough')


# >>>>>>> COMMAND
cbind("<Alt-j>", 'spawn --userscript add_quotes 1')
cbind("<Alt-k>", 'spawn --userscript add_quotes 2')
cbind("<Alt-l>", 'spawn --userscript add_quotes 3')
cbind("<Alt-u>", 'spawn --userscript add_quotes -1')
cbind("<Alt-i>", 'spawn --userscript add_quotes -2')
cbind("<Alt-o>", 'spawn --userscript add_quotes -3')
cbind('<Ctrl-f>', 'edit-command --run')
cbind('<Ctrl-y>', 'fake-key --global <Return>v$y')

# >>>>>>> INSERT
ibind('<Ctrl-f>', 'open-editor')
ibind('<Ctrl-i>', 'spawn -d qute-pass-add {url}')
ibind('<Alt-i>', 'spawn --userscript qute-pass')
ibind('<Ctrl-Shift-i>', 'spawn --userscript qute-pass')
ibind('<Ctrl-n>', 'fake-key -g <Down>')
ibind('<Alt-p>', 'spawn --userscript qute-pass --password-only')
ibind('<Ctrl-p>', 'fake-key -g <Up>')
ibind('<Alt-u>', 'spawn --userscript qute-pass --username-only')

# >>>>>>> PROMPT
pbind('<Ctrl-o>', 'prompt-open-download rifle {}')

# >>>>>>> PASSTHROUGH
ptbind('<Ctrl-]>', 'fake-key <Escape>')
ptbind('<Ctrl-x>', 'tab-close', 'enter-mode passthrough')

# >>>>>>> NORMAL
# -------------------
# ----- Numeric -----
# -------------------
bind('9', 'scroll-page 0 -0.5')
bind('0', 'scroll-page 0 0.5')
# ----------------------
# ----- Alphabetic -----
# ----------------------
bind('A', 'set-cmd-text -s :quickmark-load -t')
bind('a', 'set-cmd-text -s :quickmark-load')
bind(',b', 'set-cmd-text :bookmark-add {url} "')
bind('b', 'quickmark-save')
bind('B', 'bookmark-add --toggle')
bind('c', 'yank selection')
bind('C', 'tab-clone')
bind('dl', 'tab-close')
bind('D', 'download')
bind(',e', 'spawn --userscript searchbar-command')
bind('gc', 'spawn "{}" {{url}}'.format(
    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' if is_macos else 'google-chrome'
))
bind(',gc', 'spawn "init-chrome"')
bind('gh', 'home')
bind('gs', 'view-source')
bind('gi', 'hint inputs')
bind('gl', 'tab-focus last')
bind(',h', 'set-cmd-text -s :help')
bind(',H', 'set-cmd-text -s :help -t')
bind(',m', 'spawn --userscript view_in_umpv -d')
bind(';m', 'hint all spawn -v qb_umpv {hint-url}', 'message-info "Select video to load with umpv."')
bind(';M', 'hint all spawn -v qb_umpv --append {hint-url}', 'message-info "Select video to append to umpv playlist."')
bind('m', 'enter-mode set_mark')
bind(';P', "hint links spawn -v pockyt put -f '{link}' -i {hint-url}")
bind('p', 'open -- {clipboard}')
bind('P', 'open -t -- {clipboard}')
bind(',q', 'set-cmd-text :', 'run-with-count 2 command-history-prev', 'edit-command --run')
bind(',sd', 'set-cmd-text -s :session-delete')
bind(',sl', 'set-cmd-text -s :session-load -c')
bind(',ss', 'set-cmd-text -s :session-save -o')
bind(',S', 'session-save -c')
bind(',tt', 'set tabs.position top', 'set tabs.title.format "{audio}{index}: {title}"', 'set tabs.title.format_pinned "[{index}]"')
bind(',tl', 'set tabs.position left', 'set tabs.title.format " * {audio}{index}: {title}"', 'set tabs.title.format_pinned "[{index}]: {title}"')
bind(',tr', 'set tabs.position right', 'set tabs.title.format " * {audio}{index}: {title}"', 'set tabs.title.format_pinned "[{index}]: {title}"')
bind(';Tm', 'hint links spawn -d -v torrent -d {hint-url} -w /media/bryan/zeus/media/Entertainment/Movies', 'message-info "Select movie to torrent."')
bind(';TM', 'hint links spawn --userscript add-to-torrent-file movies.txt "{hint-url}"', 'message-info "Select movie to add to torrent list."')
bind(';Tt', 'hint links spawn -d -v torrent -d {hint-url} -w /media/bryan/zeus/media/Entertainment/TV', 'message-info "Select TV show to torrent."')
bind(';TT', 'hint links spawn --userscript add-to-torrent-file tv.txt "{hint-url}"', 'message-info "Select TV show to add to torrent list."')
bind('t-', 'tab-only')
bind('tt', 'set-cmd-text -s :tab-take')
bind('tg', 'set-cmd-text -s :tab-give')
bind('w-', 'window-only')
bind('x', 'tab-close -n')
bind('X', 'tab-close -p')
bind(',Y', 'spawn ytcast {url}', 'message-info "Casting YouTube to chromecast..."')
bind(';Y', 'hint links spawn -v ytcast {hint-url}', 'message-info "Casting YouTube to chromecast..."')
bind('Y', 'fake-key --global v$y')
# ----------------------------
# ----- Non-Alphanumeric -----
# ----------------------------
bind('=', 'zoom-in')
bind('\\', 'set-cmd-text :open /')
bind('|', 'set-cmd-text :open -t /')
bind('(', 'navigate prev')
bind(')', 'navigate next')
bind('{', 'navigate prev -t')
bind('}', 'navigate next -t')
bind('[', 'spawn winstack prev')
bind(']', 'spawn winstack next')
bind('>', 'tab-move +')
bind('<', 'tab-move -')
# -------------------------
# ----- Miscellaneous -----
# -------------------------
bind('<Alt-i>', 'enter-mode insert', 'spawn --userscript qute-pass')
bind('<Alt-p>', 'enter-mode insert', 'spawn --userscript qute-pass --password-only')
bind('<Alt-u>', 'enter-mode insert', 'spawn --userscript qute-pass --username-only')
bind('<Ctrl-p>', 'tab-pin')
bind('<Ctrl-l>', 'edit-url')
bind('<Ctrl-r>', 'restart')
bind('<Ctrl-y>', 'fake-key --global v$y')
bind('<Escape>', 'search', 'clear-messages')


######################
#  Load yaml Config  #
######################
with (config.configdir / 'config.yml').open() as f:
    yaml_data = yaml.load(f)


def dict_attrs(obj: str, path: str = '') -> Generator[Tuple[str, str], None, None]:
    if isinstance(obj, dict):
        for k, v in obj.items():
            yield from dict_attrs(v, '{}.{}'.format(path, k) if path else k)
    else:
        yield path, obj


for k, v in dict_attrs(yaml_data):
    config.set(k, v)

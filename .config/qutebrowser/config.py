"""Qutebrowser Configuration"""

import datetime as dt
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
def stackoverflow(n, *, prefix=None):
    """Returns stackoverflow HTTP encoded google search string.

    The search results returned by Google will range from @n years ago until now.

    Args:
        prefix (opt): If a string is provided for @prefix, it will be prepended to the final query.
    """
    if prefix is None:
        prefix = ''

    try:
        assert isinstance(prefix, str), "@prefix arguement must be a string."
    except AssertionError as e:
        raise ValueError(str(e))
    else:
        prefix = prefix + ' '

    fmt = '{0}{{}} site:stackoverflow.com&source=lnt&tbs=cdr%3A1%2Ccd_min%3A{1}%2F{2}%2F{3}%2Ccd_max%3A&tbm='
    D = _n_years_ago(n)
    return fmt.format(prefix, D.month, D.day, D.year)


def _n_years_ago(n):
    """Return a datetime N years ago."""
    today = dt.date.today()
    return today.replace(year=(today.year - n))


one_letter_bangs = ['a', 'd', 'g', 'i', 't', 'w']
two_letter_bangs = ['wa', 'yt']
long_bangs = ['gt[A-z][A-z]+', 'ddg', 'bang', 'giphy']
included_bangs = one_letter_bangs + two_letter_bangs + long_bangs

bang_fmt = '^({})%20'
bang_pttrn = bang_fmt.format('|'.join(included_bangs))


c.url.searchengines = {
    'A': 'https://www.amazon.com/gp/your-account/order-history/search?&search={}',
    'al': SE.static.google('arch linux {}'),
    'cc': SE.static.google(stackoverflow(7, prefix='C++')),
    'DEFAULT': SE.URL(SE.static.google('{}'),
                      SE.static.duckduckgo('{}'),
                      SE.static.duckduckgo('!{}'),
                      SE.LuckyQuery.url('{}'),
                      patterns=('^%21', bang_pttrn, SE.LuckyQuery.pattern),
                      filters=(None, None, SE.LuckyQuery.filter)),
    'ep': SE.URL(SE.static.google('{} episodes'),
                 SE.static.google('Season {} episodes'),
                 patterns=SE.OneIntQuery.pattern),
    'gh': SE.URL(SE.static.google('{} site:github.com'),
                 SE.LuckyQuery.url('{} site:github.com'),
                 'https://github.com/bbugyi200/{}',
                 patterns=(
                     SE.LuckyQuery.pattern,
                     '^%40',
                 ),
                 filters=(
                     SE.LuckyQuery.filter,
                     lambda x: x.replace('%40', ''),
                 )),
    'ghi': SE.URL('https://github.com/bbugyi200/{}/issues',
                  'https://github.com/bbugyi200/scripts/issues/{}',
                  'https://github.com/bbugyi200/{1}/issues/{0}',
                  SE.LuckyQuery.url('{} site:github.com', end='issues?&q=is%3Aissue+{}'),
                  SE.LuckyQuery.url('{} site:github.com', end='issues/{}'),
                  SE.LuckyQuery.url('{} site:github.com', end='issues'),
                  patterns=(
                      '^[0-9]+$',
                      SE.OneIntQuery.pattern,
                      '{}{}'.format(SE.LuckyQuery.pattern, r'([A-z]|%20)+%3F'),
                      '{}{}'.format(SE.LuckyQuery.pattern, r'([A-z]|%20)+%23'),
                      SE.LuckyQuery.pattern,
                  ),
                  filters=(
                      None,
                      SE.OneIntQuery.filter,
                      lambda x: re.split(r'%20%3F', SE.LuckyQuery.filter(x), maxsplit=1),
                      lambda x: re.split(r'%20%23', SE.LuckyQuery.filter(x), maxsplit=1),
                      SE.LuckyQuery.filter,
                  )),
    'li': SE.URL(SE.static.google('site:linkedin.com {}'),
                 SE.static.google('site:linkedin.com {} software'),
                 patterns='^%40',
                 filters=lambda x: x[3:]),
    'lib': 'http://libgen.io/search.php?req={}',
    'pir': SE.URL('https://thepiratebay.org/search/{}',
                  'https://thepiratebay.org/search/{2} S{0:02d}E{1:02d}',
                  patterns=SE.TwoIntQuery.pattern,
                  filters=SE.TwoIntQuery.filter),
    'p': SE.static.google(stackoverflow(7, prefix='Python')),
    'py': 'https://docs.python.org/3.6/library/{}',
    'r': SE.URL(SE.static.google('{} site:reddit.com'),
                SE.LuckyQuery.url('{} site:reddit.com'),
                patterns=SE.LuckyQuery.pattern,
                filters=SE.LuckyQuery.filter),
    'so': SE.static.google('{} site:stackoverflow.com'),
    'sub': SE.URL(SE.static.google('{} inurl:english site:subscene.com'),
                  SE.LuckyQuery.url('{0} inurl:english site:subscene.com'),
                  SE.LuckyQuery.url('{2} S{0:02d}E{1:02d} inurl:english site:subscene.com'),
                  patterns=(SE.LuckyQuery.pattern, SE.TwoIntQuery.pattern),
                  filters=(SE.LuckyQuery.filter, SE.TwoIntQuery.filter)),
}

##### Setting Up Stackoverflow Search Engines
for n in range(1, 11):
    c.url.searchengines['s{}'.format(n)] = SE.static.google(stackoverflow(n))

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
bind(',b', 'set-cmd-text :bookmark-add {url} "')
bind(',dp', 'spawn -v pockyt-delete {url}')
bind(',e', 'spawn --userscript searchbar-command')
bind(',h', 'set-cmd-text -s :help')
bind(',H', 'set-cmd-text -s :help -t')
bind(',m', 'spawn --userscript view_in_umpv -d')
bind(',p', 'open -p')
bind(',q', 'set-cmd-text :', 'run-with-count 2 command-history-prev', 'edit-command --run')
bind(',rss', 'spawn --userscript openfeeds')
bind(',ss', 'set-cmd-text -s :session-save -o')
bind(',sl', 'set-cmd-text -s :session-load -c')
bind(',sp', "spawn -v pockyt put -f '{link}' -i {url}")
bind(',t', 'config-cycle tabs.position left top')
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
bind('h', 'back')
bind('H', 'scroll left')
bind('l', 'forward')
bind('L', 'scroll right')
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

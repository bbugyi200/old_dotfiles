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
# construction of bang search pattern for 1-3 letter words and specified longer bangs
excluded_bangs = ['is', 'py']
included_bangs = ['gt[A-z][A-z]+', 'ddg', 'bang', 'giphy']
bang_fmt = '^({}[A-z][A-z]?|({}))%20'
bang_pttrn = bang_fmt.format(''.join(['(?!{})'.format(w) for w in excluded_bangs]),
                             '|'.join(included_bangs))

c.url.searchengines = {
    'A': 'https://www.amazon.com/gp/your-account/order-history/search?&search={}',
    'al': SE.static.google('arch linux {}'),
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
                 SE.LuckyQuery.url('{} site:github.com inurl:doc'),
                 SE.LuckyQuery.url('{} site:github.com'),
                 'https://github.com/bbugyi200/{}',
                 patterns=('{}%23'.format(SE.LuckyQuery.pattern),
                           SE.LuckyQuery.pattern,
                           '^%40'),
                 filters=(lambda x: SE.LuckyQuery.filter(x).replace('%23', ''),
                          SE.LuckyQuery.filter,
                          lambda x: x.replace('%40', ''))),
    'ghi': SE.URL('https://github.com/bbugyi200/{}/issues',
                  'https://github.com/bbugyi200/scripts/issues/{}',
                  'https://github.com/bbugyi200/{1}/issues/{0}',
                  SE.LuckyQuery.url('{} site:github.com', end='issues?&q=is%3Aissue+{}'),
                  SE.LuckyQuery.url('{} site:github.com', end='issues'),
                  patterns=('^[0-9]+$',
                            SE.OneIntQuery.pattern,
                            '{}{}'.format(SE.LuckyQuery.pattern, r'([A-z]|%20)+%3F'),
                            SE.LuckyQuery.pattern),
                  filters=(None,
                           SE.OneIntQuery.filter,
                           lambda x: re.split(r'%20%3F', SE.LuckyQuery.filter(x), maxsplit=1),
                           SE.LuckyQuery.filter)),
    'li': SE.static.google('site:linkedin.com {}'),
    'lib':'http://libgen.io/search.php?req={}',
    'pir': SE.URL('https://thepiratebay.org/search/{}',
                  'https://thepiratebay.org/search/{2} S{0:02d}E{1:02d}',
                  patterns=SE.TwoIntQuery.pattern,
                  filters=SE.TwoIntQuery.filter),
    'py': 'https://docs.python.org/3.6/library/{}',
    'r': SE.URL(SE.static.google('{} site:reddit.com'),
                SE.LuckyQuery.url('{} site:reddit.com'),
                patterns=SE.LuckyQuery.pattern,
                filters=SE.LuckyQuery.filter),
    'sub': SE.URL(SE.static.google('{} inurl:english site:subscene.com'),
                  SE.LuckyQuery.url('{0} inurl:english site:subscene.com'),
                  SE.LuckyQuery.url('{2} S{0:02d}E{1:02d} inurl:english site:subscene.com'),
                  patterns=(SE.LuckyQuery.pattern, SE.TwoIntQuery.pattern),
                  filters=(SE.LuckyQuery.filter, SE.TwoIntQuery.filter)),
    'waf':'https://waffle.io/bbugyi200/{}'
}

#############
#  Aliases  #
#############
c.aliases['mpv'] = 'spawn --userscript view_in_umpv {url}'


##############
#  Bindings  #
##############
def unbind(*args, **kwargs):
    config.unbind(*args, **kwargs)


def bind(key, *commands, mode='normal'):
    config.bind(key, ' ;; '.join(commands), mode=mode)


########## Unbinds
unbound_keys = ['b', 'B', 'd', 'D', 'gd', 'ad', 'co']
for key in unbound_keys:
    unbind(key)

########## Binds
# >>> INSERT
bind('<Ctrl-i>', 'spawn -d qute-pass-add {url}', mode='insert')
bind('<Ctrl-p>', 'spawn --userscript qute-pass', mode='insert')
bind('<Ctrl-Shift-u>', 'spawn --userscript qute-pass --username-only', mode='insert')
bind('<Ctrl-Shift-p>', 'spawn --userscript qute-pass --password-only', mode='insert')

# >>> PROMPT
bind('<Ctrl-o>', 'prompt-open-download xdg-open {}', mode='prompt')

# >>> COMMAND
bind('<Ctrl-f>', 'edit-command', mode='command')

# >>> NORMAL
bind(',e', 'spawn --userscript searchbar-command')
bind(',G', 'jseval -q document.querySelector("h2").click()')  # click GET on libgen
bind(',m', 'spawn --userscript view_in_umpv -d')
bind(',p', 'open -p')
bind(',q', 'set-cmd-text :', 'run-with-count 2 command-history-prev', 'edit-command --run')
bind(',rss', 'spawn --userscript openfeeds')
bind(',t', 'config-cycle tabs.position left top')
bind(';m', 'hint links spawn umpv {hint-url}', 'message-info "Select video to load with umpv."')
bind(';M', 'hint links spawn umpv --append {hint-url}', 'message-info "Select video to append to umpv playlist."')
bind(';T', 'hint links spawn torrent -d {hint-url}', 'message-info "Select magnet link to torrent."')
bind(';Y', 'hint links spawn ytcast {hint-url}', 'message-info "Casting YouTube to chromecast..."')
bind('<Ctrl-l>', 'edit-url')
bind('<Ctrl-r>', 'restart')
bind('<Ctrl-t>', 'spawn --userscript taskadd tags:inbox')
bind('a', ':set-cmd-text -s :quickmark-load')
bind('A', ':set-cmd-text -s :quickmark-load -t')
bind('b', 'set-cmd-text -s :buffer')
bind('cd', 'download-cancel')
bind('D', 'download')
bind('gi', 'hint inputs')
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

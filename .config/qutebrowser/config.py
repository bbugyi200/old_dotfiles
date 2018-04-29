import re
import yaml

import searchengines as SE

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

# Load autoconfig.yml
config.load_autoconfig()

# construction of bang search pattern for 1-3 letter words
two_letter_words = ['is']
three_letter_words = ['the', 'are', 'was', 'who', 'can', 'how', 'did']
bang_fmt = '^({}[A-z][A-z]?|{}[A-z]{{3}})%20'
bang_pttrn = bang_fmt.format(''.join(['(?!{})'.format(w) for w in two_letter_words]),
                             ''.join(['(?!{})'.format(w) for w in three_letter_words]))

# ----- Search Engines
c.url.searchengines['DEFAULT'] = SE.URL(SE.static.google('{}'),
                                        SE.static.duckduckgo('{}'),
                                        SE.static.duckduckgo('!{}'),
                                        SE.LuckyQuery.url('{}'),
                                        patterns=('^%21', bang_pttrn, SE.LuckyQuery.pattern),
                                        filters=(None, None, SE.LuckyQuery.filter))
c.url.searchengines['ep'] = SE.URL(SE.static.google('{} episodes'),
                                   SE.static.google('Season {} episodes'),
                                   patterns=SE.OneIntQuery.pattern)
c.url.searchengines['al'] = SE.static.google('arch linux {}')
c.url.searchengines['gh'] = SE.URL(SE.static.google('{} site:github.com'),
                                   SE.LuckyQuery.url('{} site:github.com'),
                                   'https://github.com/bbugyi200/{}',
                                   patterns=(SE.LuckyQuery.pattern, '^%40'),
                                   filters=(SE.LuckyQuery.filter, lambda x: x.replace('%40', '')))
c.url.searchengines['ghi'] = SE.URL('https://github.com/bbugyi200/{}/issues',
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
                                             SE.LuckyQuery.filter))
c.url.searchengines['li'] = SE.static.google('site:linkedin.com {}')
c.url.searchengines['red'] = SE.static.google('site:reddit.com {}')
c.url.searchengines['waf'] = 'https://waffle.io/bbugyi200/{}'
c.url.searchengines['lib'] = 'http://libgen.io/search.php?req={}'
c.url.searchengines['pir'] = SE.URL('https://thepiratebay.org/search/{}',
                                    'https://thepiratebay.org/search/{2} S{0:02d}E{1:02d}',
                                    patterns=SE.TwoIntQuery.pattern,
                                    filters=SE.TwoIntQuery.filter)
c.url.searchengines['sub'] = SE.URL(SE.static.google('{} inurl:english site:subscene.com'),
                                    SE.LuckyQuery.url('{0} inurl:english site:subscene.com'),
                                    SE.LuckyQuery.url('{2} S{0:02d}E{1:02d} inurl:english site:subscene.com'),
                                    patterns=(SE.LuckyQuery.pattern, SE.TwoIntQuery.pattern),
                                    filters=(SE.LuckyQuery.filter, SE.TwoIntQuery.filter))


# ----- Bindings
def bind(key, *commands, mode='normal'):
    config.bind(key, ' ;; '.join(commands), mode=mode)


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
# Ctrl
bind('<Ctrl-r>', 'restart')
bind('<Ctrl-t>', 'spawn --userscript taskadd tags:inbox')
bind('<Ctrl-l>', 'edit-url')
# Leader (,)
bind(',e', 'scroll-to-perc 0', 'later 25 hint inputs -m number',
     'later 50 spawn xdotool key 0', 'later 100 open-editor')
bind(',t', 'config-cycle tabs.position left top')
bind(',rss', 'spawn --userscript openfeeds')
# Miscellaneous
bind('gi', 'hint inputs')
bind('sb', 'quickmark-save')
bind('C', 'tab-clone', 'back')
bind('m', 'enter-mode set_mark')
bind('p', 'open -- {clipboard}')
bind('P', 'open -t -- {clipboard}')
bind('x', 'search')  # Clears search

# ----- Load Yaml Config
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

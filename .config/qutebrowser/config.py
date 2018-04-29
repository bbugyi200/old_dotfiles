import yaml

import searchengines as SE

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

# Load autoconfig.yml
config.load_autoconfig()

# ----- Dictionary Values
c.url.searchengines['DEFAULT'] = SE.URL(SE.google('{}'),
                                        SE.duckduckgo('{}'),
                                        SE.duckduckgo('!{}'),
                                        SE.LuckyQuery.url('{}'),
                                        patterns=('^%21', '^(?!is)[A-z][A-z]?%20', SE.LuckyQuery.pattern),
                                        filters=(None, None, SE.LuckyQuery.filter))
c.url.searchengines['ep'] = SE.URL(SE.google('{} episodes'),
                                   SE.google('Season {} episodes'),
                                   patterns=SE.OneIntQuery.pattern)
c.url.searchengines['d'] = SE.duckduckgo('{}')
c.url.searchengines['al'] = SE.google('arch linux {}')
c.url.searchengines['gh'] = SE.URL(SE.google('{} site:github.com'),
                                   SE.LuckyQuery.url('{} site:github.com'),
                                   'https://github.com/bbugyi200/{}',
                                   patterns=(SE.LuckyQuery.pattern, '^%40'),
                                   filters=(SE.LuckyQuery.filter, lambda x: x.replace('%40', '')))
c.url.searchengines['ghi'] = SE.URL('https://github.com/bbugyi200/{}/issues',
                                    'https://github.com/bbugyi200/scripts/issues/{}',
                                    'https://github.com/bbugyi200/{1}/issues/{0}',
                                    patterns=('^[0-9]+$', SE.OneIntQuery.pattern),
                                    filters=(None, SE.OneIntQuery.filter))
c.url.searchengines['li'] = SE.google('site:linkedin.com {}')
c.url.searchengines['py'] = 'https://docs.python.org/2/library/{}'
c.url.searchengines['red'] = SE.google('site:reddit.com {}')
c.url.searchengines['waf'] = 'https://waffle.io/bbugyi200/{}'
c.url.searchengines['lib'] = 'http://libgen.io/search.php?req={}'
c.url.searchengines['pir'] = SE.URL('https://thepiratebay.org/search/{}',
                                    'https://thepiratebay.org/search/{2} S{0:02d}E{1:02d}',
                                    patterns=SE.TwoIntQuery.pattern,
                                    filters=SE.TwoIntQuery.filter)
c.url.searchengines['sub'] = SE.URL(SE.LuckyQuery.url('{0} inurl:english site:subscene.com'),
                                    SE.LuckyQuery.url('{2} S{0:02d}E{1:02d} inurl:english site:subscene.com'),
                                    patterns=SE.TwoIntQuery.pattern,
                                    filters=SE.TwoIntQuery.filter)


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

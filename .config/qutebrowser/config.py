import yaml

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

# Load autoconfig.yml
config.load_autoconfig()


# ----- Dictionary Values
c.url.searchengines['DEFAULT'] = 'http://www.google.com/search?q={}'
c.url.searchengines['a'] = 'https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords={}'
c.url.searchengines['al'] = 'http://www.google.com/search?q=arch+linux+{}'
c.url.searchengines['d'] = 'http://www.dictionary.com/browse/{}'
c.url.searchengines['gg'] = 'https://www.google.com/search?q=site%3Agithub.com+{}'
c.url.searchengines['ggg'] = 'https://github.com/bbugyi200/{}'
c.url.searchengines['ggi'] = 'https://github.com/bbugyi200/{}/issues'
c.url.searchengines['ggii'] = 'https://github.com/bbugyi200/{}/issues/new'
c.url.searchengines['img'] = 'https://www.google.com/search?tbm=isch&q={}'
c.url.searchengines['li'] = 'https://www.google.com/search?q=site%3Alinkedin.com+{}'
c.url.searchengines['py'] = 'https://docs.python.org/2/library/{}'
c.url.searchengines['red'] = 'https://www.google.com/search?q=site%3Areddit.com+{}'
c.url.searchengines['site'] = 'https://www.google.com/search?q=site%3A{}'
c.url.searchengines['t'] = 'http://www.thesaurus.com/browse/{}'
c.url.searchengines['w'] = 'https://www.wikipedia.org/w/index.php?title=Special:Search&search={}'
c.url.searchengines['waf'] = 'https://waffle.io/bbugyi200/{}'
c.url.searchengines['yt'] = 'https://www.youtube.com/results?search_query={}'


# ----- Bindings
def bind(key, *commands, mode='normal'):
    config.bind(key, ' ;; '.join(commands), mode=mode)


# >>> INSERT
bind('<Ctrl-i>', 'spawn -d qute-pass-add {url}', mode='insert')
bind('<Ctrl-p>', 'spawn --userscript qute-pass', mode='insert')
bind('<Ctrl-Shift-u>', 'spawn --userscript qute-pass --username-only', mode='insert')
bind('<Ctrl-Shift-p>', 'spawn --userscript qute-pass --password-only', mode='insert')

# >>> COMMAND
bind('<Ctrl-f>', 'edit-command', mode='command')

# >>> NORMAL
# Ctrl
bind('<Ctrl-r>', 'restart')
bind('<Ctrl-t>', 'spawn --userscript taskadd tags:read')
bind('<Ctrl-l>', 'edit-url')
# Leader (,)
bind(',e', 'scroll-to-perc 0', 'later 25 hint inputs -m number',
     'later 50 spawn xdotool key 0', 'later 100 open-editor')
bind(',t', 'config-cycle tabs.position left top')
bind(',s', 'set-cmd-text -s :open -t site {url} ')
bind(',rss', 'spawn --userscript openfeeds')
# Miscellaneous
bind('gi', 'hint inputs')


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

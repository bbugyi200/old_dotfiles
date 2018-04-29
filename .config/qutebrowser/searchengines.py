""" Helper Classes for Search Engine Configuration Options """

import re

import imfeelinglucky as IFL


class URL(str):
    """ URL for 'url.searchengines' dict

    Allows for additional pattern matching.
    """
    def __new__(cls, value, *args, **kwargs):
        return super(URL, cls).__new__(cls, value)

    def __init__(self, default_url, *other_urls, patterns, filters=None):
        self.default_url = default_url
        self.other_urls = other_urls
        if isinstance(patterns, str):
            self.patterns = (patterns,)
            self.filters = (filters,)
        else:
            self.patterns = patterns
            self.filters = filters

        # Allows 'filters' argument to be omitted even when multiple 'other_urls' exist
        if filters is None:
            self.filters = (None,) * len(self.other_urls)

        # Allows 'None' to be given as one of multiple filters
        self.filters = tuple(map(lambda x: x if x else lambda y: y, self.filters))

    def format(self, term, *args, **kwargs):
        for url, pttrn, filter_ in zip(self.other_urls, self.patterns, self.filters):
            if re.match(pttrn, term):
                filtered = filter_(term)
                if isinstance(filtered, str):
                    filtered = (filtered,)

                formatted = str.format(url, *filtered, *args, **kwargs)

                lucky_url_prefix = LuckyQuery.url('')
                if formatted.startswith(lucky_url_prefix):
                    return IFL.getTopLink(formatted[len(lucky_url_prefix):])

                return formatted

        return str.format(self.default_url, term, *args, **kwargs)


class IntQueryFactory:
    """ Template for URL Queries that have Int Arguments """
    def __init__(self, N):
        self.N = N
        pttrn_fmt = '^{}[A-z]'
        int_pttrn = '[0-9]+%20'
        for i in range(self.N-1):
            int_pttrn = int_pttrn + int_pttrn
        self.pattern = pttrn_fmt.format(int_pttrn)

    def filter(self, x):
        y = re.split('%20', x, maxsplit=self.N)
        for i in range(self.N):
            y[i] = int(y[i])
        return y


class LuckyQuery:
    """ Queries that Utilize Google's I'm Feeling Lucky Feature """
    pattern = '^(%5C|/)'

    @staticmethod
    def url(x):
        x = _filter_urlstr(x)
        # dummy link is needed to pass qutebrowser's validation checks
        return 'https://imfeelinglucky/{}'.format(x)

    @staticmethod
    def filter(x):
        return re.sub('(%5C|/)', '', x)


def _filter_urlstr(x):
    mapping = [(' ', '+'), ('%3A', ':')]

    temp = x
    for pattern, repl in mapping:
        temp = re.sub(pattern, repl, temp)

    y = re.sub(r'{(\d)%3A', r'{\1:', temp)
    return y


def google(x):
    x = _filter_urlstr(x)
    return 'https://google.com/search?q={}'.format(x)


def duckduckgo(x):
    x = _filter_urlstr(x)
    return 'https://duckduckgo.com/?q={}'.format(x)


OneIntQuery = IntQueryFactory(1)
TwoIntQuery = IntQueryFactory(2)

"""Qutebrowser SearchEngine Helpers"""

import re

import searchengines.imfeelinglucky as IFL
import searchengines.utils as utils
import searchengines.static as static  # noqa: F401


class URL(str):
    """ Dynamic URL for 'url.searchengines'

    Enables additional pattern matching

    Args:
        default_url (str): default URL to return from 'format'
        *others (tuple): 2 or 3-tuple of the form (URL, pattern, filter)
    """
    def __new__(cls, value, *args, **kwargs):
        return super(URL, cls).__new__(cls, value)

    def __init__(self, default_url, *others):
        self.urls = []
        self.patterns = []
        self.filters = []

        noop_filter = lambda x: x

        try:
            for other in others:
                assert isinstance(other, tuple), "{} is NOT a tuple.".format(other)
                assert len(other) in [2, 3], "{} must be either a 2-tuple or a 3-tuple.".format(other)

                self.urls.append(other[0])
                self.patterns.append(other[1])
                if len(other) == 3:
                    self.filters.append(other[2])
                else:
                    self.filters.append(noop_filter)
        except AssertionError as e:
            raise ValueError(str(e))

        # default pattern and default filter
        self.urls.append(default_url)
        self.patterns.append('.*')
        self.filters.append(noop_filter)

    def format(self, term, *args, **kwargs):
        for url, pttrn, filter_ in zip(self.urls, self.patterns, self.filters):
            if re.match(pttrn, term):
                filtered = filter_(term)
                if isinstance(filtered, str):
                    filtered = (filtered,)

                formatted_url = str.format(url, *filtered, *args, **kwargs)

                if LuckyQuery.is_lucky(formatted_url):
                    formatted_url = LuckyQuery.get_top_link(formatted_url)

                return formatted_url


class LuckyQuery:
    """ Queries that Utilize Google's I'm Feeling Lucky Feature """
    pattern = '^(%5C|/)'

    # dummy url is needed to pass qutebrowser's validation checks
    prefix = 'https://imfeelinglucky/'
    suffix = '@'

    @classmethod
    def url(cls, query, end=''):
        escaped_query = utils.escape(query)
        fmt_url = '{}{{}}{}{}'.format(cls.prefix, cls.suffix, re.sub(r'\{(\d*)\}', r'{{\1}}', end))
        return fmt_url.format(escaped_query)

    @classmethod
    def filter(cls, query):
        return re.sub(cls.pattern, '', query)

    @classmethod
    def is_lucky(cls, url):
        return url.startswith(cls.prefix)

    @classmethod
    def get_top_link(cls, url):
        query, end = url[len(cls.prefix):].split(cls.suffix)
        top_link = IFL.get_top_link(query)
        return '{}/{}'.format(top_link, end) if end else top_link


class IntQueryFactory:
    """ Template for URL Queries that start with Int Arguments """
    def __init__(self, N):
        self.N = N
        pttrn_fmt = '^{}[A-z]'
        int_pttrn = '[0-9]+%20'
        for i in range(self.N - 1):
            int_pttrn = int_pttrn + int_pttrn
        self.pattern = pttrn_fmt.format(int_pttrn)

    def filter(self, query):
        y = re.split('%20', query, maxsplit=self.N)
        for i in range(self.N):
            y[i] = int(y[i])
        return y


OneIntQuery = IntQueryFactory(1)
TwoIntQuery = IntQueryFactory(2)

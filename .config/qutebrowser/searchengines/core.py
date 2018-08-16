"""Core Classes of the 'searchengines' Package

This module is imported directly into the global scope of the 'searchengines' package. All
classes/functions must be added to __all__ or they will NOT be made available.
"""

import re

import searchengines.imfeelinglucky as IFL
import searchengines.utils as utils

__all__ = ['SearchEngine', 'URL', 'LuckyQuery', 'OneIntQuery', 'TwoIntQuery']


class SearchEngine(str):
    """Dynamic SearchEngine for 'url.searchengines'

    Enables additional pattern matching

    Args:
        default_url (str): default URL to return from 'format'
        *url_objects (URL): variable number of URL objects
    """
    def __new__(cls, value, *args, **kwargs):
        return super(SearchEngine, cls).__new__(cls, value)

    def __init__(self, default_url, *url_objects):
        try:
            for U in url_objects:
                assert isinstance(U, URL), "{} is NOT a URL object.".format(U)
        except AssertionError as e:
            raise ValueError(str(e))

        self.url_objects = url_objects + (URL(default_url, '.*'), )

    def format(self, term, *args, **kwargs):
        for url, pttrn, filter_ in self.url_objects:
            if re.match(pttrn, term):
                filtered = filter_(term)
                if isinstance(filtered, str):
                    filtered = (filtered,)

                formatted_url = str.format(url, *filtered, *args, **kwargs)

                if LuckyQuery.is_lucky(formatted_url):
                    formatted_url = LuckyQuery.get_top_link(formatted_url)

                return formatted_url


class URL:
    """URL Object

    Used to initialize a SearchEngine object.

    Args:
        url (str): url string with braces ({}) to represent the search query
        pattern (str): regex pattern used to identify when this URL should be used
        filter_ (opt): a callable object used to filter out garbage in the search query
    """
    def __init__(self, url, pattern, filter_=None):
        self.url = url
        self.pattern = utils.encode(pattern)

        if filter_ is None:
            self.filter = lambda x: x
        else:
            self.filter = filter_

    def __iter__(self):
        return (x for x in [self.url, self.pattern, self.filter])


class LuckyQuery:
    """Queries that Utilize Google's I'm Feeling Lucky Feature"""
    pattern = r'^(\|/)'

    # dummy url is needed to pass qutebrowser's validation checks
    prefix = 'https://imfeelinglucky/'
    suffix = '@'

    @classmethod
    def url(cls, query, end=''):
        encoded_query = utils.encode(query)
        fmt_url = '{}{{}}{}{}'.format(cls.prefix, cls.suffix, re.sub(r'\{(\d*)\}', r'{{\1}}', end))
        return fmt_url.format(encoded_query)

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
    """Template for URL Queries that start with Int Arguments"""
    def __init__(self, N):
        self.N = N
        pttrn_fmt = '^{}[A-z]'
        int_pttrn = '[0-9]+ '
        for i in range(self.N - 1):
            int_pttrn = int_pttrn + int_pttrn
        self.pattern = pttrn_fmt.format(int_pttrn)

    def filter(self, query):
        y = re.split(utils.encode(' '), query, maxsplit=self.N)
        for i in range(self.N):
            y[i] = int(y[i])
        return y


OneIntQuery = IntQueryFactory(1)
TwoIntQuery = IntQueryFactory(2)

""" Qutebrowser SearchEngine Helpers """

import re

import searchengines.imfeelinglucky as IFL
import searchengines.utils as utils
import searchengines.static as static  # noqa: F401


class URL(str):
    """ Dynamic URL for 'url.searchengines'

    Enables additional pattern matching
    """
    def __new__(cls, value, *args, **kwargs):
        return super(URL, cls).__new__(cls, value)

    def __init__(self, default_url, *other_urls, patterns, filters=None):
        self.urls = list(other_urls)
        self.urls.append(default_url)

        if isinstance(patterns, str):
            patterns = [patterns]
            filters = [filters]
        else:
            patterns = list(patterns)
            filters = list(filters)

        # Allows 'filters' argument to be omitted even when multiple 'other_urls' exist
        if filters == [None]:
            self.filters = [None] * len(other_urls)

        # default pattern and default filter
        patterns.append('.*')
        filters.append(None)

        # Allows 'None' to be given as one of multiple filters
        self.filters = list(map(lambda x: x if x else lambda y: y, filters))
        self.patterns = patterns

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

""" Helper Classes for Search Engine Configuration Options """

import re


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
                return str.format(url, *filtered, *args, **kwargs)

        return str.format(self.default_url, term, *args, **kwargs)


class DoubleInt:
    """ URL Pattern with two Int Arguments """
    pattern = '^[0-9][0-9][0-9]?%20[0-9][0-9][0-9]?%20[A-z]'

    @staticmethod
    def filter(x):
        y = re.split('%20', x, maxsplit=2)
        y[0] = int(y[0])
        y[1] = int(y[1])
        return y

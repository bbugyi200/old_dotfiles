"""Core Classes of the 'searchengines' Package

This module is imported directly into the global scope of the 'searchengines' package. All
classes/functions must be added to __all__ or they will NOT be made available.
"""

import re
from typing import Any, Callable, Container, Dict, Generator, Iterable, Iterator, List, NoReturn, Optional, Sequence, Set, Tuple, Type, Union  # noqa

import searchengines.imfeelinglucky as IFL
import searchengines.utils as utils

# ----- Type Aliases -----
filter_type = Callable[[str], Sequence]
# ------------------------


__all__ = [
    'SearchEngine',
    'URL',
    'LuckyURL',
    'OneIntURL',
    'TwoIntURL',
]


class SearchEngine(str):
    """Dynamic SearchEngine for 'url.searchengines'

    Enables additional pattern matching

    Args:
        default_url: default URL to return from 'format'
        *url_objects: variable number of URL objects
    """
    def __new__(cls, default_url: str, *url_objects: 'URL') -> 'SearchEngine':
        return super(SearchEngine, cls).__new__(cls, default_url)  # type: ignore

    def __init__(self, default_url: str, *url_objects: 'URL'):
        self.url_objects = url_objects + (URL(default_url, '.*'), )

    def format(self, *args: str, **kwargs: str) -> str:
        term = args[0]
        for url, pttrn, filter_ in self.url_objects:
            if re.match(pttrn, term):
                filtered = filter_(utils.filter_aliases(term))

                if isinstance(filtered, str):
                    filtered = (filtered,)

                formatted_url = str.format(url, *filtered, *args[1:], **kwargs)

                if LuckyURL.is_lucky(formatted_url):
                    formatted_url = LuckyURL.get_top_link(formatted_url)

                return formatted_url
        else:
            return str.format(term, *args, **kwargs)


class URL:
    """URL Object

    Used to initialize a SearchEngine object.

    Args:
        url (str): url string with braces ({}) to represent the search query
        pattern (str): regex pattern used to identify when this URL should be used
        filter_ (callable, optional): used to filter out garbage in the search query
    """
    def __init__(self, url: str, pattern: str = None, filter_: filter_type = None):
        self.url = url

        if pattern is None:
            self.pattern = '.*'
        else:
            self.pattern = utils.encode(pattern)

        if filter_ is None:
            self.filter = lambda x: x
        else:
            self.filter = filter_

    def __iter__(self) -> Generator:
        return (x for x in [self.url, self.pattern, self.filter])


class LuckyURL(URL):
    """Queries that Utilize Google's 'I'm Feeling Lucky' Feature"""
    pattern = r'^(\|/)'

    # dummy url is needed to pass qutebrowser's validation checks
    start_mark = 'https://imfeelinglucky/'
    end_mark = '@'

    def __init__(self, url: str, pattern: str = None, filter_: filter_type = None, suffix: str = ''):
        if pattern is not None:
            self.pattern = pattern

        if filter_ is not None:
            self.filter = filter_  # type: ignore

        super().__init__(self.make_lucky(url, suffix=suffix), self.pattern, self.filter)

    def make_lucky(self, query: str, suffix: str = '') -> str:
        query = utils.encode(query)
        fmt_url = '{}{{}}{}{}'.format(self.start_mark, self.end_mark, re.sub(r'\{(\d*)\}', r'{{\1}}', suffix))
        return fmt_url.format(query)

    @classmethod
    def filter(cls, query: str) -> str:
        return re.sub(cls.pattern, '', query)

    @classmethod
    def is_lucky(cls, url: str) -> bool:
        return url.startswith(cls.start_mark)

    @classmethod
    def get_top_link(cls, url: str) -> str:
        query, suffix = url[len(cls.start_mark):].split(cls.end_mark)
        top_link = IFL.get_top_link(query)
        return '{}/{}'.format(top_link, suffix) if suffix else top_link


def IntURLFactory(n: int) -> Type[URL]:
    """Factory for URL Objects with patterns that start with Int Arguments

    Args:
        n (int): number of integers that the pattern starts with
    """
    pttrn_fmt = '^{}[A-z]'
    int_pttrn = '[0-9]+ ' * n

    class IntURL(URL):
        pattern = pttrn_fmt.format(int_pttrn)

        def __init__(self, url: str, filter_: filter_type = None):
            if filter_ is not None:
                self.filter = filter_  # type: ignore

            super().__init__(url, self.pattern, self.filter)

        @classmethod
        def filter(cls, query: str) -> Sequence:
            nums: List = re.split(utils.encode(' '), query, maxsplit=n)

            ret = nums[:]
            for i in range(n):
                ret[i] = int(nums[i])

            return ret

    return IntURL


OneIntURL = IntURLFactory(1)
TwoIntURL = IntURLFactory(2)

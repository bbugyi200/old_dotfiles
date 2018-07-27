"""Static URL Helpers"""

import datetime as dt

import searchengines.utils as utils


def stackoverflow(n, *, prefix=None):
    """Returns stackoverflow google search string.

    The search results returned by Google will range from @n years ago until now.

    Args:
        prefix (opt): If a string is provided for @prefix, it will be appended to the final query.
    """
    try:
        assert isinstance(prefix, (str, type(None))), "@prefix arguement must be a string."
    except AssertionError as e:
        raise ValueError(str(e))

    if prefix is None:
        prefix = ''
    elif prefix[-1] != ' ':
        prefix = prefix + ' '

    fmt = '{0}{{}} site:stackoverflow.com&source=lnt&tbs=cdr%3A1%2Ccd_min%3A{1}%2F{2}%2F{3}%2Ccd_max%3A&tbm='
    D = _n_years_ago(n)

    return google(fmt.format(prefix, D.month, D.day, D.year))


def _n_years_ago(n):
    """Return a datetime N years ago."""
    today = dt.date.today()
    return today.replace(year=(today.year - n))


def google(query):
    escaped_query = utils.escape(query)
    return 'https://google.com/search?q={}'.format(escaped_query)


def duckduckgo(query):
    escaped_query = utils.escape(query)
    return 'https://duckduckgo.com/?q={}'.format(escaped_query)

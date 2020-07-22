"""Static URL Helpers"""

import datetime as dt

import searchengines as SE
import searchengines.utils as utils


############
#  Public  #
############
def stackoverflow(n: int, *, prefix: str = None) -> SE.SearchEngine:
    """Returns stackoverflow google search string.

    The search results returned by Google will range from @n years ago until
    now.

    Args:
        prefix (opt): static content to prepend to query.
    """
    prefix = _validate_prefix(prefix)
    return google(f'{prefix}{{}} site:stackoverflow.com', max_years_old=n)


def site(*domains: str, prefix: str = None) -> 'SE.SearchEngine':
    """Returns google search string using Google's advanced 'site' option.

    Args:
        prefix (opt): static content to prepend to query.
    """
    prefix = _validate_prefix(prefix)
    return google(
        '{0}{{}} {1}'.format(
            prefix, ' OR '.join(['site:' + D for D in domains])
        )
    )


def google(query: str, *, max_years_old: int = None) -> SE.SearchEngine:
    encoded_query = utils.encode(query)
    if max_years_old is None:
        return SE.SearchEngine(
            'https://google.com/search?q={}'.format(encoded_query)
        )
    else:
        D = _n_years_ago(max_years_old)
        return SE.SearchEngine(
            f'https://google.com/search?q={encoded_query}&source=lnt&tbs=cdr%3A1%2Ccd_min%3A{D.month}%2F{D.day}%2F{D.year}%2Ccd_max%3A&tbm='
        )


def duckduckgo(query: str) -> 'SE.SearchEngine':
    encoded_query = utils.encode(query)
    return SE.SearchEngine(
        'https://duckduckgo.com/?q={}'.format(encoded_query)
    )


#############
#  Private  #
#############
def _n_years_ago(n: int) -> dt.date:
    """Return a datetime N years ago."""
    today = dt.date.today()
    # So qutebrowser doesn't fail to start once every 1461 days ;).
    if today.month == 2 and today.day == 29:
        return today.replace(year=(today.year - n), day=28)
    else:
        return today.replace(year=(today.year - n))


def _validate_prefix(prefix: str = None) -> str:
    """Validates and Beautifies @prefix Argument"""
    if prefix is None:
        return ''
    elif prefix[-1] != ' ':
        prefix = prefix + ' '

    return utils.encode(prefix)

"""Static URL Helpers"""

import searchengines.utils as utils


def google(query):
    escaped_query = utils.escape(query)
    return 'https://google.com/search?q={}'.format(escaped_query)


def duckduckgo(query):
    escaped_query = utils.escape(query)
    return 'https://duckduckgo.com/?q={}'.format(escaped_query)

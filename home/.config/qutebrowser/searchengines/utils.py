"""Qutebrowser SearchEngine Utilities"""

import re


def encode(query):
    """Encodes @query using HTML URL Codes

    A list of HTML URL Codes can be found at: https://www.w3schools.com/tags/ref_urlencode.asp
    """
    code_map = [(' ', '%20'), ('!', '%21'), ('#', '%23'), ('\\+', '%2B'), (':', '%3A'),
                ('@', '%40'), ('\\', '%5C')]

    encoded = query
    for a, b in code_map:
        encoded = encoded.replace(a, b)

    # preserves python format strings
    encoded = re.sub(r'{(\d)%3A', r'{\1:', encoded)

    return encoded


def bang_pttrn():
    """Returns regex pattern that matches DuckDuckGo bangs that I like to use."""
    one_letter_bangs = ['a', 'd', 'g', 'm', 't', 'w', ]
    two_letter_bangs = ['gm', 'ho', 'wa', 'yt', ]
    long_bangs = ['gt[A-z][A-z]+', 'ddg', 'bang', 'giphy', ]
    included_bangs = one_letter_bangs + two_letter_bangs + long_bangs

    bang_fmt = '^({}) '
    return bang_fmt.format('|'.join(included_bangs))

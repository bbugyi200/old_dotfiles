"""Qutebrowser SearchEngine Utilities"""

import re


def encode(query):
    """Encodes @query using HTML URL Codes

    A list of HTML URL Codes can be found at: https://www.w3schools.com/tags/ref_urlencode.asp
    """
    code_map = [('!', '%21'), ('#', '%23'), (' ', '%2B'), (':', '%3A'), ('@', '%40'), ('\\', '%5C')]

    temp = query
    for a, b in code_map:
        temp = temp.replace(a, b)

    # preserves python format strings
    y = re.sub(r'{(\d)%3A', r'{\1:', temp)
    return y

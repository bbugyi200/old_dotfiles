"""Qutebrowser SearchEngine Utilities"""

import re


def encode(query):
    """Encodes @query using HTML URL Codes

    A list of HTML URL Codes can be found at: https://www.w3schools.com/tags/ref_urlencode.asp
    """
    code_map = [(' ', '%20'), ('!', '%21'), ('#', '%23'), (':', '%3A'), ('@', '%40'), ('\\', '%5C')]

    encoded = query
    for a, b in code_map:
        encoded = encoded.replace(a, b)

    # preserves python format strings
    encoded = re.sub(r'{(\d)%3A', r'{\1:', encoded)

    return encoded

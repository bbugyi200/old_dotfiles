"""Qutebrowser SearchEngine Utilities"""

import re


def encode(query):
    character_map = [('!', '%21'), ('#', '%23'), (' ', '%2B'), (':', '%3A'), ('@', '%40'), ('\\', '%5C')]

    temp = query
    for a, b in character_map:
        temp = temp.replace(a, b)

    # preserves python format strings
    y = re.sub(r'{(\d)%3A', r'{\1:', temp)
    return y

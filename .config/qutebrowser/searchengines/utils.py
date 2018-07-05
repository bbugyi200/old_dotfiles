""" Qutebrowser SearchEngine Utilities """

import re


def escape(query):
    character_map = [(r'\+', '%2B'), (' ', '+'), (':', '%3A')]

    temp = query
    for pattern, repl in character_map:
        temp = re.sub(pattern, repl, temp)

    # preserves python format strings
    y = re.sub(r'{(\d)%3A', r'{\1:', temp)
    return y

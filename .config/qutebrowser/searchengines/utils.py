""" Qutebrowser SearchEngine Utilities """

import re


def escape_query(x):
    mapping = [(' ', '+'), (':', '%3A')]

    temp = x
    for pattern, repl in mapping:
        temp = re.sub(pattern, repl, temp)

    # preserves python format strings
    y = re.sub(r'{(\d)%3A', r'{\1:', temp)
    return y

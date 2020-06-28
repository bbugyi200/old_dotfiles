"""Qutebrowser SearchEngine Utilities"""

import re


# Defined in the config.py file.
search_aliases: dict = {}


def encode(query: str) -> str:
    """Encodes @query using HTML URL Codes

    A list of HTML URL Codes can be found at: https://www.w3schools.com/tags/ref_urlencode.asp
    """
    code_map = [
        (' ', '%20'),
        ('!', '%21'),
        ('#', '%23'),
        ('\\+', '%2B'),
        (':', '%3A'),
        ('@', '%40'),
        ('\\', '%5C'),
    ]

    encoded = query
    for a, b in code_map:
        encoded = encoded.replace(a, b)

    # preserves python format strings
    encoded = re.sub(r'{(\d)%3A', r'{\1:', encoded)

    return encoded


def filter_aliases(search_term: str) -> str:
    """Checks for and Substitutes Aliases with their Definitions"""
    new_search_term = search_term
    for alias, definition in search_aliases.items():
        new_search_term = re.sub(
            r'\b{}\b'.format(alias), definition, new_search_term
        )
        new_search_term = re.sub(
            r'%20{}%20'.format(alias),
            '%20' + definition + '%20',
            new_search_term,
        )
        new_search_term = re.sub(
            r'^{}%20'.format(alias), definition + '%20', new_search_term
        )
        new_search_term = re.sub(
            r'%20{}$'.format(alias), '%20' + definition, new_search_term
        )

    return new_search_term

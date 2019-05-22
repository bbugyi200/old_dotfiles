"""Prints the URL of the First Google Search Result for the Given Query"""

##########################################################
#  http://edmundmartin.com/scraping-google-with-python/  #
##########################################################

import argparse
import re

import searchengines.utils as utils

USER_AGENT = {'User-Agent': 'Mozilla/5.0 (X11; U; Linux i686; en-US) AppleWebKit/534.3 (KHTML, like Gecko) Chrome/6.0.472.63 Safari/534.3'}


def get_top_link(query: str) -> str:
    from bs4 import BeautifulSoup
    import requests

    try:
        html = _fetch_results(query)
    except requests.exceptions.HTTPError as e:
        return e.response.url

    soup = BeautifulSoup(html, 'html.parser')
    result_block = soup.find_all('div', attrs={'class': 'g'})
    for result in result_block:
        link = result.find('a', href=True)
        if link and link != '#' and re.match('^http[s]?://', link['href']):
            return link['href']

    return 'https://www.google.com/search?q={}'.format(utils.encode(query))


def _fetch_results(query: str) -> str:
    # dynamic import needed to work around weird qutebrowser bug with 'cryptography' module
    import requests

    try:
        assert isinstance(query, str), 'Search term must be a string'
    except AssertionError as e:
        raise ValueError(str(e))

    encoded_query = utils.encode(query)

    google_url = 'https://www.google.com/search?q={}'.format(encoded_query)
    response = requests.get(google_url, headers=USER_AGENT)
    response.raise_for_status()

    return response.text


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('query', help='Google Search Query')
    args = parser.parse_args()

    print(get_top_link(args.query))

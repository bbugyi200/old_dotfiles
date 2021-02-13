"""Prints the URL of the First Google Search Result for the Given Query"""

##########################################################
#  http://edmundmartin.com/scraping-google-with-python/  #
##########################################################

import argparse
import os
from os.path import join
import re
import sys

import searchengines.utils as utils


if sys.platform == "darwin":
    PY_TOP_DIR = "/usr/local/Cellar/python"
    PY3_VERSION_SUBDIR = sorted(os.listdir(PY_TOP_DIR))[-1]
    PY3_LIB_DIR = join(
        PY_TOP_DIR,
        PY3_VERSION_SUBDIR,
        "Frameworks/Python.framework/Versions/3.7/lib/python3.7",
    )

USER_AGENT = {
    'User-Agent': (
        'Mozilla/5.0 (X11; U; Linux i686; en-US) AppleWebKit/534.3 (KHTML, '
        'like Gecko) Chrome/6.0.472.63 Safari/534.3'
    )
}


def get_top_link(query: str) -> str:
    if sys.platform == "darwin":
        import importlib.util  # pylint: disable=import-outside-toplevel

        spec = importlib.util.spec_from_file_location(
            "http.cookies", join(PY3_LIB_DIR, "http/cookies.py"),
        )
        cookies = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cookies)
        sys.modules["http.cookies"] = cookies

        sys.path.append("/usr/local/lib/python3.7/site-packages")

    from bs4 import BeautifulSoup
    import requests  # pylint: disable=import-outside-toplevel

    try:
        html = _fetch_results(query)
    except requests.exceptions.HTTPError as e:
        return e.response.url

    soup = BeautifulSoup(html, 'html.parser')
    a_tags = soup.find_all("a", href=True)
    for a_tag in a_tags:
        match = re.match(r"^/url\?q=(.*?)&.*$", a_tag['href'])
        if match:
            return match.group(1)

    div_tags = soup.find_all('div', attrs={'class': 'g'})
    for div_tag in div_tags:
        a_tag = div_tag.find('a', href=True)
        if a_tag and a_tag != '#' and re.match('^http[s]?://', a_tag['href']):
            return a_tag['href']

    return 'https://www.google.com/search?q={}'.format(utils.encode(query))


def _fetch_results(query: str) -> str:
    if sys.platform == "darwin":
        sys.path.append(PY3_LIB_DIR)

    # dynamic import needed to work around weird qutebrowser bug with
    # 'cryptography' module
    import requests  # pylint: disable=import-outside-toplevel

    assert isinstance(query, str), 'Search term must be a string'

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

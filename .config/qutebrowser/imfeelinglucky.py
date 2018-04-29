#!/usr/bin/env python

""" Prints the URL of the First Google Search Result for the Given Query """

##########################################################
#  http://edmundmartin.com/scraping-google-with-python/  #
##########################################################

import argparse
import re
import requests
from bs4 import BeautifulSoup

USER_AGENT = {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36}'}


def escape_query(query):
    return query.replace(' ', '+').replace(':', '%3A')


def fetch_results(query):
    assert isinstance(query, str), 'Search term must be a string'
    escaped_query = escape_query(query)

    google_url = 'https://www.google.com/search?q={}'.format(escaped_query)
    response = requests.get(google_url, headers=USER_AGENT)
    response.raise_for_status()

    return response.text


def getTopLink(query):
    html = fetch_results(query)
    soup = BeautifulSoup(html, 'html.parser')
    result_block = soup.find_all('div', attrs={'class': 'g'})
    for result in result_block:
        link = result.find('a', href=True)
        if link and link != '#' and re.match('^http[s]?://', link['href']):
            return link['href']

    return 'https://www.google.com/search?q={}'.format(escape_query(query))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('query', help='Google Search Query')
    args = parser.parse_args()

    print(getTopLink(args.query))

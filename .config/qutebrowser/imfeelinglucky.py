#!/usr/bin/env python

""" Prints the URL of the First Google Search Result for the Given Query """

##########################################################
#  http://edmundmartin.com/scraping-google-with-python/  #
##########################################################

import argparse
import requests
from bs4 import BeautifulSoup

USER_AGENT = {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36}'}


def escape_url(query):
    return query.replace(' ', '+').replace(':', '%3A')


def fetch_results(query):
    assert isinstance(query, str), 'Search term must be a string'
    escaped_query = escape_url(query)

    google_url = 'https://www.google.com/search?q={}'.format(escaped_query)
    response = requests.get(google_url, headers=USER_AGENT)
    response.raise_for_status()

    return query, response.text


def parse_results(html, keyword):
    soup = BeautifulSoup(html, 'html.parser')

    found_results = []
    rank = 1
    result_block = soup.find_all('div', attrs={'class': 'g'})
    for result in result_block:

        link = result.find('a', href=True)
        title = result.find('h3', attrs={'class': 'r'})
        description = result.find('span', attrs={'class': 'st'})
        if link and title:
            link = link['href']
            title = title.get_text()
            if description:
                description = description.get_text()
            if link != '#':
                found_results.append({'keyword': keyword, 'rank': rank, 'title': title, 'description': description, 'link': link})
                rank += 1
    return found_results


def getTopLink(query):
    try:
        keyword, html = fetch_results(query)
        results = parse_results(html, keyword)
        return results[0]['link']
    except IndexError as e:
        return 'https://www.google.com/search?q={}'.format(escape_url(query))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('query', help='Google Search Query')
    args = parser.parse_args()

    print(getTopLink(args.query))

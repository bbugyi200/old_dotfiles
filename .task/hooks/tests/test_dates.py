"""Tests for dates.py"""

import subprocess as sp
import unittest.mock as mock

import pytest

import dates


def test_get_today(shell_date_str):
    today = dates.get_today()
    assert today == shell_date_str


def test_get_today_dt(shell_date_str):
    today_dt = dates.get_today_dt()
    assert today_dt.strftime(dates.date_fmt) == shell_date_str


@pytest.fixture
def shell_date_str():
    out = sp.check_output(['date', '+{}'.format(dates.date_fmt.split('T')[0])])
    return out.decode().strip() + 'T100000Z'

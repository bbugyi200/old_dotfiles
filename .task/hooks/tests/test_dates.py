"""Tests for dates.py"""

import subprocess as sp
import unittest.mock as mock

import pytest

from utils import dates


def test_get_today(today_shell_date):
    today = dates.get_today()
    assert today == today_shell_date


def test_get_today_dt(today_shell_date):
    today_dt = dates.get_today_dt()
    assert today_dt.strftime(dates.date_fmt) == today_shell_date


def test_get_tomorrow(tomorrow_shell_date):
    tomorrow = dates.get_tomorrow()
    assert tomorrow == tomorrow_shell_date


@pytest.fixture
def today_shell_date():
    return shell_date()


@pytest.fixture
def tomorrow_shell_date():
    return shell_date(date='tomorrow')


def shell_date(date='today'):
    cmd_list = ['date']
    cmd_list.extend(['--date', date])
    cmd_list.extend(['+{}'.format(dates.date_fmt.split('T')[0])])

    out = sp.check_output(cmd_list)
    return out.decode().strip() + 'T100000Z'

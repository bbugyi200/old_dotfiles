""" Functions Relating to Dates and Times"""

import datetime as dt

date_fmt = '%Y%m%dT%H%M%SZ'


def getTodayDT():
    """Return datetime for today (at 6AM)"""
    now = dt.datetime.now().astimezone(tz=dt.timezone.utc)
    today = now - dt.timedelta(hours=10)
    today = today.replace(hour=10, minute=0, second=0, microsecond=0)
    return today.astimezone(tz=dt.timezone.utc)


def getToday():
    """Returns Formated Datetime for Today (at 6AM)"""
    today = getTodayDT()
    return today.strftime(date_fmt)


def due_in_N_years(years, due_date):
    """Returns New Due Date for Annually Recurring Tasks"""
    new_year = due_date.year + years
    new_due = due_date.replace(year=new_year)

    return new_due


def get_new_wait(task):
    """Return Formatted wait datetime Based on Existing 'delta' Field"""
    new_wait = dt.datetime.strptime(task['due'], date_fmt) - dt.timedelta(days=task['delta'])
    return new_wait.strftime(date_fmt)

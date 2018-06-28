""" Functions Relating to Dates and Times"""

import datetime as dt

date_fmt = '%Y%m%dT%H%M%SZ'


def _style_dt(old_dt):
    """Return datetime for today (at 6AM)"""
    new_dt = old_dt.astimezone(tz=dt.timezone.utc)
    new_dt = new_dt - dt.timedelta(hours=10)
    new_dt = new_dt.replace(hour=10, minute=0, second=0, microsecond=0)
    return new_dt.astimezone(tz=dt.timezone.utc)


def _convert_to_dt(dt_string):
    return dt.datetime.strptime(dt_string, date_fmt)


def get_today():
    """Returns Formated Datetime for Today (at 6AM)"""
    today = _style_dt(dt.datetime.today())
    return today.strftime(date_fmt)


def get_tomorrow():
    """Returns Formated Datetime for Tomorrow (at 6AM)"""
    tomorrow = _style_dt(dt.datetime.today() + dt.timedelta(hours=18))
    return tomorrow.strftime(date_fmt)


def get_today_dt():
    """Returns Datetime for Today (at 6AM)"""
    return _convert_to_dt(get_today())


def due_in_N_years(years, due_date):
    """Returns New Due Date for Annually Recurring Tasks"""
    new_year = due_date.year + years
    new_due = due_date.replace(year=new_year)

    return new_due


def get_new_wait(task):
    """Return Formatted wait datetime Based on Existing 'delta' Field"""
    new_wait = dt.datetime.strptime(task['due'], date_fmt) - dt.timedelta(days=task['delta'])
    return new_wait.strftime(date_fmt)

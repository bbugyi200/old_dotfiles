import datetime as dt
from dateutil.parser import parse

date_fmt = '%Y%m%dT%H%M%SZ'


def getTodayDT():
    now = dt.datetime.now().astimezone(tz=dt.timezone.utc)
    today = now - dt.timedelta(hours=10)
    today = today.replace(hour=10, minute=0, second=0, microsecond=0)
    return today.astimezone(tz=dt.timezone.utc)


def getToday():
    """ Returns Formated Datetime for Today (at 6AM) """
    today = getTodayDT()
    return today.strftime(date_fmt)


def due_in_N_years(years, due_date):
    """ Returns New Due Date for Annually Recurring Tasks """
    new_year = due_date.year + years
    new_due = due_date.replace(year=new_year)

    return new_due


def get_new_wait(task):
    new_wait = parse(task['due']) - dt.timedelta(days=task['wait_delta'])
    return new_wait.strftime(date_fmt)

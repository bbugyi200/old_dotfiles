import datetime as dt

date_fmt = '%Y%m%dT%H%M%SZ'


def getToday():
    """ Returns Formated Datetime for Today (at 6AM) """
    today = dt.datetime.now()
    today = today.replace(hour=6, minute=0, second=0, microsecond=0)
    today = today.astimezone(tz=dt.timezone.utc)
    return today.strftime(date_fmt)


def due_in_N_years(due_date, years):
    """ Returns New Due Date for Annually Recurring Tasks """
    new_year = due_date.year + years
    return due_date.replace(year=new_year)

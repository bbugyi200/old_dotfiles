"""Hooks related to custom repeats (see defaults.py)"""

import datetime as dt

from hooks import defaults
from hooks import utils
from hooks.utils import log
from hooks.utils import dates

logger = log.getLogger()


def run(new_task, old_task=None):
    log.running(logger)

    if 'delta' in new_task.keys():
        if utils.fields_equiv(new_task, old_task, 'wait') and (new_task['delta'] >= 0):
            new_task['wait'] = dates.get_new_wait(new_task)
            out = "'delta' Field Exists => task[wait] = task[due] - {}d\n".format(int(new_task['delta']))
            print(out)
    else:
        new_task = _set_delta(new_task)

    if old_task is not None and utils.is_done(new_task) and not utils.is_done(old_task):
        logger.debug('Task has been marked COMPLETED: {}'.format(new_task['description'][:40]))
        new_task = _revive_repeat(new_task)

    return new_task


def _set_delta(new_task):
    """Sets 'task[delta]' to Integer Difference of 'task[due]' and 'task[wait]'"""
    if utils.field_equals(new_task, 'repeat', 'yes'):
        if 'wait' in new_task.keys():
            tdelta = dt.datetime.strptime(new_task['due'], dates.date_fmt) - dt.datetime.strptime(new_task['wait'], dates.date_fmt)
            if tdelta.days < 0:
                new_task['delta'] = -1
                new_task.pop('wait')
                print('Negative wait removed.')
            else:
                new_task['delta'] = tdelta.days
                print('[repeat.delta] set to {} for repeating task.'.format(tdelta.days))
        else:
            new_task['delta'] = -1

    return new_task


def _revive_repeat(new_task):
    """Revives Tasks with Custom Repeat Tags

    Checks if task has a custom repeat tag. If so, the task is brought back into pending status and
    a new due date is set.
    """
    for tag, N in defaults.repeats.items():
        if utils.has_tag(new_task, tag):
            msg = '+{} tag found. Task "{}..." has been identified as a custom repeat.'
            logger.debug(msg.format(tag, new_task['description'][:40]))
            due_date = dt.datetime.strptime(new_task['due'], dates.date_fmt)

            todayDT = dates.get_today_dt()
            if not utils.field_equals(new_task, 'consistent', 'yes') and (due_date.astimezone(tz=dt.timezone.utc) < todayDT):
                due_date = todayDT

            if any(x in tag for x in ['annual', 'year']):
                new_due = dates.due_in_N_years(N, due_date)
            else:
                new_due = due_date + dt.timedelta(days=N)

            new_task['due'] = new_due.strftime(dates.date_fmt)

            if new_task['delta'] >= 0:
                new_task['wait'] = dates.get_new_wait(new_task)

            new_task['status'] = 'pending'
            new_task.pop('end')

    return new_task

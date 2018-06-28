"""Done (completed) on-modify routines."""

import datetime as dt
import dates
import defaults
import github
import log
import tags


def run(old_task, new_task):
    """Run 'done' (completed) hooks

    This hook contains functions that are run when a task is completed (e.g. `task done`). This
    includes:
       - Implementing custom "repeat" tag recurrences.
       - Closing corresponding GitHub Issues.
    """
    if tags.isDone(new_task) and not tags.isDone(old_task):
        log.logger.debug('Task has been marked COMPLETED: {}'.format(new_task['description'][:40]))
        if github.is_issue(new_task):
            github.close_issue(new_task)
        new_task = _revive_repeat(new_task)


def _isConsistent(task):
    """Returns True if 'consistent' attribute is present and enabled."""
    return ('tags' in task.keys()) and ('consistent' in task.keys()) and (task['consistent'] == 'yes')


def _revive_repeat(new_task):
    """Checks if task has a custom repeat tag. If so, the task is brought back into pending
    status and a new due date is set.
    """
    for tag, N in defaults.repeats.items():
        if tags.hasTag(new_task, tag):
            msg = '+{} tag found. Task "{}..." has been identified as a custom repeat.'
            log.logger.debug(msg.format(tag, new_task['description'][:40]))
            due_date = dt.datetime.strptime(new_task['due'], dates.date_fmt)

            todayDT = dates.get_today_dt()
            if not _isConsistent(new_task) and (due_date.astimezone(tz=dt.timezone.utc) < todayDT):
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

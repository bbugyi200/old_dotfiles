""" This module defines defaults for tasks matching particular
    filters (e.g. have a particular tag)
"""

import datetime as dt
import sys

from dates import getToday


class FieldRef:
    """ Field Reference

    References one of a Task's fields. Setting <foo> field's default to `FieldRef(<bar>)` is
    equivalent to `task add ... foo:bar ...`.

    You can also set the <foo> field's default to `FieldRef(<foo>)` to enforce a mandatory
    field.
    """
    def __init__(self, field):
        self.field = field


class ModList:
    """ Modify a List (used when field value is a list)

    For example, you can use this to add or remove a tag from the 'tags' field.
    """
    mode_opts = ['+', '-']

    def __init__(self, items, modes: '(+|-, ..., +|-)'):
        if not isinstance(items, str) and not isinstance(modes, str):
            self.items = items
            self.modes = modes
        else:
            self.items = (items, )
            self.modes = (modes, )

        if not all(mode in self.mode_opts for mode in modes):
            print("Mode must be one of the following!: {}", self.mode_opts)
            sys.exit(1)


tomorrow = dt.datetime.today() + dt.timedelta(hours=18)
tomorrow_due_time = '{year}{month:02d}{day:02d}T100000Z'.format(year=tomorrow.year,
                                                             month=tomorrow.month,
                                                             day=tomorrow.day)

tags = {'GTD': {'project': 'Meta'},
        'inbox': {
            'project': 'Meta',
            'due': tomorrow_due_time,
            'wait': tomorrow_due_time},
        'tickle': {
            'project': 'Meta',
            'due': tomorrow_due_time,
            'delta': 0},
        'reminder': {
            'project': 'Meta',
            'due': tomorrow_due_time,
            'delta': 1,
            'tags': ModList(('reminder', 'tickle'), ('-', '+'))},
        'tv': {'project': 'Fun'},
        'read': {'project': 'Fun'},
        'consistent': {
            'consistent': 'yes',
            'tags': ModList('consistent', '-')},
        'strict': {
            'strict': 'yes',
            'tags': ModList('strict', '-')},
        'call': {'tags': ModList('go', '+')},
        'dev': {
            'project': 'Dev',
            'tags': ModList('dev', '-')},
        'bug': {
            'project': 'Dev',
            'priority': 'H'}}


# Note that the 'annual' and 'Nyears' tags are treated differently
# so we make sure to adjust for leap years.
repeat_basics = {
        'daily': 1,
        'weekly': 7,
        'monthly': 30,
        'annually': 1
}

repeats = {}
for prefix, i in [('', 1), ('bi', 2)]:
    for basic, N in repeat_basics.items():
        repeats[prefix + basic] = i * N

for i in range(3,7):
    for period, N in {'{}days': 1, '{}weeks': 7,
                      '{}months': 30, '{}years': 1}.items():
        repeats[period.format(i)] = int(i * N)

for key in repeats.keys():
    tags[key] = {
            'due': getToday(),
            'wait': FieldRef('due'),
            'repeat': 'yes'
    }

# Annual tasks should repeat at the same time every year
for tag in ['annually', 'biannually', '3years',
            '4years', '5years', '6years']:
    tags[tag]['consistent'] = 'yes'

# Due dates of Daily Tasks should be respected (higher urgency)
tags['daily']['strict'] = 'yes'
tags['bidaily']['strict'] = 'yes'

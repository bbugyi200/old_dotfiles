"""Defines defaults for tasks matching particular filters (e.g. have a particular tag)

Attributes:
    force_update (list): list of fields that should be updated regardless of whether or not the
        field already exists.
    repeats (dict): a mapping of all special repeat tags to an integer that represents the repeat
        duration (e.g. Ndays, N years, etc.)
    tags (dict): a mapping of special tags to a of dict with the signature {<field>: <new_value>},
        where <new_value> may be a constant value or defined using a special field action class
        defined in this module.
"""

import datetime as dt
import sys

import dates
import log


class FieldRef:
    """Field Reference

    References one of a Task's fields. Setting <foo> field's default to `FieldRef(<bar>)` is
    equivalent to `task add ... foo:bar ...`.

    You can also set the <foo> field's default to `FieldRef(<foo>)` to enforce a mandatory
    field.
    """
    def __init__(self, field):
        self.field = field


class ModList:
    """Modify a List (used when field value is a list)

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


_tomorrow = dt.datetime.today() + dt.timedelta(hours=18)
_tomorrow_due_time = '{year}{month:02d}{day:02d}T100000Z'.format(year=_tomorrow.year,
                                                             month=_tomorrow.month,
                                                             day=_tomorrow.day)

# Public Attributes
force_update = ['severity', 'tags']
repeats = {}
tags = {'inbox': {
            'project': 'Meta',
            'due': _tomorrow_due_time,
            'delta': 0},
        'taskwarrior': {'tags': ModList('GTD', '+')},
        'khal': {'tags': ModList('GTD', '+')},
        'GTD': {'priority': 'H'},
        'tickle': {
            'due': _tomorrow_due_time,
            'delta': 0},
        'remind': {
            'project': 'Meta',
            'due': _tomorrow_due_time,
            'delta': 1,
            'tags': ModList(('remind', 'tickle'), ('-', '+'))},
        'blog': {'project': 'Blogs',
                 'tags': ModList('blog', '-')},
        'doc': {'project': 'Study.Docs',
                'tags': ModList('doc', '-')},
        'video': {'project': 'Videos',
                  'tags': ModList('video', '-')},
        'const': {
            'consistent': 'yes',
            'tags': ModList('const', '-')},
        'strict': {
            'strict': 'yes',
            'tags': ModList('strict', '-')},
        'dev': {
            'project': 'Dev',
            'tags': ModList('dev', '-')},
        'Severity_1': {
            'tags': ModList('Severity_1', '-'),
            'severity': 'critical'},
        'Severity_2': {
            'tags': ModList('Severity_2', '-'),
            'severity': 'high'},
        'Severity_3': {
            'tags': ModList('Severity_3', '-'),
            'severity': 'medium'},
        'Severity_4': {
            'tags': ModList('Severity_4', '-'),
            'severity': 'low'},
        'bug': {
            'project': 'Dev'}}


# Note that the 'annual' and 'Nyears' tags are treated differently
# so we make sure to adjust for leap years.
_repeat_basics = {
        'daily': 1,
        'weekly': 7,
        'monthly': 30,
        'annually': 1
}

for prefix, i in [('', 1), ('bi', 2)]:
    for basic, N in _repeat_basics.items():
        repeats[prefix + basic] = i * N

for i in range(3, 7):
    for period, N in {'{}days': 1, '{}weeks': 7,
                      '{}months': 30, '{}years': 1}.items():
        repeats[period.format(i)] = int(i * N)

for key in repeats.keys():
    tags[key] = {
            'due': dates.getToday(),
            'repeat': 'yes'
    }

# Annual tasks should repeat at the same time every year
for tag in ['annually', 'biannually', '3years',
            '4years', '5years', '6years']:
    tags[tag]['consistent'] = 'yes'

# Due dates of Daily Tasks should be respected (higher urgency)
tags['daily']['strict'] = 'yes'
tags['bidaily']['strict'] = 'yes'

log.logger.debug('Finished defining defaults.')
log.logger.vdebug('repeats: %s', repr(repeats))

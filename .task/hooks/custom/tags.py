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

from hooks.custom import fields
from hooks.utils import log
from utils import dates

logger = log.getLogger()


def _ghost_tag(tag, **kwargs):
    """Helper function for creating custom tag that is deleted once used."""
    D = {'tags': fields.ModTags(tag, '-')}
    D.update(kwargs)
    return D


_today_due_time = dates.get_today()
_tomorrow_due_time = dates.get_tomorrow()

# Public Attributes
force_update = ['description', 'severity', 'tags']
repeats = {}
tags = {
    'bug': {
        'project': 'Dev',
    },
    'call': {
        'tags': fields.ModTags('note', '+'),
    },
    'const': _ghost_tag('const', consistent='yes'),
    'dev': _ghost_tag('dev', project='Dev'),
    'errand': {
        'tags': fields.ModTags('note', '+'),
    },
    'GTD': {
        'priority': 'H',
    },
    'inbox': {
        'project': 'Meta',
        'due': _tomorrow_due_time,
        'wait': fields.Ref('due'),
    },
    'khal': {
        'tags': fields.ModTags('GTD', '+'),
    },
    'note': {
        'description': fields.Wrap(start='Add "', end='" to notebook tasks'),
    },
    'remind': {
        'project': 'Meta',
        'due': _tomorrow_due_time,
        'delta': 1,
        'tags': fields.ModTags(('remind', 'tickle'), ('-', '+')),
    },
    'Severity_1': _ghost_tag('Severity_1', severity='critical'),
    'Severity_2': _ghost_tag('Severity_2', severity='high'),
    'Severity_3': _ghost_tag('Severity_3', severity='medium'),
    'Severity_4': _ghost_tag('Severity_4', severity='low'),
    'taskwarrior': {
        'tags': fields.ModTags('GTD', '+'),
    },
    'tickle': {
        'due': _tomorrow_due_time,
        'delta': 0,
    },
    'today': _ghost_tag('today', due=_today_due_time, project='Misc'),
    'tv': _ghost_tag('tv', project='TV'),
}


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
            'due': _today_due_time,
            'repeat': 'yes'
    }

# Annual tasks should repeat at the same time every year
for tag in ['annually', 'biannually', '3years',
            '4years', '5years', '6years']:
    tags[tag]['consistent'] = 'yes'

logger.vdebug('repeats: %s', repr(repeats))

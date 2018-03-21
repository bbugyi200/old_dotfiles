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

    def __init__(self, item, mode: '+|-'):
        self.item = item
        self.mode = mode

        if mode not in self.mode_opts:
            print("Mode must be one of the following!: {}", self.mode_opts)
            sys.exit(1)


tomorrow = dt.datetime.today() + dt.timedelta(hours=18)
inbox_due_time = '{year}{month:02d}{day:02d}T100000Z'.format(year=tomorrow.year,
                                                             month=tomorrow.month,
                                                             day=tomorrow.day)


tag_defaults = {'GTD': {'project': 'Meta'},
                'hotfix': {'project': 'Meta'},
                'inbox': {'project': 'Meta',
                          'due': inbox_due_time,
                          'wait': inbox_due_time},
                'tickle': {'project': 'Meta',
                           'due': FieldRef('due'),
                           'wait': FieldRef('due')},
                'tv': {'project': 'Fun'},
                'read': {'project': 'Fun'},
                'call': {'tags': ModList('go', '+')},
                'dev': {'project': 'Dev',
                        'tags': ModList('dev', '-')},
                'bug': {'project': 'Dev'}}


# Note that the 'annual' and 'Nyears' tags are treated differently
# so we make sure to adjust for leap years.
basic_recur_tags = {'daily': 1,
                    'weekly': 7,
                    'monthly': 30,
                    'annually': 1}

recur_tags = {}
for prefix, i in [('', 1), ('bi', 2), ('tri', 3)]:
    for basic, N in basic_recur_tags.items():
        recur_tags[prefix + basic] = i * N

for i in range(4,7):
    for period, N in {'{}days': 1, '{}weeks': 7,
                      '{}months': 30, '{}years': 1}.items():
        recur_tags[period.format(i)] = int(i * N)

tag_defaults.update(dict.fromkeys(recur_tags.keys(),
                                  {'due': getToday(),
                                   'wait': FieldRef('due')}))


def hasTag(task, tag):
    return ('tags' in task.keys()) and (tag in task['tags'])


def process_del_tags(new_task, old_task):
    """ Remove default task values when special tag is removed from task """
    header = ' \n======= Custom Tag Removed =======\n'
    output = header
    if 'tags' not in old_task.keys():
        return new_task

    for tag in tag_defaults.keys():
        if (tag in old_task['tags']) and (('tags' not in new_task.keys()) or (tag not in new_task['tags'])):
            for field in tag_defaults[tag].keys():
                try:
                    if new_task[field] == old_task[field]:
                        fmt = "-{tag} => {field}:\n"
                        output += fmt.format(tag=tag, field=field)
                        new_task.pop(field)
                except KeyError as e:
                    pass

    if output != header:
        output += '   \n'  # Spaces Needed
        print(output)

    return new_task


def process_add_tags(new_task, *, old_task={}):
    """ Add default task settings when special tag is added to task """
    header = ' \n======= Custom Tag Added =======\n'
    output = header
    if 'tags' in new_task.keys():
        # Clear 'Misc' project
        try:
            if new_task['project'] == 'Misc':
                new_task.pop('project')
        except KeyError as e:
            pass

        fmt = "+{tag} => {field}{sep}{val}\n"
        for tag in tag_defaults.keys():
            if tag in new_task['tags'] and (('tags' not in old_task.keys()) or (tag not in old_task['tags'])):

                if tag in recur_tags.keys():
                    output += "+{tag} recurrence has been set.\n".format(tag=tag)

                for field, value in tag_defaults[tag].items():
                    if value is None:
                        try:
                            new_task.pop(field)
                        except KeyError as e:
                            pass
                    else:
                        if field not in new_task.keys():
                            if isinstance(value, FieldRef):
                                try:
                                    new_task[field] = new_task[value.field]
                                except KeyError as e:
                                    error_fmt = "The '{field}:' field must be defined when using the '+{tag}' tag."
                                    print(error_fmt.format(field=value.field, tag=tag))
                                    sys.exit(1)
                            else:
                                new_task[field] = value

                            output += fmt.format(tag=tag, field=field, sep=':', val=new_task[field])

                        if isinstance(value, ModList):
                            if value.mode == '+' and value.item not in new_task[field]:
                                if field not in new_task.keys():
                                    new_task[field] = []
                                new_task[field].append(value.item)
                                sep = '.append'
                            elif value.mode == '-':
                                try:
                                    new_task[field].remove(value.item)
                                    sep = '.remove'
                                except ValueError as e:
                                    error_fmt = "Cannot remove '{item}'! '{item}' is not in '{field}'!"
                                    print(error_fmt.format(item=value.item, field=field))
                                    sys.exit(1)

                            output += fmt.format(tag=tag, field=field, sep=sep, val='(\'' + value.item + '\')')

    # Set 'Misc' project if no other project is set
    if 'project' not in new_task.keys():
        new_task['project'] = 'Misc'
        temp = "Default Project Set => Misc\n"
        print(temp)

    # Medium priority should not exist
    if ('priority' in new_task.keys()) and (new_task['priority'] == 'M'):
        new_task.pop('priority')

    if output != header:
        output += '   \n'  # Spaces Needed
        print(output)

    return new_task

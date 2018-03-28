""" Functions Relating to Task Tags """

import sys
from dateutil.parser import parse

import defaults
from dates import get_new_wait


def hasTag(task, tag):
    return ('tags' in task.keys()) and (tag in task['tags'])


def fieldsEquivalent(taskA, taskB, field):
    """ Compares the Given Field of two Tasks

    Returns True if the fields are equal.
    """
    c1 = (field in set(taskA.keys()) & set(taskB.keys())) and (taskA[field] == taskB[field])
    c2 = field not in set(taskA.keys()) | set(taskB.keys())

    return any([c1, c2])


def fieldEquals(task, field, value):
    """ True if 'task[field]' equals 'value' """
    return (field in task.keys()) and (task[field] == value)


def process_del_tags(new_task, old_task):
    """ Remove default task values when special tag is removed from task """
    header = ' \n======= Custom Tag Removed =======\n'
    output = header
    if 'tags' not in old_task.keys():
        return new_task

    for tag in defaults.tags.keys():
        if (tag in old_task['tags']) and (('tags' not in new_task.keys()) or (tag not in new_task['tags'])):
            for field in defaults.tags[tag].keys():
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

        # Medium priority should not exist
        if ('priority' in new_task.keys()) and (new_task['priority'] == 'M'):
            new_task.pop('priority')

        fmt = "+{tag} => {field}{sep}{val}\n"
        for tag in defaults.tags.keys():
            if tag in new_task['tags'] and (('tags' not in old_task.keys()) or (tag not in old_task['tags'])):

                if tag in defaults.repeats.keys():
                    output += "+{tag} recurrence has been set.\n".format(tag=tag)

                for field, value in defaults.tags[tag].items():
                    if value is None:
                        try:
                            new_task.pop(field)
                        except KeyError as e:
                            pass
                    else:
                        if field not in new_task.keys():
                            if isinstance(value, defaults.FieldRef):
                                try:
                                    new_task[field] = new_task[value.field]
                                    output += fmt.format(tag=tag, field=field,
                                                         sep=' = ', val='task[{}]'.format(value.field))
                                except KeyError as e:
                                    error_fmt = "The '{field}:' field must be defined when using the '+{tag}' tag."
                                    print(error_fmt.format(field=value.field, tag=tag))
                                    sys.exit(1)
                            else:
                                new_task[field] = value

                            output += fmt.format(tag=tag, field=field, sep=':', val=new_task[field])

                        if isinstance(value, defaults.ModList):
                            for item, mode in zip(value.items, value.modes):
                                if mode == '+' and item not in new_task[field]:
                                    if field not in new_task.keys():
                                        new_task[field] = []
                                    new_task[field].append(item)
                                    sep = '.append'
                                elif mode == '-':
                                    try:
                                        new_task[field].remove(item)
                                        sep = '.remove'
                                    except ValueError as e:
                                        error_fmt = "Cannot remove '{item}'! '{item}' is not in '{field}'!"
                                        print(error_fmt.format(item=item, field=field))
                                        sys.exit(1)

                                output += fmt.format(tag=tag, field=field, sep=sep, val='(\'' + item + '\')')

    if 'delta' in new_task.keys():
        if fieldsEquivalent(new_task, old_task, 'wait') and (new_task['delta'] >= 0):
            new_task['wait'] = get_new_wait(new_task)
            out = "'delta' Field Exists => task[wait] = task[due] - {}d\n".format(int(new_task['delta']))
            print(out)
    else:
        new_task = _set_delta(new_task)

    # Set 'Misc' project if no other project is set
    if 'project' not in new_task.keys():
        new_task['project'] = 'Misc'
        out = "Default Project Set => Misc\n"
        print(out)

    if output != header:
        output += '   \n'  # Spaces Needed
        print(output)

    return new_task


def _set_delta(new_task):
    """ Sets 'task[delta]' to Integer Difference of 'task[due]' and 'task[wait]' """
    if fieldEquals(new_task, 'repeat', 'yes'):
        if 'wait' in new_task.keys():
            tdelta = parse(new_task['due']) - parse(new_task['wait'])
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

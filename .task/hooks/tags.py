import sys
from dateutil.parser import parse

import defaults
from dates import get_new_wait


def hasTag(task, tag):
    return ('tags' in task.keys()) and (tag in task['tags'])


def compareFields(taskA, taskB, field):
    return (field not in set(taskA.keys()) & set(taskB.keys())) or (taskA[field] == taskB[field])


def fieldEquals(task, field, value):
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
                                except KeyError as e:
                                    error_fmt = "The '{field}:' field must be defined when using the '+{tag}' tag."
                                    print(error_fmt.format(field=value.field, tag=tag))
                                    sys.exit(1)
                            else:
                                new_task[field] = value

                            output += fmt.format(tag=tag, field=field, sep=':', val=new_task[field])

                        if isinstance(value, defaults.ModList):
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

    if 'wait_delta' in new_task.keys():
        if compareFields(new_task, old_task, 'wait') and (new_task['wait_delta'] >= 0):
            new_task['wait'] = get_new_wait(new_task)
    else:
        new_task = _wait_delta_check(new_task)

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


def add_hook(new_task):
    """ Ran by Add Hook Only Must be run after process functions. """
    new_task = _wait_delta_check(new_task)
    return new_task


def _wait_delta_check(new_task):
    if fieldEquals(new_task, 'repeat', 'yes'):
        if 'wait' in new_task.keys():
            delta = parse(new_task['due']) - parse(new_task['wait'])
            if delta.days < 0:
                new_task['wait_delta'] = -1
                new_task.pop('wait')
                print('Negative wait removed.')
            else:
                new_task['wait_delta'] = delta.days
                print('[repeat.wait_delta] set to {} for repeating task.'.format(delta.days))
        else:
            new_task['wait_delta'] = -1

    return new_task

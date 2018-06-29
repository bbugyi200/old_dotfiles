"""Functions Relating to Task Tags"""

import datetime as dt
import subprocess as sp
import sys

import dates
import defaults

import gutils


def run(new_task, old_task=None):
    new_task = _process_del_tags(new_task, old_task)
    new_task = _process_add_tags(new_task, old_task)
    return new_task


def hasTag(task, tag):
    """True if tags field of @task contains @tag."""
    return ('tags' in task.keys()) and (tag in task['tags'])


def isDone(task):
    """True if task is completed."""
    return task['status'].lower() == 'completed'


def _process_del_tags(new_task, old_task=None):
    """Remove default task values when special tag is removed from task"""
    if old_task is None:
        return new_task

    header = ' \n======= Custom Tag Removed =======\n'
    output = header
    if 'tags' not in old_task.keys():
        return new_task

    for tag in defaults.tags.keys():
        if (tag in old_task['tags']) and (('tags' not in new_task.keys()) or (tag not in new_task['tags'])):
            for field, value in defaults.tags[tag].items():
                try:
                    if new_task[field] == old_task[field]:
                        fmt = "-{tag} => {field}:\n"
                        output += fmt.format(tag=tag, field=field)

                        if isinstance(value, defaults.Wrap):
                            new_task[field] = new_task[field].replace(value.start, '')
                            new_task[field] = new_task[field].replace(value.end, '')
                        else:
                            new_task.pop(field)
                except KeyError:
                    pass

    if output != header:
        output += '   \n'  # Spaces Needed
        print(output)

    return new_task


def _process_add_tags(new_task, old_task=None):
    """Add default task settings when special tag is added to task"""
    header = ' \n======= Custom Tag Added =======\n'
    output = header

    if old_task is None:
        old_task = dict()

    if 'tags' in new_task.keys():
        fmt = "+{tag} => {field}{sep}{val}\n"
        for tag in defaults.tags.keys():
            if tag in new_task['tags'] and (('tags' not in old_task.keys()) or (tag not in old_task['tags'])):

                if tag in defaults.repeats.keys():
                    output += "+{tag} recurrence has been set.\n".format(tag=tag)

                for field, value in defaults.tags[tag].items():
                    if value is None:
                        try:
                            new_task.pop(field)
                        except KeyError:
                            pass
                    elif field not in new_task.keys() or field in defaults.force_update:
                        if isinstance(value, defaults.FieldRef):
                            try:
                                new_task[field] = new_task[value.field]
                                output += fmt.format(tag=tag, field=field,
                                                     sep=' = ', val='task[{}]'.format(value.field))
                            except KeyError:
                                error_fmt = "The '{field}:' field must be defined when using the '+{tag}' tag."
                                print(error_fmt.format(field=value.field, tag=tag))
                                sys.exit(1)
                        elif isinstance(value, defaults.ModList):
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
                                    except ValueError:
                                        error_fmt = "Cannot remove '{item}'! '{item}' is not in '{field}'!"
                                        print(error_fmt.format(item=item, field=field))
                                        sys.exit(1)

                                output += fmt.format(tag=tag, field=field, sep=sep, val='(\'' + item + '\')')
                        elif isinstance(value, defaults.Notify):
                            gutils.notify(value.msg)
                        elif isinstance(value, defaults.Wrap):
                            new_task[field] = value.start + new_task[field] + value.end
                            val = "(start='%s', end='%s')" % (value.start, value.end)
                            output += fmt.format(tag=tag, field=field, sep='.wrap', val=val)
                        else:
                            new_task[field] = value
                            output += fmt.format(tag=tag, field=field, sep=':', val=new_task[field])

    # Medium priority should not exist
    if ('priority' in new_task.keys()) and (new_task['priority'] == 'M'):
        new_task.pop('priority')

    if 'delta' in new_task.keys():
        if _fieldsEquiv(new_task, old_task, 'wait') and (new_task['delta'] >= 0):
            new_task['wait'] = dates.get_new_wait(new_task)
            out = "'delta' Field Exists => task[wait] = task[due] - {}d\n".format(int(new_task['delta']))
            print(out)
    else:
        new_task = _set_delta(new_task)

    # Don't let task be created without project
    if 'project' not in new_task.keys():
        out = sp.check_output(['prompt', 'Select a Project'])
        new_task['project'] = out.decode().strip()

    if output != header:
        output += '   \n'  # Spaces Needed
        print(output)

    return new_task


def _fieldsEquiv(taskA, taskB, field):
    """Compares the Given Field of two Tasks

    Returns True if the fields are equal.
    """
    c1 = (field in set(taskA.keys()) & set(taskB.keys())) and (taskA[field] == taskB[field])
    c2 = field not in set(taskA.keys()) | set(taskB.keys())

    return any([c1, c2])


def _fieldEquals(task, field, value):
    """True if 'task[field]' equals 'value'"""
    return (field in task.keys()) and (task[field] == value)


def _set_delta(new_task):
    """Sets 'task[delta]' to Integer Difference of 'task[due]' and 'task[wait]'"""
    if _fieldEquals(new_task, 'repeat', 'yes'):
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

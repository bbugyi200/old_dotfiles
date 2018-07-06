"""Hooks and functions related to custom tags"""

from hooks.custom import tags
from hooks.custom import fields
from hooks.utils import log

logger = log.getLogger()


def run(new_task, old_task=None):
    log.running(logger)
    new_task = _process_del_tags(new_task, old_task)
    new_task = _process_add_tags(new_task, old_task)
    return new_task


def _process_del_tags(new_task, old_task=None):
    """Remove default task values when special tag is removed from task"""
    if old_task is None:
        return new_task

    header = ' \n======= Custom Tag Removed =======\n'
    output = header
    if 'tags' not in old_task.keys():
        return new_task

    for tag in tags.tags.keys():
        if (tag in old_task['tags']) and (('tags' not in new_task.keys()) or (tag not in new_task['tags'])):
            for field, value in tags.tags[tag].items():
                try:
                    if new_task[field] == old_task[field]:
                        fmt = "-{tag} => {field}:\n"
                        output += fmt.format(tag=tag, field=field)

                        if isinstance(value, fields.Field):
                            new_task = value.pop(new_task, tag, field)
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
        for tag in tags.tags.keys():
            if tag in new_task['tags'] and (('tags' not in old_task.keys()) or (tag not in old_task['tags'])):

                if tag in tags.repeats.keys():
                    output += "+{tag} recurrence has been set.\n".format(tag=tag)

                for field, value in tags.tags[tag].items():
                    if value is None:
                        try:
                            new_task.pop(field)
                        except KeyError:
                            pass
                    elif field not in new_task.keys() or field in tags.force_update:
                        if isinstance(value, fields.Field):
                            new_task = value.add(new_task, tag, field)
                            output += value.msg
                        else:
                            new_task[field] = value
                            output += fields.msg_fmt.format(tag=tag, field=field, sep=':', val=new_task[field])

    if output != header:
        output += '   \n'  # Spaces Needed
        print(output)

    return new_task

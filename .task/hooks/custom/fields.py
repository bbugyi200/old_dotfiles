"""Custom Field Classes

Attributes:
    msg_fmt (str): Format string used to output messages to user.
"""

from abc import ABCMeta, abstractmethod

import gutils
from hooks.utils import log

logger = log.getLogger()
msg_fmt = "+{tag} => {field}{sep}{val}\n"


class FieldError(RuntimeError):
    """Generic exception raised by custom Field objects"""


class Field(metaclass=ABCMeta):
    """Abstract Field Class

    Attributes:
        msg (str): This message will be appended to the output displayed to the user after
            custom tag actions have been completed.
    """
    def __init__(self):
        self.msg = ''

    @abstractmethod
    def add(self, task, tag, field):
        """Processes and returns task dict when corresponding special tag is added."""

    def pop(self, task, tag, field):
        """Processes and returns task dict when corresponding special tag is removed."""
        return task


class Ref(Field):
    """Field Reference

    References one of a Task's fields. Setting <foo> field's default to `FieldRef(<bar>)` is
    equivalent to `task add ... foo:bar ...`.

    You can also set the <foo> field's default to `FieldRef(<foo>)` to enforce a mandatory
    field.
    """
    def __init__(self, field):
        super().__init__()
        self.field = field

    def add(self, task, tag, field):
        try:
            task[field] = task[self.field]
            self.msg += msg_fmt.format(tag=tag, field=field,
                                       sep=' = ', val='task[{}]'.format(self.field))
        except KeyError:
            error_fmt = "The '{field}:' field must be defined when using the '+{tag}' tag."
            raise FieldError(error_fmt.format(field=self.field, tag=tag))

        return task


class ModTags(Field):
    """Modify Tags List

    Used to add/remove a tag from the 'tags' field.
    """
    mode_opts = ['+', '-']

    def __init__(self, items, modes):
        super().__init__()
        if not isinstance(items, str) and not isinstance(modes, str):
            self.items = items
            self.modes = modes
        else:
            self.items = (items, )
            self.modes = (modes, )

        if not all(mode in self.mode_opts for mode in modes):
            raise FieldError("Mode must be one of the following!: {}", self.mode_opts)

    def add(self, task, tag, field):
        for item, mode in zip(self.items, self.modes):
            if mode == '+' and item not in task[field]:
                if field not in task.keys():
                    task[field] = []
                task[field].append(item)
                sep = '.append'
            elif mode == '-':
                try:
                    task[field].remove(item)
                    sep = '.remove'
                except ValueError:
                    error_fmt = "Cannot remove '{item}'! '{item}' is not in '{field}'!"
                    raise FieldError(error_fmt.format(item=item, field=field))

            self.msg += msg_fmt.format(tag=tag, field=field, sep=sep, val='(\'' + item + '\')')

        return task


class Notify(Field):
    """Send Notification

    Use @msg to send a desktop notification.
    """
    def __init__(self, notification_msg):
        super().__init__()
        self.notification_msg = notification_msg

    def add(self, task, tag, field):
        gutils.notify(self.notification_msg, title='TaskWarrior Hook')
        return task


class Wrap(Field):
    """Wrap Field

    Wraps string field with @start and @end.
    """
    def __init__(self, *, start, end):
        super().__init__()
        self.start = start
        self.end = end

    def add(self, task, tag, field):
        task[field] = self.start + task[field] + self.end
        val = "(start='%s', end='%s')" % (self.start, self.end)
        self.msg += msg_fmt.format(tag=tag, field=field, sep='.wrap', val=val)
        return task

    def pop(self, task, tag, field):
        task[field] = task[field].replace(self.start, '')
        task[field] = task[field].replace(self.end, '')
        return task

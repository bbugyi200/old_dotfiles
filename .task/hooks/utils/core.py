"""Core TaskWarrior Hook Utilities

This module's contents are imported directly into the utils package's global namespace. Any
classes, functions, or variables defined here MUST be included in __all__ or they will NOT be
made available.
"""

__all__ = ['has_tag', 'is_done', 'fields_equiv', 'field_equals']


def has_tag(task, tag):
    """True if tags field of @task contains @tag."""
    return ('tags' in task.keys()) and (tag in task['tags'])


def is_done(task):
    """True if task is completed."""
    return task['status'].lower() == 'completed'


def fields_equiv(taskA, taskB, field):
    """Compares the Given Field of two Tasks

    Returns True if the fields are equal.
    """
    if taskA is None or taskB is None:
        return True

    c1 = (field in set(taskA.keys()) & set(taskB.keys())) and (taskA[field] == taskB[field])
    c2 = field not in set(taskA.keys()) | set(taskB.keys())

    return any([c1, c2])


def field_equals(task, field, value):
    """True if 'task[field]' equals 'value'"""
    return (field in task.keys()) and (task[field] == value)

"""Core TaskWarrior Hook Utilities

This module's contents are imported directly into the utils package's global namespace. Any
classes, functions, or variables defined here MUST be included in __all__ or they will NOT be
made available.
"""

__all__ = ['has_tag', 'is_done']


def has_tag(task, tag):
    """True if tags field of @task contains @tag."""
    return ('tags' in task.keys()) and (tag in task['tags'])


def is_done(task):
    """True if task is completed."""
    return task['status'].lower() == 'completed'

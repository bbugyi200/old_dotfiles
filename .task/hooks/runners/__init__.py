"""Runnable Hooks

Every module (hook) in this package has a run() method which executes that hook.

Dynamic imports are used in _add_run and _modify_run to ensure that the logger is
initialized globally.
"""

from hooks.utils import log


def run(new_task, old_task=None):
    log.init_logger()
    if old_task is None:
        return _add_run(new_task)
    else:
        return _modify_run(new_task, old_task)


def _add_run(new_task):
    """Run all on-add hooks"""
    from hooks.runners import github
    from hooks.runners import tags

    new_task = tags.run(new_task)
    new_task = github.run(new_task)
    return new_task


def _modify_run(new_task, old_task):
    """Run all on-modify hooks"""
    from hooks.runners import active
    from hooks.runners import repeats
    from hooks.runners import github
    from hooks.runners import tags

    new_task = tags.run(new_task, old_task)
    new_task = github.run(new_task, old_task)
    new_task = active.run(new_task, old_task)
    new_task = repeats.run(new_task, old_task)
    return new_task

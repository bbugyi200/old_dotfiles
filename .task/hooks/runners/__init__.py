"""Runnable Hooks

Every module (hook) in this package has a run() method which executes that hook.
"""

from hooks.utils import log


def run(*task_dicts):
    """Runs all Runners.

    Dynamic imports are used to ensure that the logger is initialized globally.

    Args:
        task_dicts: Either (new_task,) or (new_task, old_task).
    """
    log.init_logger()

    from hooks.runners import active
    from hooks.runners import github
    from hooks.runners import repeats
    from hooks.runners import tags

    new_task = tags.run(*task_dicts)
    new_task = github.run(*task_dicts)
    new_task = active.run(*task_dicts)
    new_task = repeats.run(*task_dicts)
    return new_task

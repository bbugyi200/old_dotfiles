"""Main entry point for TaskWarrior Hooks

Dynamic imports are used to ensure that the logger is initialized globally.
"""

import utils.log


def run(new_task, old_task=None):
    utils.log.init_logger()
    if old_task is None:
        return _add_run(new_task)
    else:
        return _modify_run(new_task, old_task)


def _add_run(new_task):
    """Run all on-add hooks"""
    import github
    import tags

    new_task = tags.run(new_task)
    new_task = github.run(new_task)
    return new_task


def _modify_run(new_task, old_task):
    """Run all on-modify hooks"""
    import active
    import repeats
    import github
    import tags

    new_task = tags.run(new_task, old_task)
    new_task = github.run(new_task, old_task)
    new_task = active.run(new_task, old_task)
    new_task = repeats.run(new_task, old_task)
    return new_task

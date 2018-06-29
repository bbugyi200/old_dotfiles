"""Main entry point for TaskWarrior Hooks"""

import active
import repeats
import github
import tags


def add_run(new_task):
    """Run all on-add hooks"""
    new_task = tags.run(new_task)
    new_task = github.run(new_task)
    return new_task


def modify_run(new_task, old_task):
    """Run all on-modify hooks"""
    new_task = tags.run(new_task, old_task)
    new_task = github.run(new_task, old_task)
    new_task = active.run(new_task, old_task)
    new_task = repeats.run(new_task, old_task)
    return new_task

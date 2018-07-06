"""Validation Hooks"""

import subprocess as sp


def run(new_task, old_task=None):
    """Run Validation Hook.

    Performs the following actions:
        - Remove 'M' priority. This priority is not supported.
        - Prompt the user for a project name if none was given.
    """
    # Medium priority should not exist
    if ('priority' in new_task.keys()) and (new_task['priority'] == 'M'):
        new_task.pop('priority')

    # Don't let task be created without project
    if 'project' not in new_task.keys():
        out = sp.check_output(['prompt', 'Select a Project'])
        new_task['project'] = out.decode().strip()

    return new_task

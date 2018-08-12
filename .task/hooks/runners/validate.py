"""Validation Hooks"""

import subprocess as sp

from hooks.utils import log

logger = log.getLogger()


def run(new_task, old_task=None):
    """Run Validation Hook.

    Performs the following actions:
        - Remove 'M' priority. This priority is not supported.
        - Prompt the user for a project name if none was given.
        - If due/wait time is midnight, change to 6AM.
    """
    # Medium priority should not exist
    if ('priority' in new_task.keys()) and (new_task['priority'] == 'M'):
        new_task.pop('priority')

    # Don't let task be created without project
    if 'project' not in new_task.keys():
        out = sp.check_output(['prompt', 'Select a Project'])
        project = out.decode().strip()

        if project == '':
            project = 'Misc'

        new_task['project'] = project

    for tag in ['due', 'wait']:
        if tag in new_task.keys() and new_task[tag][-7:] == '040000Z':
            logger.debug('Setting {} time for 6AM.'.format(tag))
            new_task[tag] = new_task[tag][:-7] + '100000Z'

    return new_task

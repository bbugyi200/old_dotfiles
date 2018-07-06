"""Active (start/stop) Hooks"""

import subprocess as sp

from hooks import utils
from hooks.utils import log

logger = log.getLogger()


def run(new_task, old_task=None):
    """Run Active Hook.

    This hook contains functions that are run when a task is start and/or stopped. This includes:
       - Starting/stopping TimeWarrior
       - Starting xtaskidle
       - Writing last task UUID to file (used by 'last_task' script).
    """
    if old_task is None:
        return new_task

    # Extract attributes for use as tags.
    log.running(logger)
    timew_tags = []

    if 'project' in new_task:
        project = new_task['project']
        timew_tags.append(project)
        while '.' in project:
            project = project[:project.rfind('.')]
            timew_tags.append(project)

    combined_timew_tags = ' '.join(['"%s"' % tag for tag in timew_tags]).encode('utf-8').strip()

    # Started task.
    if 'start' in new_task and 'start' not in old_task:
        logger.debug('Task Started: {}'.format(new_task['description']))
        cmd = 'timew start "[Project: {project}]" {tags}'
        sp.check_call(cmd.format(project=new_task['description'], tags=combined_timew_tags.decode()),
                shell=True, stdout=sp.DEVNULL, stderr=sp.STDOUT)

        sp.Popen('xtaskidle -d', shell=True, stdout=sp.DEVNULL, stderr=sp.STDOUT)

    # Stopped task.
    elif ('start' not in new_task) and ('start' in old_task):
        logger.debug('Task Stopped: {}'.format(new_task['description']))
        sp.Popen('timew stop', shell=True, stdout=sp.DEVNULL, stderr=sp.STDOUT)
        if not utils.is_done(new_task):
            with open('/home/bryan/.task/.last_task', 'w') as f:
                f.write(new_task['uuid'])

    return new_task

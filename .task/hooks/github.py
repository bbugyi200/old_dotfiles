"""Hooks related to GitHub Issues"""

import subprocess as sp
import re

import utils
from utils.log import logger as log


def run(new_task, old_task=None):
    if _is_issue(new_task):
        new_task = _depends(new_task)
        if old_task is not None and utils.is_done(new_task) and not utils.is_done(old_task):
            _close_issue(new_task)
    return new_task


def _is_issue(task):
    """True if @task corresponds to a GitHub issue."""
    return 'githubrepo' in task


def _close_issue(task):
    """Closes corresponding GitHub Issue (if one exists)."""
    log.debug('{} corresponds to task: {}'.format(task['githubrepo'], task['description']))

    gh_issue_number = str(int(float(task['githubnumber'])))
    cmd_list = ['ghi', 'close', gh_issue_number, '--', task['githubrepo']]
    log.debug('Running command: {}'.format(cmd_list))

    ps = sp.Popen(cmd_list, stdout=sp.PIPE, stderr=sp.STDOUT)
    out = ps.communicate()[0]

    log.debug('ghi command failed. Error output: {}'.format(out.decode().strip()))


def _depends(task):
    """Integrates taskwarrior depends field with GitHub issues.

    If the GitHub issue corresponding to @task contains a comment of the form "Depends: #[N]" then
    the 'depends' field of @task is set accordingly.

    Args:
        task (dict): mapping of task fields to task field values.

    Returns:
        New task dictionary.
    """
    if 'annotations' not in task:
        log.debug('Task has no annotations, so cannot have a "Depends" comment.')
        return task

    issue_number = None
    for annotation in task['annotations']:
        description = annotation['description'].lower()
        if 'depends:' not in description:
            continue

        match = re.search(r'depends:\s*#([0-9]+)', description)
        if not match:
            log.debug('Found "Depends:" but regex match failed.')
            log.vdebug('Comment: %s', repr(description))
            return task
        issue_number = int(match.groups()[0])
        break

    if issue_number is None:
        log.debug('No dependency found on GitHub issue.')
        return task

    log.debug('Task depends on issue #{}.'.format(issue_number))
    gh_repo = task['githubrepo'].split('/')[1]
    log.debug('GitHub Repo: %s', repr(gh_repo))

    cmd_list = ['task', 'rc.context:none', 'uuids']
    cmd_list.extend(['project:Dev.{}'.format(gh_repo)])
    cmd_list.extend(['/#{}:/'.format(issue_number)])
    log.vdebug('Command List: %s', repr(cmd_list))

    ps = sp.Popen(cmd_list, stdout=sp.PIPE)
    uuid = ps.communicate()[0].decode().strip()

    if uuid == '':
        log.debug('Attempt to find UUID failed. Empty string returned by task command.')
        return task
    else:
        log.debug('UUID of issue #{}: {}'.format(issue_number, uuid))

    new_task = task.copy()

    if 'depends' in new_task:
        dependencies = new_task['depends'].split(',')
    else:
        dependencies = []

    if uuid not in dependencies:
        log.debug('Dependency not found. Adding...')
        dependencies.append(uuid)
        new_task['depends'] = ','.join(dependencies)
    else:
        log.debug('Dependency is already present.')

    log.vdebug('New Task (after adding dependency): %s', repr(new_task))
    return new_task

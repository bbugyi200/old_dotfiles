"""Functions Relating to Logging"""

import getpass
import inspect
import logging
import os
import sys

sys.path.insert(0, '/home/{}/Sync/scripts/pymodules'.format(getpass.getuser()))

import gutils  # noqa

gutils.logging.add_vdebug_level(logging)
_STREAM_LOGGING_LEVEL = logging.INFO  # THIS VALUE CAN BE CHANGED TO ENABLE DEBUGGING OUTPUT


def init_logger():
    """Initialize the logger"""
    root = logging.getLogger()

    if _STREAM_LOGGING_LEVEL == logging.INFO:
        max_level = logging.DEBUG
    else:
        max_level = _STREAM_LOGGING_LEVEL

    fh = logging.FileHandler('/var/tmp/taskwarrior-hooks.log')
    sh = logging.StreamHandler()

    root.setLevel(max_level)
    fh.setLevel(max_level)
    sh.setLevel(_STREAM_LOGGING_LEVEL)

    formatter = logging.Formatter('[%(process)d] (%(asctime)s) <%(name)s> [%(levelname)s] %(message)s',
                                  datefmt='%Y-%m-%d %H:%M:%S')
    fh.setFormatter(formatter)
    sh.setFormatter(formatter)

    root.addHandler(fh)
    root.addHandler(sh)


def running(logger):
    """Logs hook startup message"""
    logger.debug("Running Hook...")


def getLogger():
    """Returns custom logger based on calling module's name"""
    module_path = inspect.stack()[1].filename.rstrip('.py')
    module_basename = os.path.basename(module_path)
    return logging.getLogger(module_basename)

"""Sets up logging.

Attributes:
    logger: log.Logger object used for logging taskwarrior hooks.
"""

import inspect
import logging
import os

import gutils

gutils.logging.add_vdebug_level(logging)
_STREAM_LOGGING_LEVEL = logging.INFO


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
    logger.debug("Running Hooks...")


def getLogger():
    """Returns custom logger based on calling module's name"""
    module_path = inspect.stack()[1].filename.rstrip('.py')
    module_basename = os.path.basename(module_path)
    return logging.getLogger(module_basename)

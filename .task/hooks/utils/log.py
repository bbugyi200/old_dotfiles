"""Sets up logging.

Attributes:
    logger: log.Logger object used for logging taskwarrior hooks.
"""

import logging

import gutils

gutils.logging.add_vdebug_level(logging)
_STREAM_LOGGING_LEVEL = logging.VDEBUG
logger = logging.getLogger("twhooks")


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

    formatter = gutils.logging.getFormatter(verbose=True)
    fh.setFormatter(formatter)
    sh.setFormatter(formatter)

    root.addHandler(fh)
    root.addHandler(sh)

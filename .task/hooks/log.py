import logging

import gutils

logger = logging.getLogger(__name__)
_fh = logging.FileHandler('/var/tmp/taskwarrior-hooks.log')

logger.setLevel(logging.DEBUG)
_fh.setLevel(logging.DEBUG)

formatter = gutils.logging.getFormatter(verbose=True)
_fh.setFormatter(formatter)

logger.addHandler(_fh)

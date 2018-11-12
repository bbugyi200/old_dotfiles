#!/usr/bin/env python

{% START INSERT MODE %}

import datetime as dt  # noqa: F401
import os  # noqa: F401
import subprocess as sp  # noqa: F401
import sys  # noqa: F401

import gutils

############################################################################################
#  gutils library: https://github.com/bbugyi200/scripts/tree/master/modules/python/gutils  #
############################################################################################

log = gutils.logging.getEasyLogger(__name__)
scriptname = os.path.basename(os.path.realpath(__file__))


def main(args):
    pass


if __name__ == "__main__":
    parser = gutils.ArgumentParser()
    args = parser.parse_args()

    with gutils.logging.context(log, debug=args.debug, verbose=args.verbose):
        main(args)

#!/usr/bin/env python3

{% INSERT %}

import argparse  # pylint: disable=unused-import
import datetime as dt  # pylint: disable=unused-import
import os  # pylint: disable=unused-import
from pathlib import Path  # pylint: disable=unused-import
import subprocess as sp  # pylint: disable=unused-import
import sys  # pylint: disable=unused-import
import typing as Type  # pylint: disable=unused-import

import gutils
from loguru import logger as log  # noqa


scriptname = os.path.basename(os.path.realpath(__file__))


@gutils.catch
def main() -> None:
    args = parse_cli_args()
    gutils.logging.configure(__file__, debug=args.debug, verbose=args.verbose)


def parse_cli_args() -> argparse.Namespace:
    parser = gutils.ArgumentParser()
    return parser.parse_args()


if __name__ == "__main__":
    main()

#!/usr/bin/env python3

{% INSERT %}

import argparse  # noqa: F401
import datetime as dt  # noqa: F401
import os  # noqa: F401
from pathlib import Path  # noqa: F401
import subprocess as sp  # noqa: F401
import sys  # noqa: F401
from typing import (  # noqa: F401
    Any,
    Callable,
    Container,
    Dict,
    Generator,
    Iterable,
    Iterator,
    List,
    NoReturn,
    Optional,
    Sequence,
    Set,
    Tuple,
    TypeVar,
    Union,
)

import gutils
from loguru import logger as log


scriptname = os.path.basename(os.path.realpath(__file__))


@log.crash
def main() -> None:
    args = parse_cli_args()
    gutils.logging.configure(__file__, debug=args.debug, verbose=args.verbose)


def parse_cli_args() -> argparse.Namespace:
    parser = gutils.ArgumentParser()
    return parser.parse_args()


if __name__ == "__main__":
    main()

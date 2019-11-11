#!/usr/bin/env python3

{% INSERT %}

import argparse
import sys
from typing import List

from loguru import logger as log  # pylint: disable=unused-import

import gutils


@gutils.catch
def main(argv: List[str] = None) -> int:
    if argv is None:
        argv = sys.argv

    args = parse_cli_args(argv)
    gutils.logging.configure(__file__, debug=args.debug, verbose=args.verbose)

    return 0


def parse_cli_args(argv: List[str]) -> argparse.Namespace:
    parser = gutils.ArgumentParser()
    return parser.parse_args(argv[1:])


if __name__ == "__main__":
    sys.exit(main())

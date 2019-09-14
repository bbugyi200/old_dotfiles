#!/usr/bin/env python3

{% INSERT %}

import argparse

from loguru import logger as log  # pylint: disable=unused-import

import gutils


@gutils.catch
def main() -> None:
    args = parse_cli_args()
    gutils.logging.configure(__file__, debug=args.debug, verbose=args.verbose)


def parse_cli_args() -> argparse.Namespace:
    parser = gutils.ArgumentParser()
    return parser.parse_args()


if __name__ == "__main__":
    main()

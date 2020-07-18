{% INSERT %}

import sys
from typing import NamedTuple, Sequence

from loguru import logger as log  # pylint: disable=unused-import

import gutils


@gutils.catch
def main(argv: Sequence[str] = None) -> int:
    if argv is None:
        argv = sys.argv

    args = parse_cli_args(argv)
    gutils.logging.configure(__file__, debug=args.debug, verbose=args.verbose)

    return run(args)


class Arguments(NamedTuple):
    debug: bool
    verbose: bool


def parse_cli_args(argv: Sequence[str]) -> Arguments:
    parser = gutils.ArgumentParser()

    args = parser.parse_args(argv[1:])

    kwargs = dict(args._get_kwargs())
    return Arguments(**kwargs)


def run(args: Arguments) -> int:
    return 0


if __name__ == "__main__":
    sys.exit(main())

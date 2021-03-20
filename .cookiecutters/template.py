{% INSERT %}

import sys
from typing import NamedTuple, Sequence

from bugyi.core import ArgumentParser, main_factory
from loguru import logger as log  # pylint: disable=unused-import


class Arguments(NamedTuple):
    debug: bool
    verbose: bool


def parse_cli_args(argv: Sequence[str]) -> Arguments:
    parser = ArgumentParser()

    args = parser.parse_args(argv[1:])

    kwargs = dict(args._get_kwargs())
    return Arguments(**kwargs)


def run(args: Arguments) -> int:
    return 0


main = main_factory(parse_cli_args, run)
if __name__ == "__main__":
    sys.exit(main())

{% INSERT %}

from dataclasses import dataclass
import sys
from typing import Sequence

from bugyi import cli
from bugyi.core import main_factory
from loguru import logger as log  # pylint: disable=unused-import


@dataclass(frozen=True)
class Arguments(cli.Arguments):
    pass


def parse_cli_args(argv: Sequence[str]) -> Arguments:
    parser = cli.ArgumentParser()

    args = parser.parse_args(argv[1:])

    kwargs = dict(args._get_kwargs())
    return Arguments(**kwargs)


def run(args: Arguments) -> int:
    return 0


main = main_factory(parse_cli_args, run)
if __name__ == "__main__":
    sys.exit(main())

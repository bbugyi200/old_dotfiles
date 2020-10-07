import argparse
import sys
from typing import NamedTuple, Sequence  # pylint: disable=unused-import


def main(argv=None):
    # type: (Sequence[str]) -> int
    if argv is None:
        argv = sys.argv

    args = parse_cli_args(argv)

    return 0


Arguments = NamedTuple("Arguments", [])


def parse_cli_args(argv):
    # type: (Sequence[str]) -> Arguments
    parser = argparse.ArgumentParser(description=__doc__)

    args = parser.parse_args(argv[1:])
    kwargs = dict(args._get_kwargs())

    return Arguments(**kwargs)


if __name__ == "__main__":
    sys.exit(main())

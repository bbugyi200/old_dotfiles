"""(E)dgestream (V)irtual (E)nvironments

This script is used to manage Edgestream's Python dependencies as well as all
requirements.txt files and/or virtual environments that correspond to those
dependencies.
"""

from __future__ import print_function

import argparse
import logging
import platform
import sys
from typing import (  # pylint: disable=unused-import
    Any,
    Callable,
    List,
    MutableMapping,
    NamedTuple,
    Optional,
    Sequence,
    Union,
)

from prod_common import git_tools as git
from prod_common.epathlib import Path
from prod_common.errors import EResult  # pylint: disable=unused-import
from prod_common.errors import Err
from prod_common.etypes import Unicode  # pylint: disable=unused-import
from util.eve import freeze, init


logger = logging.getLogger(__name__)


def main(argv=None):
    # type: (Sequence[str]) -> int
    if argv is None:
        argv = sys.argv

    configure_logging(debug="-d" in argv or "--debug" in argv)
    args = parse_cli_args(argv)
    logger.debug("Args: %r", args)

    if isinstance(args, FreezeArguments):
        return run_freeze(args)
    else:
        assert isinstance(args, InitArguments)
        return run_init(args)


def configure_logging(debug=False):
    # type: (bool) -> None
    sh = logging.StreamHandler()
    sh.formatter = logging.Formatter(
        "%(asctime)s.%(msecs)-3d  |  %(levelname)-7s  |  %(message)s "
        " [%(module)s::%(funcName)s::%(lineno)d]",
        datefmt="%H:%M:%S",
    )

    root = logging.root
    root.setLevel("DEBUG" if debug else "INFO")
    root.addHandler(sh)

    if debug:
        logger.debug("Debug Mode Enabled")


FreezeArguments = NamedTuple(
    "FreezeArguments",
    [
        ### Global Options
        ("config_dir", Path),
        ("debug", bool),
        ### Freeze Options
        ("check", bool),
        ("jobs", Optional[int]),
    ],
)
InitArguments = NamedTuple(
    "InitArguments",
    [
        ### Global Options
        ("config_dir", Path),
        ("debug", bool),
        ### Init Options
        ("infile_names", List[str]),
        ("venv_dirs", List[init.VenvDir]),
        ("force", bool),
        ("warnings", bool),
    ],
)
Arguments = Union[FreezeArguments, InitArguments]


def parse_cli_args(argv):
    # type: (Sequence[str]) -> Arguments
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "-c",
        "--config-dir",
        default=_default_config_dir(),
        type=Path,
        help=(
            "The directory which contains the files that eve uses (e.g. requirement"
            " files, 'scripts' directory, etc....). Defaults to %(default)s."
        ),
    )
    parser.add_argument("-d", "--debug", action="store_true", help="Enable debug mode.")
    subparsers = parser.add_subparsers(dest="command")

    FREEZE_DESC = (
        "Generate frozen requirements.txt files for every possible combination of the"
        " 'init' sub-command's --extra options (i.e. for every possible combination of"
        " requirements.in files found in the <EVE>/requirements directory)."
    )
    freeze_parser = subparsers.add_parser(
        "freeze", description=FREEZE_DESC, help=FREEZE_DESC
    )
    freeze_parser.add_argument(
        "-j",
        "--jobs",
        metavar="N",
        type=_jobs_type,
        default=None,
        help=(
            "Use N multiple processes to speed up the compilation of requirement.in"
            " files. Defaults to using as many processes as needed without using more"
            " processes than this machine has CPUs."
        ),
    )
    freeze_parser.add_argument(
        "--check", action="store_true", help="Check if 'eve freeze' needs to be run."
    )

    INIT_DESC = (
        "Initialize the python virtual environment and then print the full"
        " path of the resultant venv directory to STDOUT. Note that we attempt"
        " to reuse an existing, equivalent virtual environment when possible"
        " (i.e. we create a new virtual environment ONLY when necessary)."
    )
    init_parser = subparsers.add_parser("init", description=INIT_DESC, help=INIT_DESC)
    init_parser.add_argument(
        "-D",
        "--venv-dir",
        metavar="VENV_DIR",
        dest="venv_dirs",
        action="append",
        help=(
            "Attempt to initialize the virtual environment as a subdirectory of"
            " VENV_DIR. This option can be used multiple times, in which case we treat"
            " the additional provided directories as backups in-case there is a problem"
            " with the first directory that was provided. NOTE: You can specify that"
            " the venv created in this directory be made read-only by prepending the"
            " directory with 'ro:'."
        ),
    )
    init_parser.add_argument(
        "-E",
        "--extra",
        dest="infile_names",
        metavar="EXTRA",
        action="append",
        help=(
            "Extra set of dependencies to install. The extra category 'foo' specified"
            " via '--extra foo' corresponds to the packages listed in the"
            " <EVE>/requirements/foo.in file, where <EVE> is the path of eve's"
            " configuration directory (specified via the --config-dir option). This"
            " option can be used multiple times. NOTE: You can preface EXTRA with"
            " 'opt:' to specify that the given package category is optional (e.g. eve"
            " will crash if given '--extra foo' when <EVE>/requirements/foo.in does not"
            " exist, but will just ignore this option if '--extra opt:foo' was given"
            " instead)."
        ),
    )
    init_parser.add_argument(
        "-f",
        "--force",
        action="store_true",
        help=(
            "Force the creation of a new venv directory (instead of using an existing"
            " one if possible). WARNING: This option exists primarily as a way to debug"
            " 'eve' and thus should NOT need to be used in most circumstances."
        ),
    )
    init_parser.add_argument(
        "-W",
        "--enable-warnings",
        dest="warnings",
        action="store_true",
        help=(
            "Display warning messages regarding any inconsistencies between the final"
            " venv and the frozen requirements.txt file that _should_ completely define"
            " that venv."
        ),
    )

    args = parser.parse_args(argv[1:])

    kwargs = dict(args._get_kwargs())

    command = kwargs["command"]
    del kwargs["command"]

    if command == "freeze":
        return FreezeArguments(**kwargs)
    else:
        assert command == "init"
        _init_hooks(kwargs, init_parser.error)
        return InitArguments(**kwargs)


def _jobs_type(jobs_str):
    # type: (str) -> int
    """Parses the 'eve freeze' command's --jobs option."""
    error = argparse.ArgumentTypeError(
        "The --jobs option expects a single positive integer argument: {}".format(
            jobs_str
        )
    )

    try:
        jobs = int(jobs_str)
    except ValueError:
        raise error

    if jobs <= 0:
        raise error

    return jobs


def _init_hooks(kwargs, parser_error):
    # type: (MutableMapping[str, Any], Callable[[str], None]) -> None
    """Hooks to parse and validate the 'eve init' command's options."""
    # >>> 'venv_dirs' hook
    if not kwargs["venv_dirs"]:
        parser_error(
            "At least one venv directory MUST be specified via the"
            " -D|--venv-dir option."
        )

    venv_dir_paths = kwargs["venv_dirs"]
    del kwargs["venv_dirs"]
    kwargs["venv_dirs"] = []

    for D in venv_dir_paths:
        read_only = False
        if D.lower().startswith("ro:"):
            # This venv directory will be made read-only.
            D = D[3:]
            read_only = True

        venv_dir = init.VenvDir(Path(D), read_only)
        kwargs["venv_dirs"].append(venv_dir)

    # >>> 'infile_names' hook
    # We validate that every --extra option corresponds to an existing
    # requirements.in file, unless that --extra option is of the form
    # opt:foo---in which case the foo.in file MAY or MAY NOT exist.
    if not kwargs["infile_names"]:
        kwargs["infile_names"] = []

    kwargs["infile_names"].append("base")
    for name in kwargs["infile_names"][:]:
        optional = False

        if name.startswith("opt:"):
            kwargs["infile_names"].remove(name)
            name = name[4:]
            kwargs["infile_names"].append(name)
            optional = True

        infile = Path(kwargs["config_dir"]) / "requirements/{}.in".format(name)
        if not infile.exists():
            if optional:
                kwargs["infile_names"].remove(name)
                continue

            if infile.name == "base.in":
                parser_error(
                    "The {} file MUST always exist since it specifies the base set of"
                    " packages that will always be installed.".format(infile),
                )
            else:
                parser_error(
                    "The {} file, which corresponds to the extra specified via '--extra"
                    " {}', does not exist.".format(infile, infile.stem)
                )

    # Remove duplicates and sort.
    kwargs["infile_names"] = sorted(set(kwargs["infile_names"]))


def _default_config_dir():
    # type: () -> Path
    """
    Returns:
        The default value for the --config-dir option.
    """
    top_level_dir_r = git.top_level_dir()
    if isinstance(top_level_dir_r, Err):
        top_level_dir = Path.cwd()
    else:
        top_level_dir = Path(top_level_dir_r.ok())

    return top_level_dir / ".eve"


def run_freeze(args):
    # type: (FreezeArguments) -> int
    """This function is called when eve's 'freeze' sub-command is used."""
    if args.check:
        if freeze.check(args.config_dir):
            return 0
        else:
            freeze.failed_check_error()
            return 1
    else:
        if platform.system() != "Linux":
            print(
                "[ERROR] The 'eve freeze' command MUST be run from a Linux machine."
                " Otherwise, the generated frozen.txt files may differ from those which"
                " would have been generated on a Linux machine.",
                file=sys.stderr,
            )
            return 1

        needed_freezing_r = freeze.freeze(args.config_dir, max_jobs=args.jobs)
        if isinstance(needed_freezing_r, Err):
            e = needed_freezing_r.err()
            logger.error(e.report())
            return 1

        needed_freezing = needed_freezing_r.ok()
        if needed_freezing:
            return 0
        else:
            print(
                "\nNo changes were made. All frozen requirements.txt files exist and"
                " are already accurate.",
            )
            return 3


def run_init(args):
    # type: (InitArguments) -> int
    """This function is called when eve's 'init' sub-command is used."""
    venv_path_r = init.get_venv_path(
        args.config_dir,
        args.venv_dirs,
        args.infile_names,
        force=args.force,
        debug=args.debug,
        warnings=args.warnings,
    )
    if isinstance(venv_path_r, Err):
        e = venv_path_r.err()
        logger.error(e.report())
        return 1

    venv_path = venv_path_r.ok()

    logger.debug("Eve has initialized the following venv path: %s", venv_path)
    print(venv_path)

    return 0


if __name__ == "__main__":
    sys.exit(main())

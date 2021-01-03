"""Functionality related to eve's 'freeze' sub-command."""

from __future__ import print_function

import itertools as it
import logging
import multiprocessing as mp
import os
import subprocess as sp
import sys
import time
from typing import (  # pylint: disable=unused-import
    Callable,
    Iterable,
    Iterator,
    List,
    NamedTuple,
)

from prod_common import esubprocess as esp, git_tools as git
from prod_common.epathlib import Path
from prod_common.errors import EResult  # pylint: disable=unused-import
from prod_common.errors import Err, Ok
from prod_common.etypes import (  # pylint: disable=unused-import
    AnyStr,
    PathLike,
)
from prod_common.file_tools import copy, hash_files
from util import colors


logger = logging.getLogger(__name__)

PipCompileJob = NamedTuple(
    "PipCompileJob",
    [
        ("ps", sp.Popen),
        ("cmd_list", List[str]),
        ("frozen_txt", Path),
        ("old_lines", List[str]),
    ],
)


def freeze(config_dir, max_jobs=None):
    # type: (PathLike, int) -> EResult[bool]
    """
    Args:
        @config_dir: Corresponds to the --config-dir CLI option.
        @max_jobs: Corresponds to the --jobs CLI option.

    Returns:
        True iff this function changed at least one line in at least one frozen
        requirements.txt file.
    """
    config_dir = Path(config_dir)
    if max_jobs is None:
        max_jobs = mp.cpu_count()

    needed_freezing = False

    expected_infile_name_combos = list(_all_infile_combinations(config_dir))
    existing_infile_name_combos = list(_existing_infile_name_combos(config_dir))

    _cleanup_frozen_dir(
        config_dir, existing_infile_name_combos, expected_infile_name_combos
    )

    real_max_jobs = min(max_jobs, len(expected_infile_name_combos))

    jobs_finished = [0]  # HACK: Since Python2.X has no 'nonlocal' keyword.
    total_jobs = len(expected_infile_name_combos)

    def print_progress(frozen_txt, status, formatter=lambda x: x):
        # type: (Path, str, Callable[[str], str]) -> None
        """
        Prints a single line to STDOUT indicating whether or not @frozen_txt
        was changed, how many jobs have been run, and how many are left.
        """
        jobs_finished[0] += 1
        print(
            formatter(
                "[{}/{}] {} ({})".format(
                    jobs_finished[0], total_jobs, frozen_txt, status
                )
            )
        )
        sys.stdout.flush()

    jobs = []  # type: List[PipCompileJob]

    # While we have more jobs to run or we still have jobs running...
    while expected_infile_name_combos or jobs:
        # We add a small delay here so we aren't being a nag and running
        # Popen.poll() a million times a second.
        time.sleep(0.1)

        # While we have more jobs to run and running another one wouldn't put
        # us over the max...
        while expected_infile_name_combos and len(jobs) < real_max_jobs:
            infile_name_list = expected_infile_name_combos.pop(0)

            frozen_txt = get_frozen_txt(config_dir, infile_name_list)
            infile_list = get_infiles_and_base(config_dir, infile_name_list)

            # HACK: We use this variable to tell _pip_compile() not to bother
            # reading the frozen.txt file (since it's a copy we just made--see
            # below).
            read_frozen_txt = True

            # If the frozen.txt file does not exist and other frozen
            # requirements.txt files do exist...
            if not frozen_txt.exists() and existing_infile_name_combos:
                # Then we use one of those existing frozen.txt files as a
                # template for this new one. We try to be smart about picking
                # which one to copy.
                read_frozen_txt = False

                src_infile_names = _biggest_subset(
                    [name for name in infile_name_list if name != "base"],
                    existing_infile_name_combos,
                )
                src_infile_names = src_infile_names or ["base"]
                src_frozen_txt = get_frozen_txt(config_dir, src_infile_names)

                copy(src_frozen_txt, frozen_txt, verbose=True)

            job = _pip_compile(infile_list, frozen_txt, read_frozen_txt=read_frozen_txt)
            jobs.append(job)

        # For each currently running 'pip-compile' job...
        for _ in range(len(jobs)):
            job = jobs.pop(0)
            ps, cmd_list, frozen_txt, old_lines = job

            # If the frozen.txt file already existed before running
            # 'pip-compile'...
            if old_lines:
                ok_status = "updated"
            else:
                ok_status = "created"

            exit_code = ps.poll()

            # If the job is still running...
            if exit_code is None:
                jobs.append(job)
                continue

            proc = esp.DoneProcess(ps, cmd_list)

            # If the 'pip-compile' command failed for some reason...
            if exit_code != 0:
                # We kill all other jobs and then return an error.
                while jobs:
                    job = jobs.pop(0)
                    job.ps.kill()

                return proc.to_error()

            # We now read from the 'pip-compile' command's STDERR to retrieve
            # the frozen.txt file's new contents. We then make a few changes to
            # those contents (e.g. make directory names relative and add hash
            # to the top of the file).
            new_lines = _make_dirnames_relative(
                [line + "\n" for line in proc.err.strip().split("\n")]
            )

            # We add a hash to the top of the file that was generated using all
            # of the *.in files that were used to create the frozen.txt file as
            # well as the contents of the frozen.txt file (not including this
            # hash--for obvious reasons).
            frozen_hash = hash_files(
                get_infiles_and_base(config_dir, frozen_txt.stem.split("-")),
                salt="\n".join(new_lines),
            )
            new_lines[0:0] = ["# {}\n".format(frozen_hash)]

            with frozen_txt.open("w") as f:
                f.writelines(line.decode() for line in new_lines)

            # If the 'pip-compile' command didn't make any changes to the
            # frozen.txt file...
            if old_lines == new_lines:
                print_progress(frozen_txt, "no changes made")
                continue

            needed_freezing = True
            print_progress(frozen_txt, ok_status, formatter=colors.bold)

    return Ok(needed_freezing)


def check(config_dir):
    # type: (PathLike) -> bool
    """
    Returns:
        True iff all frozen.txt files are up-to-date.
    """
    config_dir = Path(config_dir)
    result = True

    infile_combos = list(_all_infile_combinations(config_dir))

    # For every frozen.txt file that _should_ exist...
    for combo in infile_combos:
        frozen_txt = get_frozen_txt(config_dir, combo)
        result &= check_frozen_txt(config_dir, frozen_txt)

    return result


def check_frozen_txt(config_dir, frozen_txt):
    # type: (PathLike, PathLike) -> bool
    """
    Returns:
        True iff @frozen_txt is up-to-date. Otherwise, if 'eve freeze' needs to
        be run to update @frozen_txt, we return False.
    """
    config_dir = Path(config_dir)
    frozen_txt = Path(frozen_txt)

    if not frozen_txt.exists():
        return False

    with frozen_txt.open() as f:
        frozen_lines = [str(line) for line in f.readlines()]

    # We use the hash at the top of the @frozen_txt file to determine if it is
    # up-to-date.
    actual_hash = frozen_lines[0].lstrip("# ").rstrip()
    expected_hash = hash_files(
        get_infiles_and_base(config_dir, frozen_txt.stem.split("-")),
        salt="\n".join(frozen_lines[1:]),
    )

    result = str(actual_hash) == str(expected_hash)
    if not result:
        logger.warning("The %s file needs to be updated.", frozen_txt)

    return result


def failed_check_error():
    # type: () -> None
    """Print a check failure message to STDERR."""
    print(
        "eve: Run 'eve freeze' to update this project's frozen.txt files.",
        file=sys.stderr,
    )


def get_infile(config_dir, name):
    # type: (PathLike, str) -> Path
    """
    Returns:
        The *.in file that corresponds with @name.

    Examples
        >>> get_infile("/tmp/eve", "foo")
        PosixPath('/tmp/eve/requirements/foo.in')
    """
    config_dir = Path(config_dir)
    return config_dir / "requirements/{}.in".format(name)


def get_infiles_and_base(config_dir, names):
    # type: (PathLike, Iterable[str]) -> List[Path]
    """
    Returns:
        A list of *.in files corresponding to @names that ALWAYS includes
        base.in.
    """
    names = list(names)
    if "base" not in names:
        names.append("base")

    return [get_infile(config_dir, name) for name in sorted(names)]


def get_frozen_txt(config_dir, infile_names):
    # type: (PathLike, Iterable[str]) -> Path
    """
    Returns:
        The frozen.txt file that corresponds to the group of *.in filenames
        @infile_names.

    Examples:
        >>> get_frozen_txt("/tmp/eve", ["base"])
        PosixPath('/tmp/eve/requirements/frozen/base.txt')

        >>> get_frozen_txt("/tmp/eve", ["foo"])
        PosixPath('/tmp/eve/requirements/frozen/foo.txt')

        >>> get_frozen_txt("/tmp/eve", ["bar", "foo"])
        PosixPath('/tmp/eve/requirements/frozen/bar-foo.txt')

        >>> get_frozen_txt("/tmp/eve", ["base", "foo", "bar"])
        PosixPath('/tmp/eve/requirements/frozen/bar-foo.txt')
    """
    config_dir = Path(config_dir)

    # We filter 'base' out of @infile_names, unless it is alone.
    if infile_names != ["base"]:
        infile_names = sorted(name for name in infile_names if name != "base")

    return config_dir / "requirements/frozen/{}.txt".format("-".join(infile_names))


def pip_freeze(prefix=None):
    # type: (PathLike) -> List[str]
    """
    Returns:
        The list of python packages generated by running `@prefix/bin/python
        -m pip freeze`.
    """
    if prefix is None:
        python = "python"  # type: PathLike
    else:
        prefix = Path(prefix)
        python = prefix / "bin/python"

    env = os.environ.copy()

    # HACK: So 'site-packages' on PYTHONPATH don't get in our way.
    if "PYTHONPATH" in env:
        del env["PYTHONPATH"]

    ps = sp.Popen(
        [str(python), "-m", "pip", "freeze"], stdout=sp.PIPE, stderr=sp.PIPE, env=env
    )
    stdout, _stderr = ps.communicate()

    if sys.version_info >= (3, 0):
        stdout = stdout.decode()

    return stdout.strip().split("\n")


def _get_all_infile_names(config_dir):
    # type: (PathLike) -> Iterator[str]
    """
    Returns:
        All *.in files that exist currently.
    """
    config_dir = Path(config_dir)
    requirements_dir = config_dir / "requirements"
    for child in requirements_dir.iterdir():
        if child.suffix == ".in":
            yield child.stem


def _cleanup_frozen_dir(config_dir, existing_combos, expected_combos):
    # type: (PathLike, Iterable[List[str]], Iterable[List[str]]) -> None
    """
    Side Effects:
        Removes all frozen.txt files that no longer correspond to a valid set
        of *.in files.
    """
    config_dir = Path(config_dir)
    for combo in existing_combos:
        if combo not in expected_combos:
            frozen_txt = get_frozen_txt(config_dir, combo)
            logger.info("Removing %s...", str(frozen_txt))
            os.remove(str(frozen_txt))


def _existing_infile_name_combos(config_dir):
    # type: (PathLike) -> Iterator[List[str]]
    """
    Yields:
        A list of *.in filenames that correspond to an existing frozen.txt
        file. We yield one list per frozen.txt file found.
    """
    config_dir = Path(config_dir)
    frozen_dir = get_frozen_txt(config_dir, ["base"]).parent

    for infile in frozen_dir.glob("*.txt"):
        yield sorted(infile.stem.split("-"))


def _biggest_subset(L1, Ls):
    # type: (Iterable[str], Iterable[Iterable[str]]) -> List[str]
    """
    Returns:
        The largest L in @Ls such that L is a subset of @L1.
    """
    result = []  # type: List[str]
    for L in Ls:
        L = list(L)

        if len(L) <= len(result):
            continue

        if set(L).issubset(set(L1)):
            result = L

    return result


def _all_infile_combinations(config_dir):
    # type: (PathLike) -> Iterator[List[str]]
    """
    Yields:
        A list of *.in filenames for each frozen.txt file that should exist.
    """
    # The base frozen.txt file should always exist and every other frozen.txt
    # file will always include the packages listed in the base.in file.
    # Therefore, we make the frozen.txt names shorter by only including 'base'
    # in the base frozen.txt file's filename.
    infile_names = list(_get_all_infile_names(config_dir))
    if "base" in infile_names:
        infile_names.remove("base")

    yield ["base"]

    for r in range(1, len(list(infile_names)) + 1):
        for infile_name_list in it.combinations(infile_names, r):
            yield sorted(infile_name_list)


def _make_dirnames_relative(lines):
    # type: (Iterable[str]) -> List[str]
    """
    Args:
        @lines: The lines of a frozen.txt file.

    Returns:
        A new list of lines such that the directory names output by
        'pip-compile' are shortened (i.e. made relative).
    """

    def fixed_lines(inner_lines):
        # type: (Iterable[str]) -> Iterator[AnyStr]
        top_level_dir_r = git.top_level_dir()
        if isinstance(top_level_dir_r, Err):
            top_level_dir = os.getcwd()  # type: AnyStr
        else:
            top_level_dir = top_level_dir_r.ok()

        for line in inner_lines:
            yield line.replace(top_level_dir + "/", "")

    return list(str(line).rstrip() + "\n" for line in fixed_lines(lines))


def _pip_compile(infiles, frozen_txt, read_frozen_txt=True):
    # type: (Iterable[PathLike], PathLike, bool) -> PipCompileJob
    """Wrapper for the 'pip-compile' command.

    Args:
        @infiles: The requirements.in files we will use to generate @frozen_txt.
        @frozen_txt: The output requirements.txt that 'pip-compile' will target.
        @read_frozen_txt: Should we bother reading the @frozen_txt file?

    Returns:
        A data structure (PipCompileJob) whose purpose is to conserve relevant
        data about this 'pip-compile' job, so it can later be output back to
        the user on the command-line.
    """
    frozen_txt = Path(frozen_txt)
    cmd_list = [
        "python",
        "-m",
        "piptools",
        "compile",
        "--allow-unsafe",  # Remove after Python3.X migration is complete.
        "--output-file",
        str(frozen_txt),
    ]
    cmd_list.extend(str(fp) for fp in infiles)

    if read_frozen_txt and frozen_txt.exists():
        with frozen_txt.open("r") as f:
            old_lines = [str(line) for line in f.readlines()]
    else:
        old_lines = []

    env = os.environ.copy()
    env["CUSTOM_COMPILE_COMMAND"] = "eve freeze"

    ps = sp.Popen(cmd_list, env=env, stdout=sp.PIPE, stderr=sp.PIPE)
    return PipCompileJob(ps, cmd_list, frozen_txt, old_lines)

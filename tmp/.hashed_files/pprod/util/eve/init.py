"""Functionality related to eve's 'init' sub-command."""

from __future__ import print_function

from contextlib import contextmanager
import datetime as dt
from enum import Enum
from getpass import getuser
import itertools as it
import logging
import os
from os import path
import shutil
from socket import gethostname
import subprocess as sp
import sys
import tempfile
from typing import (  # pylint: disable=unused-import
    IO,
    Dict,
    Iterable,
    Iterator,
    List,
    Mapping,
    MutableMapping,
    NamedTuple,
    Optional,
    Tuple,
)

from prod_common import esubprocess as esp, git_tools as git
from prod_common.core import command_exists
from prod_common.epathlib import Path, path_list
from prod_common.eprint import print_box
from prod_common.errors import EResult  # pylint: disable=unused-import
from prod_common.errors import EdgelpError, EErr, Err, Ok, chain_errors
from prod_common.etypes import PathLike  # pylint: disable=unused-import
from prod_common.file_tools import (
    acquire_file_lock,
    copy,
    hash_files,
    mkdirp,
    move_existing,
    release_file_lock,
)
from util.eve import freeze, parser
from util.eve.parser import (  # pylint: disable=unused-import
    CopyCommand,
    InstallScript,
    Package,
)


logger = logging.getLogger(__name__)
VenvDir = NamedTuple("VenvDir", [("path", Path), ("read_only", bool)])


def get_venv_path(
    config_dir,  # type: PathLike
    venv_dirs,  # type: Iterable[VenvDir]
    infile_names,  # type: Iterable[str]
    force=False,  # type: bool
    debug=False,  # type: bool
    warnings=False,  # type: bool
):
    # type: (...) -> EResult[Path]
    """
    Args:
        @config_dir: Corresponds to the --config-dir CLI option.
        @venv_dirs: Corresponds to the --venv-dir CLI option.
        @infile_names: Corresponds to the --extra CLI option.
        @force: Corresponds to the --force CLI option.
        @debug: Corresponds to the --debug CLI option.
        @warnings:  Corresponds to the --warnings CLI option.

    Returns:
        Ok(venv_path), where `venv_path` is an absolute path of the
        (potentially newly initialized) virtual environment's root directory.
            OR
        Err(EdgelpError), if there was a problem initializing/retrieving a
        virtual environment directory from the provided @venv_dirs.
    """
    config_dir = Path(config_dir)

    hashed_dir_patterns = [
        (Path(__file__).parent, "*.py"),
        (config_dir, "*"),
    ]
    hashed_files = list(_files_from_dir_pttrns(hashed_dir_patterns))

    frozen_txt = freeze.get_frozen_txt(config_dir, infile_names)

    # If the frozen.txt file we were going to use is out-of-date (i.e. needs to
    # be updated via 'eve freeze')...
    if not freeze.check_frozen_txt(config_dir, frozen_txt):
        freeze.failed_check_error()
        sys.exit(1)

    venv_hash = hash_files(hashed_files, salt=frozen_txt.name, length=7)

    venv_path = None  # type: Optional[Path]
    f_lock = None  # type: Optional[IO[str]]
    error = None  # type: Optional[EdgelpError]

    # For every venv directory specified with the --venv-dir option...
    for venv_dir in venv_dirs:
        venv_path = venv_dir.path / "eve-{}".format(venv_hash)
        lockfile = venv_dir.path / ".{}.lock".format(venv_hash)

        # This file will only be created once the venv is fully initialized.
        # Therefore, we can use this file's existance to make the venv
        # initialization process atomic.
        eve_meta_fpath = venv_path / ".eve-metadata.txt"

        # If the venv directory has already been initialized and --force was
        # not specified.
        #
        # NOTE: This if-statement ensures that we only lock the lockfile when
        # the desired venv has not yet been initialized.
        if eve_meta_fpath.exists() and not force:
            break

        # Try to lock a file that is specific to this venv directory...
        try:
            f_lock = acquire_file_lock(lockfile)
        # Except when we have OS/permission issues...
        except (OSError, IOError):
            # Then we log the exception and move on to the next venv directory.
            emsg = (
                "A non-fatal exception occurred while attempting to lock venv"
                " directory: {}".format(venv_path)
            )
            log_func = logger.exception if debug else logger.debug
            log_func(emsg)

            error = EdgelpError(emsg, cause=error)

            venv_path = None
            continue

        # If the venv directory has already been initialized and --force was
        # not specified.
        if eve_meta_fpath.exists() and not force:
            break

        # If --force was specified or the venv directory exists but did not
        # finish initializing...
        if venv_path.exists():
            _rmtree_or_move_existing(venv_path)

        # For observability's sake, we copy all files used to create the hash
        # into a hidden directory within the venv dir.
        _copy_hashed_files_to_venv(venv_path, hashed_files)

        print(
            ">>> Creating a new virtual environment: {}".format(venv_path),
            file=sys.stderr,
        )
        r = _mkvenv(venv_path)  # type: EResult
        if isinstance(r, Err):
            e = r.err()
            chain_errors(e, error)
            return EErr(
                "An error occurred while attempting to create a new"
                " virtual environment: %s",
                venv_path,
                cause=e,
            )

        # We now construct a new *.in file (using the concatenation of the *.in
        # files corresponding to @infile_names) and then use this new *.in file
        # along with @frozen_txt to construct our desired Python environment.
        with _new_requirements_in(config_dir, infile_names) as requirements_in:
            r = _process_requirements_file(
                config_dir, requirements_in, frozen_txt, venv_path=venv_path
            )
            if isinstance(r, Err):
                e = r.err()
                chain_errors(e, error)
                return EErr(
                    "An error occurred when trying to install requirements into"
                    " the venv directory %s.",
                    venv_path,
                    cause=e,
                )

        # We hack the <VENV>/bin/activate and <VENV>/bin/activate_this.py files
        # so sourcing them automatically removes any 'site-packages'
        # directories from your PYTHONPATH (which is a TERRIBLE practice IMO).
        _hack_paths_in_activate_files(venv_path)

        # If this venv dir (i.e. -D|--venv-dir option value) was prefaced with
        # 'ro:', which marks it as "read-only"...
        if venv_dir.read_only:
            # We remove write access (recursively) from all directories in our
            # new venv directory to improve the integrity of using shared venv
            # dirs. This ensures, for example, that `python -m pip install ...`
            # will fail.
            _chmod_read_only(venv_path)

        # NOTE: Remember that creating this metadata file is the marker we use
        # to indicate that a venv has been fully initialized.
        _write_metadata_file(eve_meta_fpath)

        # For observability's sake, we store the extras specified via the
        # -E|--extra option to a file.
        extras_fpath = venv_path / ".extras.txt"
        _write_extras_file(
            extras_fpath, [name for name in infile_names if name != "base"]
        )

        break

    if f_lock is not None:
        release_file_lock(f_lock, lockfile, rm_on_exit=True)

    if venv_path is None:
        return EErr(
            "Unable to create a new virtual environment using any of the"
            " following parent directories: %r",
            venv_dirs,
            cause=error,
        )

    # If the --warnings CLI option was provided...
    if warnings:
        # As a bonus feature, we go through all the packages that are listed in
        # our resultant venv and verify that they match exactly what we have
        # specified in our frozen.txt file. If there are any differences (e.g.
        # maybe the user ran `pip install pudb` at some point after creating
        # the venv), then we output a warning message.
        _warn_of_package_diffs(config_dir, venv_path, infile_names)

    return Ok(venv_path)


def _copy_hashed_files_to_venv(venv_path, hashed_files):
    # type: (PathLike, Iterable[PathLike]) -> None
    venv_path = Path(venv_path)
    hashed_files = path_list(hashed_files)

    hfiles_dir = venv_path / ".hashed_files"
    mkdirp(hfiles_dir)

    for hfile in hashed_files:
        # Needed to prevent the call to Path.relative_to() below from crashing
        # when dealing with symlinked directories.
        hfile = hfile.resolve()

        top_level_dir = _get_top_level_dir(hfile.parent)

        rel_dir = hfile.relative_to(top_level_dir).parent
        hdir = hfiles_dir / rel_dir
        mkdirp(hdir)

        src = hfile
        dest = hdir / hfile.name
        copy(src, dest)


def _warn_of_package_diffs(config_dir, venv_path, infile_names):
    # type: (PathLike, PathLike, Iterable[str]) -> None
    config_dir = Path(config_dir)
    venv_path = Path(venv_path)
    infile_names = list(infile_names)

    frozen_txt = freeze.get_frozen_txt(config_dir, infile_names)

    # We use `pip freeze` to get the list of "actual" python packages that are
    # installed in the venv.
    actual_pypacks = freeze.pip_freeze(prefix=venv_path)

    # To construct the list of "expected" python packages, we start by
    # collecting all packages mentioned in the frozen.txt file.
    eve_reqs_r = parser.parse_infile(config_dir, frozen_txt)
    if isinstance(eve_reqs_r, Err):
        e = eve_reqs_r.err()
        logger.error(e.report())
        return

    eve_reqs = eve_reqs_r.ok()
    expected_pypacks = [str(pack) for pack in eve_reqs.normal_packages]

    # We then check each relevant *.in files for @install pragmas. NOTE: since
    # custom install scripts install from source, we are potentially missing
    # dependencies of these packages (this is one reason to avoid using custom
    # install scripts unless you absolutely have to).
    for infile in freeze.get_infiles_and_base(config_dir, infile_names):
        eve_reqs_r = parser.parse_infile(config_dir, infile)
        if isinstance(eve_reqs_r, Err):
            e = eve_reqs_r.err()
            logger.error(e.report())
            return

        eve_reqs = eve_reqs_r.ok()
        expected_pypacks.extend([str(pack) for pack in eve_reqs.install_scripts])

    def normalize_pypacks(pypacks):
        # type: (Iterable[str]) -> List[str]
        return [
            pypack.lower().replace("_", "-").replace("git+", "")
            for pypack in pypacks
            if "pip==" not in pypack and "setuptools==" not in pypack
        ]

    # Normalize Package Names
    actual_pypacks = normalize_pypacks(actual_pypacks)
    expected_pypacks = normalize_pypacks(expected_pypacks)

    venv_name = "the {} venv".format(venv_path.name)
    found = False
    for pypacks1, pypacks2, name1, name2 in [
        (actual_pypacks, expected_pypacks, venv_name, frozen_txt.name),
        (expected_pypacks, actual_pypacks, frozen_txt.name, venv_name),
    ]:
        for pypack in set(pypacks1) - set(pypacks2):
            found = True
            logger.warning("%s was found in %s, but is not in %s", pypack, name1, name2)

    if found:
        logger.debug(
            "Consider listing personal packages in %s instead of patching the venv"
            " manually.",
            config_dir / "requirements/private.in",
        )


def _files_from_dir_pttrns(dir_pttrns):
    # type: (Iterable[Tuple[PathLike, str]]) -> Iterator[str]
    """
    Helper function used to retrieve the files that will be used to create the
    venv hash.
    """
    for hdir, pttrn in dir_pttrns:
        hpath = Path(hdir)
        for P in list(set(hpath.rglob(pttrn)) - set(hpath.rglob("*.swp"))):
            if P.is_file():
                yield str(P.resolve())


def _get_top_level_dir(cwd):
    # type: (PathLike) -> Path
    cwd = Path(cwd)
    assert (
        cwd.is_dir()
    ), "The _get_top_level_dir() function requires the 'cwd' argument be a directory!"

    top_level_dir_r = git.top_level_dir(cwd=cwd)
    if isinstance(top_level_dir_r, Err):
        result = cwd
    else:
        result = Path(top_level_dir_r.ok())

    return result.resolve()


def _process_requirements_file(config_dir, requirements_in, frozen_txt, venv_path=None):
    # type: (PathLike, PathLike, PathLike, PathLike) -> EResult[None]
    config_dir = Path(config_dir)
    requirements_in = Path(requirements_in)
    frozen_txt = Path(frozen_txt)

    if venv_path is None:
        prefix = Path("/usr")
    else:
        venv_path = Path(venv_path)
        prefix = venv_path

    # We first build a proper environment (e.g. set of envvars) for child
    # processes to inherit from.
    env = _get_env(config_dir, prefix=prefix)

    # Then we parse the *.in file.
    eve_reqs_r = parser.parse_infile(config_dir, requirements_in)
    if isinstance(eve_reqs_r, Err):
        e = eve_reqs_r.err()
        return EErr(
            "An error occurred while parsing the %s file.", requirements_in, cause=e
        )

    eve_reqs = eve_reqs_r.ok()

    # Run all pre-hooks which are associated with a package listed in the *.in
    # file we parsed earlier.
    with _new_work_dir(env) as cwd:
        r = _run_pre_hooks(
            it.chain(eve_reqs.normal_packages, eve_reqs.install_scripts),
            config_dir,
            env=env,
            cwd=cwd,
        )
        if isinstance(r, Err):
            e = r.err()
            return EErr("Failed while executing pre-hooks.", cause=e)

    # Run `pip install -r <FROZEN>` where <FROZEN> is the frozen.txt file.
    r = _pip_install_r(frozen_txt, eve_reqs.normal_packages, env=env)
    if isinstance(r, Err):
        e = r.err()
        return EErr(
            "Failed while installing packages from the %s file.",
            requirements_in,
            cause=e,
        )

    # Install packages which need to use custom install scripts (i.e. all
    # packages specified via the special @install tag in the *.in file).
    with _new_work_dir(env) as cwd:
        r = _install_from_scripts(eve_reqs.install_scripts, env=env, cwd=cwd)
        if isinstance(r, Err):
            e = r.err()
            return EErr(
                "Failed while installing packages from install scripts.", cause=e
            )

    # Run all post-hooks which are associated with a package listed in the *.in
    # file we parsed earlier.
    with _new_work_dir(env) as cwd:
        r = _run_post_hooks(
            it.chain(eve_reqs.normal_packages, eve_reqs.install_scripts),
            config_dir,
            env=env,
            cwd=cwd,
        )
        if isinstance(r, Err):
            e = r.err()
            return EErr("Failed while executing post-hooks.", cause=e)

    # Run copy commands (i.e. copy all files specified via the special @copy
    # tag in the *.in file to our new Python environment's 'site-packages'
    # directory).
    r = _run_copy_commands(eve_reqs.copy_commands, prefix=prefix)
    if isinstance(r, Err):
        e = r.err()
        return EErr("Failed while running copy commands.", cause=e)

    return Ok(None)


def _get_env(config_dir, prefix, env=None):
    # type: (PathLike, PathLike, Mapping[str, str]) -> Dict[str, str]
    config_dir = Path(config_dir)
    prefix = str(prefix)

    if env is None:
        env = os.environ

    env = dict(env.items())

    # PREFIX is a special envvar that is available to all pre/post/install
    # scripts.
    env["PREFIX"] = prefix

    # Get rid of all 'site-package' directories from our PYTHONPATH.
    pypath_list = env["PYTHONPATH"].split(":")
    env["PYTHONPATH"] = ":".join(
        pypath for pypath in pypath_list if "site-packages" not in pypath
    )

    # PACKAGE_MANAGER is a special envvar that is available to all
    # pre/post/install scripts (it is mainly meant to be used by pre-hooks)
    # which specifies what package manager should be used to install external
    # dependencies.
    package_manager = _get_package_manager()
    env["PACKAGE_MANAGER"] = package_manager.value

    prefix_lib = path.join(prefix, "lib")
    prefix_include = path.join(prefix, "include")
    prefix_bin = path.join(prefix, "bin")

    _prepend_to_path(env, "CPLUS_INCLUDE_PATH", prefix_include)
    _prepend_to_path(env, "C_INCLUDE_PATH", prefix_include)
    _prepend_to_path(env, "LD_LIBRARY_PATH", prefix_lib)
    _prepend_to_path(env, "PATH", str(config_dir / "scripts"))
    _prepend_to_path(env, "PATH", prefix_bin)

    old_ldflags = env.get("LDFLAGS")
    env["LDFLAGS"] = "-L{}".format(prefix_lib)
    if old_ldflags is not None:
        env["LDFLAGS"] += " {}".format(old_ldflags)

    return env


def _run_pre_hooks(packages, config_dir, env, cwd):
    # type: (Iterable[Package], PathLike, Mapping[str, str], PathLike) -> EResult[None]
    return __run_hooks("pre", "pre_hook", packages, config_dir, env, cwd)


def _pip_install_r(frozen_txt, packages, env):
    # type: (PathLike, Iterable[Package], Mapping[str, str]) -> EResult[None]
    frozen_txt = Path(frozen_txt)

    _print_header(
        "Installing the following python packages:\n\n * {}\n\nNote that the above list"
        " includes 1st-level dependencies only (e.g. no deps of deps).".format(
            "\n * ".join(str(pack) for pack in sorted(packages))
        ),
        title="pip install -r {}".format(frozen_txt),
    )

    cmd_list = ["python", "-m", "pip", "install", "-r", str(frozen_txt)]

    r = esp.safe_popen(
        cmd_list,
        env=env,
        stdout=sys.stderr,
    )
    if isinstance(r, Err):
        e = r.err()
        return EErr(
            "An error occurred while attempting to 'pip install' Python packages.",
            cause=e,
        )

    return Ok(None)


def _install_from_scripts(install_scripts, env, cwd):
    # type: (Iterable[InstallScript], Mapping[str, str], PathLike) -> EResult[None]
    env = dict(env.items())

    if install_scripts:
        _print_header(
            "Running custom install scripts for the following packages:\n * {}".format(
                "\n * ".join(str(P) for P in install_scripts)
            ),
            title="Running Custom Install Scripts",
        )

    for install_script in install_scripts:
        with _package_context(install_script, env):
            r = esp.safe_popen(
                [str(install_script.exe)],
                env=env,
                cwd=str(cwd),
                stdout=sys.stderr,
                stderr=sys.stderr,
            )
            if isinstance(r, Err):
                e = r.err()
                return EErr(
                    "An error occurred while attempting to install %s from a script.",
                    install_script,
                    cause=e,
                )

        out, _err = esp.unsafe_popen(["python", "-m", "pip", "freeze"], env=env)
        if str(install_script) not in out:
            return EErr(
                "The install script ran OK, but didn't seem to actually install the"
                " package since '%s' is not included in the list of packages"
                " installed in this environment:\n\n  %s",
                install_script,
                "\n  ".join(out.split("\n")),
            )

    return Ok(None)


def _run_post_hooks(packages, config_dir, env, cwd):
    # type: (Iterable[Package], PathLike, Mapping[str, str], PathLike) -> EResult[None]
    return __run_hooks("post", "post_hook", packages, config_dir, env, cwd)


def __run_hooks(hook_name, attr, packages, config_dir, env, cwd):
    # type: (str, str, Iterable[Package], PathLike, Mapping[str, str], PathLike) -> EResult[None]
    env = dict(env.items())

    packs_with_hooks = []
    for pack in packages:
        hook_func = getattr(pack, attr)
        hook_exe = hook_func(config_dir)
        if hook_exe:
            packs_with_hooks.append((pack, hook_exe))

    if packs_with_hooks:
        _print_header(
            "Found {}-hook scripts for the following packages:\n\n * {}".format(
                hook_name, "\n * ".join(str(pack) for pack, _ in packs_with_hooks)
            ),
            title="Running {}-Hooks".format(hook_name.title()),
        )

    for pack, hook_exe in packs_with_hooks:
        with _package_context(pack, env):
            r = esp.safe_popen(
                [str(hook_exe)],
                env=env,
                cwd=str(cwd),
                stdout=sys.stderr,
                stderr=sys.stderr,
            )
            if isinstance(r, Err):
                e = r.err()
                return EErr(
                    "An error occurred while running the pre-hook associated with %s.",
                    pack,
                    cause=e,
                )
    return Ok(None)


def _run_copy_commands(copy_commands, prefix):
    # type: (Iterable[CopyCommand], PathLike) -> EResult[None]
    if copy_commands:
        _print_header(
            "Copying the following files to the 'site-packages' directory"
            " associated with {}:\n\n * {}".format(
                prefix,
                "\n * ".join(
                    "{} -> {}".format(cmd.src, cmd.rel_dest) for cmd in copy_commands
                ),
            ),
            title="Site-Package Copy Commands",
        )

    for copy_cmd in copy_commands:
        copy_cmd(prefix)

    return Ok(None)


@contextmanager
def _new_requirements_in(config_dir, infile_names):
    # type: (PathLike, Iterable[str]) -> Iterator[Path]
    config_dir = Path(config_dir)
    infile_names = list(infile_names)

    infiles = [freeze.get_infile(config_dir, name) for name in infile_names]

    _, tmp_fname = tempfile.mkstemp(prefix="eve-requirements-", suffix=".in")
    requirements_in = Path(tmp_fname)

    with requirements_in.open("a") as f1:
        for infile in infiles:
            assert infile.exists()
            with infile.open("r") as f2:
                f1.write("\n### {}\n".format(infile).decode())
                f1.writelines(f2.readlines())

    yield requirements_in

    os.remove(str(requirements_in))


@contextmanager
def _new_work_dir(env):
    # type: (MutableMapping[str, str]) -> Iterator[str]
    """
    This context manager assigns the special WORKDIR envvar (which is available
    to all pre/post/install scripts) to a temporary directory which is deleted
    upon exiting this context.
    """
    work_dir = tempfile.mkdtemp(prefix="eve-work-")
    env["WORKDIR"] = work_dir

    yield work_dir

    del env["WORKDIR"]
    shutil.rmtree(work_dir)


@contextmanager
def _package_context(package, env):
    # type: (Package, MutableMapping[str, str]) -> Iterator[None]
    """
    This context manager assigns values to the following special envvars (which
    are available to all pre/post/install scripts):

      PN: The @package package's name.
      PV: The @pacakge package's version.
    """
    env["PN"] = package.name
    env["PV"] = package.version

    yield

    del env["PN"]
    del env["PV"]


def _prepend_to_path(env, key, new_path):
    # type: (MutableMapping[str, str], str, str) -> None
    orig_path = env.get(key)
    if orig_path is not None and new_path in orig_path.split(":"):
        return

    env[key] = new_path
    if orig_path is not None:
        env[key] += ":{}".format(orig_path)


def _chmod_read_only(venv_path):
    # type: (PathLike) -> None
    venv_path = str(venv_path)

    logger.info("Making virtual environment directory read-only: %s", venv_path)
    for root, dirs, _files in os.walk(venv_path):
        for D in dirs:
            abs_dir = path.join(root, D)
            if path.islink(abs_dir):
                continue

            os.chmod(abs_dir, 0o555)


def _mkvenv(venv_path):
    # type: (PathLike) -> EResult[Tuple[str, str]]
    if sys.version_info < (3, 0):
        cmd_list = ["virtualenv"]
    else:
        cmd_list = ["python", "-m", "venv"]

    cmd_list.append(str(venv_path))
    return esp.safe_popen(cmd_list)


def _write_metadata_file(fpath):
    # type: (PathLike) -> None
    fpath = Path(fpath)

    lines = [
        "HOSTNAME:  {}".format(gethostname()),
        "TIMESTAMP: {}".format(dt.datetime.now().strftime("%Y-%m-%d %H:%M:%S")),
        "USERNAME:  {}".format(getuser()),
    ]
    lines = [line.rstrip() + "\n" for line in lines]
    with fpath.open("w") as f:
        f.writelines(line.decode() for line in lines)


def _write_extras_file(fpath, extras):
    # type: (PathLike, Iterable[str]) -> None
    with open(str(fpath), "w") as f:
        if extras:
            f.write(",".join(sorted(extras)))
        else:
            f.write("none")


def _rmtree_or_move_existing(fdir):
    # type: (PathLike) -> None
    """
    Removes @fdir from the filesystem or, failing that, renames it to @fdir.N,
    where N is some integer.

    Pre-conditions:
        * @fdir exists and is a directory.
    """
    fdir = Path(fdir)
    assert fdir.is_dir(), "{} is NOT a directory".format(fdir)

    try:
        shutil.rmtree(str(fdir))
    except OSError:
        new_fdir = move_existing(fdir)
        logger.warning("Unable to delete %s, so renamed to %s instead.", fdir, new_fdir)
    else:
        logger.info("Successfully deleted %s.", fdir)


def _hack_paths_in_activate_files(venv_path):
    # type: (PathLike) -> None
    venv_path = str(venv_path)

    header = (
        "### START EVE ENVIRONMENT VARIABLES ###\n# The following lines were added"
        " automatically by the 'eve' script.\n"
    )
    footer = "### END EVE ENVIRONMENT VARIABLES ###\n"
    pypath_code = (
        "':'.join(pypath for pypath in os.environ.get('PYTHONPATH', '').split(':') if"
        " 'site-packages' not in pypath)"
    )
    venv_lib = path.join(venv_path, "lib")
    venv_include = path.join(venv_path, "include")

    activate_file = path.join(venv_path, "bin/activate")

    # Assign values to envvars on activation.
    with open(activate_file, "a") as f:
        f.write("\n" + header)
        f.write(
            _activate_var(
                "PYTHONPATH",
                '"$(python -c "import os; print({})")"'.format(pypath_code),
            )
        )
        f.write(
            _activate_var("LD_LIBRARY_PATH", "{}:$LD_LIBRARY_PATH".format(venv_lib))
        )
        f.write(
            _activate_var("C_INCLUDE_PATH", "{}:$C_INCLUDE_PATH".format(venv_include))
        )
        f.write(
            _activate_var(
                "CPLUS_INCLUDE_PATH", "{}:$CPLUS_INCLUDE_PATH".format(venv_include)
            )
        )
        f.write(footer)

    # On deactivation (e.g. `deactivate`), we set the envvars back to the value
    # they had prior to activating this venv.
    with open(activate_file, "r") as f:
        old_lines = f.readlines()

        new_lines = []
        for i, line in enumerate(old_lines):
            new_lines.append(line)
            if "deactivate ()" in line:
                new_lines.append(
                    "    {}".format(
                        "\n    ".join(H for H in header.split("\n") if H != "") + "\n"
                    )
                )
                new_lines.append(_deactivate_var("PYTHONPATH"))
                new_lines.append(_deactivate_var("LD_LIBRARY_PATH"))
                new_lines.append(_deactivate_var("C_INCLUDE_PATH"))
                new_lines.append(_deactivate_var("CPLUS_INCLUDE_PATH"))
                new_lines.append("    {}\n".format(footer))

                new_lines.extend(old_lines[i + 1 :])
                break

    with open(activate_file, "w") as f:
        f.writelines(new_lines)

    # We try to make the same envvar changes when the venv is activated via the
    # `activate_this.py` Python module.
    with open(path.join(venv_path, "bin/activate_this.py"), "a") as f:
        f.write("\n" + header)
        f.write("os.environ['PYTHONPATH'] = {}\n".format(pypath_code))
        f.write(
            "os.environ['LD_LIBRARY_PATH'] = '{0}:' + os.environ['LD_LIBRARY_PATH'] if"
            " 'LD_LIBRARY_PATH' in os.environ else '{0}'\n".format(venv_lib)
        )
        f.write(footer)


def _activate_var(key, value):
    # type: (str, str) -> str
    result = ""
    result += '_OLD_VIRTUAL_{0}="${0}"\n'.format(key)
    result += "export {}={}\n".format(key, value)
    return result


def _deactivate_var(key, level=2):
    # type: (str, int) -> str
    old_key = "_OLD_VIRTUAL_{}".format(key)
    indent = " " * (4 * (level - 1))

    result = ""
    result += '{}if ! [ -z "${{{}+_}}" ]; then\n'.format(indent, old_key)
    result += '{}    export {}="${}"\n'.format(indent, key, old_key)
    result += "{}    unset {}\n".format(indent, old_key)
    result += "{}fi\n".format(indent)

    return result


def _print_header(msg, title=None):
    # type: (str, str) -> None
    print(file=sys.stderr)
    print_box(msg, title=title, file=sys.stderr)


class PackageManager(Enum):
    APT = "apt"
    YUM = "yum"
    BREW = "brew"
    NONE = "none"


def _get_package_manager():
    # type: () -> PackageManager
    ps = sp.Popen(["sudo", "-v"], stdout=sys.stderr)
    if ps.wait() != 0:
        return PackageManager.NONE

    # NOTE: We check for 'apt-get' here instead of 'apt' since it is more
    # likely that there are other scripts with the name 'apt' installed than
    # 'apt-get' (e.g. on my MacBook the 'apt' command is the "annotation
    # processing tool").
    if command_exists("apt-get"):
        return PackageManager.APT
    elif command_exists("yum"):
        return PackageManager.YUM
    elif command_exists("brew"):
        return PackageManager.BREW

    return PackageManager.NONE

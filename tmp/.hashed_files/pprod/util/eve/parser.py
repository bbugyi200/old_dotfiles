"""
This module contains functions/classes that help parse *.in files and represent
the packages and special eve pragmas that are found in those *.in files.
"""

import logging
import os
import re
import sys
from typing import (  # pylint: disable=unused-import
    Iterable,
    List,
    NamedTuple,
    Optional,
)

from prod_common.core import cname
from prod_common.epathlib import Path
from prod_common.errors import EErr, Ok
from prod_common.errors import EResult  # pylint: disable=unused-import
from prod_common.etypes import PathLike  # pylint: disable=unused-import
from prod_common.file_tools import copy


logger = logging.getLogger(__name__)

EveRequirements = NamedTuple(
    "EveRequirements",
    [
        ("normal_packages", Iterable["Package"]),
        ("install_scripts", Iterable["InstallScript"]),
        ("copy_commands", Iterable["CopyCommand"]),
    ],
)


def parse_infile(config_dir, infile):
    # type: (PathLike, PathLike) -> EResult[EveRequirements]
    """Parse the @infile *.in file."""
    config_dir = Path(config_dir)
    infile = Path(infile)

    normal_packages = []  # type: List[Package]
    install_scripts = []  # type: List[InstallScript]
    copy_commands = []  # type: List[CopyCommand]

    RE_PACKAGE = (
        r"([A-Za-z0-9\-_\.]+)"  # package name
        r"(?:\[[A-Za-z0-9\-_\.,]+\])?"  # optional setuptools extra(s)
        r"=="
        r"([0-9]\S+)"  # pacakge version
        r"(?:[ ]*;.*?)?"  # optional package marker(s)
        r"((?:[ ]*#.*?)?)"  # inline comment
    )

    for i, line in enumerate(infile.open()):
        line_no = i + 1

        # Parse --editable packages.
        match = re.match(r"^(?:-e[ ]|--editable[ =])(\S+#egg=\S+)(?:[ ]*#.*)?$", line)
        if match:
            url = str(match.group(1))
            name = url.split("#egg=", 1)[1]

            normal_packages.append(EditablePackage(name, url))

            continue

        # Parse normal packages.
        match = re.match(r"^{}$".format(RE_PACKAGE), line)
        if match:
            name = str(match.group(1))
            version = str(match.group(2)).rstrip()
            comment = str(match.group(3)).lstrip("# ").rstrip()

            package = Package(name, version, comment if comment else None)
            normal_packages.append(package)

            continue

        # Parse custom install scripts.
        match = re.match(r"^#[ ]?@install:[ ]{}$".format(RE_PACKAGE), line)
        if match:
            name = str(match.group(1))
            version = str(match.group(2)).rstrip()
            exe = config_dir / "scripts/install/{}-{}".format(name, version)

            if not exe.exists() or not os.access(str(exe), os.X_OK):
                return EErr(
                    "Error on line #%d of the %s file.\n\n"
                    "LINE: %r\n"
                    "ERROR: The install script %s either does not exist or is not"
                    " executable.",
                    line_no,
                    infile,
                    line,
                    exe,
                )

            install_scripts.append(InstallScript(name, version, exe))
            continue

        # Parse copy commands.
        match = re.match(r"^#[ ]?@copy:[ ](\S+)[ ](\S+)$", line)
        if match:
            src = config_dir / match.group(1)
            dest = str(match.group(2))

            if not src.exists():
                return EErr(
                    "Error on line #%d of the %s file.\n\n"
                    "LINE: %r\n"
                    "ERROR: The file which is specified as the source of the copy"
                    " command (%s) does not exist.",
                    line_no,
                    infile,
                    line,
                    src,
                )

            copy_commands.append(CopyCommand(src, dest))
            continue

    return Ok(EveRequirements(normal_packages, install_scripts, copy_commands))


class Package(object):
    """A package parsed from a *.in file.

    Args:
        @name: The name of the Python package.
        @version: The version of the Python pacakge.
        @comment: An optional inline comment.
    """

    def __init__(self, name, version, comment=None):
        # type: (str, str, str) -> None
        self.name = name
        self.version = version
        self.comment = comment

    def __str__(self):
        # type: () -> str
        return "{}=={}".format(self.name, self.version)

    def __repr__(self):
        # type: () -> str
        return "{}(name={}, version={})".format(cname(self), self.name, self.version)

    def __lt__(self, other):
        # type: (Package) -> bool
        return (self.name.lower(), self.version) < (other.name.lower(), other.version)

    def pre_hook(self, config_dir):
        # type: (PathLike) -> Optional[Path]
        """
        Returns:
            The path to the pre-hook script associated with this package if
            such a pre-hook script exists. Otherwise, returns None.
        """
        return self._hook("pre", config_dir)

    def post_hook(self, config_dir):
        # type: (PathLike) -> Optional[Path]
        """
        Returns:
            The path to the post-hook script associated with this package if
            such a post-hook script exists. Otherwise, returns None.
        """
        return self._hook("post", config_dir)

    def _hook(self, hook_name, config_dir):
        # type: (str, PathLike) -> Optional[Path]
        hook_dir = Path(config_dir) / "scripts/{}".format(hook_name)
        for hook_exe in [
            hook_dir / "{}-{}".format(self.name, self.version),
            hook_dir / self.name,
        ]:
            if hook_exe.exists() and os.access(str(hook_exe), os.X_OK):
                return hook_exe

        return None


class EditablePackage(Package):
    """An editable pacakge parsed from a *.in file.

    In other words, this class represents a line in a *.in file of the form:
      -e git+git://github.com/user/foo.git#egg=foo
    """

    def __init__(self, name, url):
        # type: (str, str) -> None
        super(EditablePackage, self).__init__(name, "<DEV>")

        self.url = url

    def __str__(self):
        # type: () -> str
        return "-e {}".format(self.url)


class InstallScript(Package):
    """
    A custom install script that was parsed from a special pragma found in a
    *.in file.

    In other words, this class represents a line in a *.in file of the form:
      # @install: foo==1.2.3

    In the example given above, it is expected that the file
    <EVE>/scripts/install/foo-1.2.3 both exists and is executable.

    Args:
        @exe: The path to the executable install script.
    """

    def __init__(self, name, version, exe):
        # type: (str, str, PathLike) -> None
        super(InstallScript, self).__init__(name, version)

        assert os.access(str(exe), os.X_OK)
        self.exe = Path(exe)


class CopyCommand(object):
    """
    A custom copy command that was parsed from a special pragma found in a *.in
    file.

    In other words, this class represents a line in a *.in file of the form:
      # @copy: path/to/src_file dest_file
    """

    def __init__(self, src, rel_dest):
        # type: (PathLike, str) -> None
        self.src = Path(src)
        self.rel_dest = rel_dest

    def __repr__(self):
        # type: () -> str
        return "{}(src={}, rel_dest={})".format(cname(self), self.src, self.rel_dest)

    def __call__(self, prefix):
        # type: (PathLike) -> None
        """Execute the copy command.

        Copies the source file
          <EVE>/path/to/src_file
        to
          @prefix/lib/pythonX.Y/site-packages/dest_file
        """
        prefix = Path(prefix)

        vinfo = sys.version_info
        pyversion = "{}.{}".format(vinfo.major, vinfo.minor)

        site_packages_dir = prefix / "lib/python{}/site-packages".format(pyversion)
        dest = site_packages_dir / self.rel_dest

        copy(self.src, dest, verbose=True)

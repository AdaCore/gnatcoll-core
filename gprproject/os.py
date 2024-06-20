from __future__ import annotations
from typing import TYPE_CHECKING
import os
import sys

if TYPE_CHECKING:
    from typing import Any


def add_search_path(name: str, path: str) -> None:
    """Prepend a path to a variable holding a list of paths.

    :param name: variable name
    :param path: path to add
    """
    prev_value = os.environ.get(name)
    if prev_value:
        os.environ[name] = path + os.pathsep + prev_value
    else:
        os.environ[name] = path


def which(prog: str, paths: str | None = None, default: Any = "") -> Any:
    """Locate executable.

    :param prog: program to find
    :param paths: if not None then we use this value instead of PATH to look
        for the executable.
    :param default: default value to return if not found

    :return: absolute path to the program on success, found by searching for an
      executable in the directories listed in the environment variable PATH
      or default value if not found
    """

    def is_exe(file_path: str) -> bool:
        return os.path.isfile(file_path) and os.access(file_path, os.X_OK)

    def possible_names(file_path: str) -> list[str]:
        names = [file_path]
        if sys.platform == "win32":  # unix: no cover
            names.extend(
                [
                    file_path + ext
                    for ext in os.environ.get("PATHEXT", ".EXE").split(";")
                ]
            )
        return names

    fpath, _ = os.path.split(prog)
    if fpath:
        # Full path given, check if executable
        for progname in possible_names(prog):
            if is_exe(progname):
                return progname
    else:
        # Check for all directories listed in $PATH
        if paths is None:
            paths = os.environ["PATH"]

        for pathdir in paths.split(os.pathsep):
            exe_file = os.path.join(pathdir, prog)
            for progname in possible_names(exe_file):
                if is_exe(progname):
                    return progname

    # Not found.
    return default

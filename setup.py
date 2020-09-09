#! /usr/bin/env python
"""Reads and creates symlinks to dotfiles.

Utility for reading dotfiles (see dotfiles.json) and creating symlinks from ~/
to the dotfiles in this repository / referenced by dotfiles.json.

Fuck it, we're python3-only now
"""

from __future__ import print_function, unicode_literals

import json
import sys
import socket
import io
from pathlib import Path

# Python 2/3 compatability.
try:
    from glob import fnmatch  # type: ignore
except ImportError:
    import fnmatch  # type: ignore

# Again, Python 2/3 compatability.
IS_PY3 = sys.version_info[0] == 3
if not IS_PY3:
    raise Error("wrong python version! must be 3")
Text = str
Bytes = bytes

DOTFILES_FILENAME = "dotfiles.json"

def get_dotfiles():
    """
    Reads out all the files in the directory, except those that should not be
    used, like .git/ and this file (setup.py)
    """
    return [str(x) for x
        in (*Path('.config').iterdir(), *Path('.').iterdir())
        if str(x)[0]=='.' and str(x) != '.git' and str(x) != '.config']

def os_error_to_dict(err):
    """
    Converts an OsError to a dict, for easy JSON serialization.
    """
    ret = {
        "errno": err.errno,
        "strerror": err.strerror,
    }
    if sys.platform == "win32":
        ret["winerror"] = err.winerror
    ret["filename"] = err.filename
    ret["filename2"] = err.filename2
    return ret

class Dotfile:
    """A dotfile to be symlinked.

    Properties:
        path (str): The dotfile's path, relative to this script.
        dest (str): The dotfile's destination, relative to the user's home directory.
        platforms (List[str]): The platforms the dotfile should be linked on,
            or None if it should always be linked.
        hostname_pats (List[str]): Hostname-globs that this file should be linked on,
            or None if it should always be linked.
    """

    def __init__(self, dotfile):
        """Creates a new Dotfile instance.

        >>> Dotfile('xyz')
        Dotfile({'path': 'xyz', 'dest': 'xyz', 'platform': None, 'hostname': None})

        >>> Dotfile({'path': 'xyz', 'when': {'hostname': 'win32'}})
        Dotfile({'path': 'xyz', 'dest': 'xyz', 'platform': None, 'hostname': ['win32']})

        >>> Dotfile({'path': 'xyz', 'when': {'hostname': ['win32', 'linux']}})
        Dotfile({'path': 'xyz', 'dest': 'xyz', 'platform': None, 'hostname': ['win32', 'linux']})

        >>> Dotfile({'path': 'xyz', 'dest': 'abc'})
        Dotfile({'path': 'xyz', 'dest': 'abc', 'platform': None, 'hostname': None})
        """
        # A simple path; assume defaults.
        self.path = dotfile
        self.dest = dotfile
        
    def __repr__(self):
        return "Dotfile({{'path': {}, 'dest': {}}})".format(
            repr(self.path),
            repr(self.dest),
        )


DIR_STATUS = {
    "DOESNT_EXIST": "The directory doesn't exist",
    "EXISTS": "The directory exists",
    "FILE_EXISTS": "A file with the same name exists where the directory is expected",
}


def mkdir(dest):
    """Creates the given directory, and any necessary leading components.

    Returns: A status string, one of the values of DIR_STATUS.
    """
    parent = dest.parent
    if not parent.exists():
        os.makedirs(parent)
        return DIR_STATUS["DOESNT_EXIST"]
    elif parent.is_dir():
        return DIR_STATUS["EXISTS"]
    else:
        return DIR_STATUS["FILE_EXISTS"]


LINK_STATUS = {
    "DOESNT_EXIST": "Link doesn't exist",
    "EXISTS": "A different file exists where the link would be",
    "OK": "The link exists and points to the correct destination",
    "CREATED": "Link newly-created",
}

STATUSES = {
    "ERRORED": "An OS error was encountered",
    "LINK_OK": "OK",
    "EXISTS": "Something else exists where the link would be; refusing to overwrite",
    "SKIPPED": "Skipped",
}


def link_exists(path, dest):
    """Checks if a symlink exists, pointing to `dest`.

    Returns: A status string, one of the values of LINK_STATUS.
    """
    if dest.exists():
        if dest.resolve() == path.resolve():
            return LINK_STATUS["OK"]
        else:
            return LINK_STATUS["EXISTS"]
    else:
        return LINK_STATUS["DOESNT_EXIST"]


class Dotfiles:
    """Resolves and links dotfiles. Ingests dotfile configuration data, checks
    the filesystem, and creates links.
    """

    def __init__(self, dotfiles=None, hostname=None, platform=None):
        """Creates a new Dotfiles instance.
        """
        self.dotfiles = list(dotfiles)
        
        self.hostname = socket.getfqdn()
        self.platform = sys.platform

        self.home = Path.home()

    def files(self):
        """Gets an iterator over this instane's dotfiles.

        Returns (Iterator[Dotfile]): An iterator over each of the dotfiles held
            by this instance.
        """
        return map(Dotfile, self.dotfiles)

    def link_all(self):
        """Creates links for all dotfiles represented by this instance.

        Returns: A JSON report describing the changes made.
        """
        ret = {"changed": False, "failed": False}
        report = []

        def link(path, dest):
            report.append(
                {"path": str(path), "dest": str(dest), "status": LINK_STATUS["CREATED"]}
            )
            try:
                path.symlink_to(dest, target_is_directory=dest.is_dir())
                ret["changed"] = True
            except OSError as err:
                report[-1]["status"] = STATUSES["ERRORED"]
                report[-1]["error"] = os_error_to_dict(err)
                ret["failed"] = True

        for dotfile in self.files():
            resolved_path = Path(dotfile.path)
            resolved_dest = Path(self.home).joinpath(Path(dotfile.dest))
            print(resolved_dest)
            report.append({"path": str(resolved_path), "status": LINK_STATUS["CREATED"]})
            dir_exists = mkdir(resolved_dest)
            if dir_exists == DIR_STATUS["FILE_EXISTS"]:
                report[-1]["status"] = dir_exists
                continue
            exists = link_exists(resolved_path, resolved_dest)
            if exists == LINK_STATUS["OK"]:
                report[-1]["status"] = STATUSES["LINK_OK"]
            elif exists == LINK_STATUS["EXISTS"]:
                report[-1]["status"] = STATUSES["EXISTS"]
                ret["failed"] = True
            elif exists == LINK_STATUS["DOESNT_EXIST"]:
                # Attempt to link.
                link(resolved_path, resolved_dest)
        
        ret["files"] = report
        return ret


def main():
    dotfiles = Dotfiles(get_dotfiles())
    report = dotfiles.link_all()
    json.dump(report, fp=sys.stdout, indent=2)
    print()


if __name__ == "__main__":
    main()

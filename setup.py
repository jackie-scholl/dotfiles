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
if not sys.version_info[0] == 3:
    raise Error("wrong python version! must be 3")

def get_dotfiles():
    """
    Reads out all the files in the directory, except those that should not be
    used, like .git/ and this file (setup.py)
    """
    return [x for x
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

    def __init__(self, dotfiles):
        """Creates a new Dotfiles instance.
        """
        self.dotfiles = list(dotfiles)
        
        self.hostname = socket.getfqdn()
        self.platform = sys.platform

    def link_all(self):
        """Creates links for all dotfiles represented by this instance.

        Returns: A JSON report describing the changes made.
        """
        ret = {"changed": False, "failed": False}
        report = []

        def link(path, dest):
            result = {"path": str(path), "dest": str(dest)}
            
            try:
                dest.symlink_to(path, target_is_directory=dest.is_dir())
                result["status"] = LINK_STATUS["CREATED"]
                ret["changed"] = True
            except OSError as err:
                result["status"] = STATUSES["ERRORED"]
                result["error"] = os_error_to_dict(err)
                ret["failed"] = True
            finally:
                report.append(result)

        # returns a LINK_STATUS
        def handle_dotfile(dotfile):
            resolved_path = dotfile.resolve()
            resolved_dest = Path.home().joinpath(dotfile)

            dir_exists = mkdir(resolved_dest)
            if dir_exists == DIR_STATUS["FILE_EXISTS"]:
                return dir_exists
            exists = link_exists(resolved_path, resolved_dest)
            if exists == LINK_STATUS["OK"]:
                return STATUSES["LINK_OK"]
            elif exists == LINK_STATUS["EXISTS"]:
                ret["failed"] = True
                return STATUSES["EXISTS"]
            elif exists == LINK_STATUS["DOESNT_EXIST"]:
                # Attempt to link.
                link(resolved_path, resolved_dest)
                return LINK_STATUS["CREATED"]

        for dotfile in self.dotfiles:
            link_status = handle_dotfile(dotfile)
            report.append({"path": str(dotfile.resolve()), "status": link_status})
            
        ret["files"] = report
        return ret


def main():
    dotfiles = Dotfiles(get_dotfiles())
    report = dotfiles.link_all()
    json.dump(report, fp=sys.stdout, indent=2)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""`chdir(2)`: what it refuses, and what `getcwd(3)` says afterwards.

    python3 chdir.py                                            # this machine
    container run --rm -v "$PWD:/probe" --user 1000:1000 \
        python:3-slim python3 /probe/chdir.py                   # Linux

Captured columns: measured-darwin.txt, measured-linux.txt.

Every row runs in a fresh tree, and every operand is relative to it, so no row
can name anything outside. `chdir` changes process state, so each row restores
the starting directory before the next.
"""

import ctypes
import ctypes.util
import errno
import os
import shutil
import tempfile

libc = ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True)
libc.chdir.argtypes = [ctypes.c_char_p]
libc.chdir.restype = ctypes.c_int


def chdir(path):
    ctypes.set_errno(0)
    return libc.chdir(path), ctypes.get_errno()


def nm(e):
    return errno.errorcode.get(e, str(e))


def build(root):
    os.mkdir(os.path.join(root, "d"))
    os.mkdir(os.path.join(root, "d", "sub"))
    open(os.path.join(root, "f"), "w").close()
    os.symlink("d", os.path.join(root, "ld"))
    os.symlink("f", os.path.join(root, "lf"))
    os.symlink("nx", os.path.join(root, "dang"))
    # Search but not read, and read but not search: the pair that says which
    # bit `chdir` actually wants.
    os.mkdir(os.path.join(root, "xonly"))
    os.mkdir(os.path.join(root, "ronly"))
    os.chmod(os.path.join(root, "xonly"), 0o100)
    os.chmod(os.path.join(root, "ronly"), 0o400)


def widen(root):
    for name in ("xonly", "ronly"):
        try:
            os.chmod(os.path.join(root, name), 0o700)
        except OSError:
            pass


def main():
    home = os.getcwd()
    print(f"# {os.uname().sysname} {os.uname().release} {os.uname().machine}, uid {os.getuid()}")

    rows = [
        ("a directory", b"d"),
        ("a directory, trailing separator", b"d/"),
        ("nested", b"d/sub"),
        ("a regular file", b"f"),
        ("a regular file, trailing separator", b"f/"),
        ("a symlink to a directory", b"ld"),
        ("a symlink to a directory, trailing sep", b"ld/"),
        ("a symlink to a file", b"lf"),
        ("a dangling symlink", b"dang"),
        ("absent", b"nx"),
        ("the empty path", b""),
        ("search bit only (0o100)", b"xonly"),
        ("read bit only (0o400)", b"ronly"),
        ("a 300-byte name", b"z" * 300),
        (".", b"."),
        ("..", b".."),
    ]

    for label, operand in rows:
        root = os.path.realpath(tempfile.mkdtemp(prefix="cd-"))
        try:
            build(root)
            os.chdir(root)
            rc, e = chdir(operand)
            # getcwd is the other half of the question: which path does a
            # successful chdir leave behind, the one named or the physical one?
            if rc == 0:
                try:
                    where = os.getcwd().replace(root, "<root>")
                except OSError as err:
                    where = f"getcwd failed {nm(err.errno)}"
                result = f"ok, getcwd={where}"
            else:
                result = f"errno={nm(e)}"
            shown = operand if len(operand) <= 12 else operand[:12] + b"..(%d)" % len(operand)
            print(f"{label:40} chdir({shown!r:20}) -> {result}")
        finally:
            os.chdir(home)
            widen(root)
            shutil.rmtree(root, ignore_errors=True)

    # The one row that needs the directory to vanish underneath the process.
    root = os.path.realpath(tempfile.mkdtemp(prefix="cd-"))
    try:
        gone = os.path.join(root, "gone")
        os.mkdir(gone)
        os.chdir(gone)
        os.rmdir(gone)
        try:
            where = os.getcwd()
        except OSError as err:
            where = f"getcwd failed {nm(err.errno)}"
        rc, e = chdir(b".")
        label = 'chdir(".") in an rmdired cwd'
        print(f"{label:40} -> rc={rc} errno={nm(e)}; before it, getcwd={where}")
    finally:
        os.chdir(home)
        shutil.rmtree(root, ignore_errors=True)


main()

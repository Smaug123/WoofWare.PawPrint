#!/usr/bin/env python3
"""`rename(2)`'s *walk order*: which of two bad paths is reported, and how much
of each path the kernel settles before it looks at the other.

`rename.py` beside this measures what a rename owes once both paths have
resolved. This measures the order the resolving happens in, which is a separate
divergence and is invisible to any row where the two paths earn the same errno
-- so every row here is built from a pair that *disagrees*.

    python3 walk-order.py                                          # this machine
    container run --rm -v "$PWD:/probe" --user 1000:1000 \
        python:3-slim python3 /probe/walk-order.py                 # Linux

Captured columns: measured-walk-order-darwin.txt, measured-walk-order-linux.txt.
"""

import ctypes
import ctypes.util
import errno
import os
import tempfile

libc = ctypes.CDLL(ctypes.util.find_library("c"), use_errno=True)
libc.rename.argtypes = [ctypes.c_void_p, ctypes.c_void_p]
libc.rename.restype = ctypes.c_int

# Page zero: never mapped, so the kernel's copy-in of this pathname is EFAULT.
BOGUS = 0x1


def rename(old, new):
    ctypes.set_errno(0)
    rc = libc.rename(old, new)
    return rc, ctypes.get_errno()


def buf(b):
    return ctypes.cast(ctypes.create_string_buffer(b), ctypes.c_void_p)


def nm(e):
    return errno.errorcode.get(e, str(e))


def show(b):
    if not isinstance(b, bytes):
        return "<unmapped>"
    return repr(b if len(b) <= 12 else b[:12] + b"..(%d)" % len(b))


def run(section, note, rows):
    print()
    print(f"## {section}")
    print(f"# {note}")
    for label, old, new in rows:
        rc, e = rename(buf(old) if isinstance(old, bytes) else old, buf(new) if isinstance(new, bytes) else new)
        print(f"{label:34} rename({show(old)}, {show(new)}) -> rc={rc} errno={nm(e)}")
        if rc == 0:
            raise SystemExit(f"row {label!r} SUCCEEDED; it is not measuring what it names")


def main():
    with tempfile.TemporaryDirectory() as d:
        os.chdir(os.path.realpath(d))
        open("f", "w").close()
        os.mkdir("dir")
        os.mkdir("dir/sub")
        os.mkdir("nosearch")
        open("nosearch/kid", "w").close()
        os.symlink("dir", "ld")
        os.symlink("f", "lf")
        os.symlink("nx", "dangling")
        os.chmod("nosearch", 0o600)

        # Over both PATH_MAX values (Darwin 1024, Linux 4096), and between them:
        # the middle length is what tells "the check fired" from "the argument
        # was merely long", since it is over Darwin's limit and under Linux's.
        over = b"z" * 5000
        mid = b"z" * 2000
        long_component = b"z" * 300

        # A *free final name* cannot tell the two orderings apart: it is not a
        # parent-walk failure, so the source's parent resolves either way and
        # the destination's pathname is reached either way. The discriminating
        # sources are the ones whose parent walk itself fails.
        run(
            "when is each pathname copied in",
            "getname()/copyinstr on the destination, against sources that fail at different phases",
            [
                ("control: both absent, short", b"nope", b"alsonope"),
                ("control: over-long source", over, b"alsonope"),
                ("free source name, over-long dest", b"nope", over),
                ("free source name, mid-length dest", b"nope", mid),
                ("existing source, over-long dest", b"f", over),
                ("DISCRIMINATOR: source parent absent", b"nodir/kid", over),
                ("DISCRIMINATOR: source parent unsearchable", b"nosearch/kid", over),
                ("DISCRIMINATOR: source parent is a file", b"f/kid", over),
            ],
        )

        run(
            "and the same question for EFAULT",
            "an unreadable pathname pointer, which getname() reports the same way it reports length",
            [
                ("control: bogus source", BOGUS, b"alsonope"),
                ("absent source, bogus dest", b"nope", BOGUS),
                ("over-long source, bogus dest", over, BOGUS),
                ("existing source, bogus dest", b"f", BOGUS),
            ],
        )

        # One destination throughout, whose parent is a regular file, so the
        # destination alone answers ENOTDIR. Any other errno is a source-side
        # refusal that ran first; ENOTDIR means the source had not been judged.
        dest = b"f/x"

        run(
            "how much of the source is settled first",
            f"destination is always {dest!r}, which answers ENOTDIR on its own",
            [
                ("free name", b"nope", dest),
                ("regular file (control)", b"f", dest),
                ("a directory", b"dir", dest),
                ("a directory, trailing sep", b"dir/", dest),
                ("symlink to a directory", b"ld", dest),
                ("symlink to a file", b"lf", dest),
                ("dangling symlink", b"dangling", dest),
                ("300-byte final component", long_component, dest),
                ("under an unsearchable parent", b"nosearch/kid", dest),
                ("parent absent", b"nodir/kid", dest),
                ("parent is a regular file", b"f/kid", dest),
            ],
        )

        run(
            "which navigation the source settles first",
            f"destination is always {dest!r}; only the bare root is early",
            [
                ("the root", b"/", dest),
                ("the root as /.", b"/.", dest),
                ("the root as /..", b"/..", dest),
                ("the root as /dev/..", b"/dev/..", dest),
                (". here", b".", dest),
                (".. here", b"..", dest),
                ("dir/.", b"dir/.", dest),
                ("dir/..", b"dir/..", dest),
                ("dir/sub/..", b"dir/sub/..", dest),
            ],
        )

        # Every relative path's parent is now an orphan: a directory that still
        # exists, because this process is in it, but that no path reaches. It is
        # the only way to reach that state, and it is what
        # `RenameRules.*Verdict`'s orphaned-destination-parent arm is about.
        base = os.getcwd()
        os.mkdir("gone")
        os.chdir(os.path.join(base, "gone"))
        os.rmdir(os.path.join(base, "gone"))

        run(
            "an orphaned destination parent, against the final lookups",
            "the cwd is an rmdir'd orphan, so a relative destination's parent is one",
            [
                ("control: short destination name", os.path.join(base, "f").encode(), b"x"),
                ("DISCRIMINATOR: 300-byte destination name", os.path.join(base, "f").encode(), b"z" * 300),
                ("300-byte *source* final name", os.path.join(base, "z" * 300).encode(), b"x"),
                ("absent absolute source", os.path.join(base, "nope").encode(), b"x"),
            ],
        )

        os.chdir(base)


print(f"# {os.uname().sysname} {os.uname().release} {os.uname().machine}, uid {os.getuid()}")
main()

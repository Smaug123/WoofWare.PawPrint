#!/usr/bin/env python3
"""What `rename(2)` refuses, and in which order, measured on a real kernel.

This is the probe behind `RenameRules` in `WoofWare.PawPrint/EmulatedKernel.fs`
and `TestRenameRules.fs`. Re-run it rather than re-deriving those tables; a row
that disagrees is a measurement, not a bug report about the probe.

    python3 rename.py                    # this machine
    python3 rename.py /Volumes/SOMEMOUNT # also the mount-root section

    container run --rm -v "$PWD:/probe" --user 1000:1000 \
        python:3-slim python3 /probe/rename.py     # Linux, unprivileged
    container run --rm -v "$PWD:/probe" python:3-slim python3 /probe/rename.py

Run it unprivileged *and* as root: privilege removes one arm of the ordering,
so neither column is derivable from the other. Build the trees on a real
filesystem -- inside a container, /tmp on the container's own block device is
ext4, while a bind-mounted host directory is virtiofs and answers differently.

Two rows are unmeasurable rather than merely unmeasured, and the probe says so
rather than printing a number:

  * a rename whose *source* is a mount root reached by "." or ".." on Darwin.
    A mount root's parent directory is on another filesystem by construction,
    so EXDEV pre-empts whatever rename(2) would otherwise answer -- confirmed
    on a freshly created APFS disk image, where the same navigation as a
    *destination* answers EINVAL and so is measured.
  * anything naming "/" while running as root. Those rows are refusals on both
    kernels and create nothing, but a probe run under sudo must not be the
    thing that discovers otherwise on somebody's real root filesystem. The
    unprivileged runs pin them, and privilege does not participate in either
    kernel's structural checks.
"""

import errno
import os
import shutil
import stat
import sys
import tempfile

BASE = sys.argv[1] if len(sys.argv) > 1 else None

LONG = "n" * 300

ROWS = []


def errname(e):
    return errno.errorcode.get(e, "errno %d" % e)


def widen(root):
    """Restore search+write everywhere, so a narrowed tree can be torn down."""
    for here, dirs, _files in os.walk(root, topdown=True):
        for d in dirs:
            p = os.path.join(here, d)
            try:
                if not os.path.islink(p):
                    os.chmod(p, 0o755)
            except OSError:
                pass
    try:
        os.chmod(root, 0o755)
    except OSError:
        pass


def names_the_real_root(path):
    """Whether `path` would hand rename(2) the actual filesystem root.

    Not a string test: "/", "/.", "/.." and a symlink whose target is "/"
    followed by a separator all reach it, and an earlier version of this guard
    caught only the first. `realpath` resolves every one of those, and answers
    the *lexical* parent for a trailing "..", which is what rename(2) will
    resolve too.
    """
    try:
        return os.path.realpath(path) == "/"
    except OSError:
        return False


def row(section, label, build, src, dst):
    """Build a fresh tree, rename src -> dst once, and record what happened.

    An argument of exactly "/" is passed through verbatim; anything else is
    joined onto the tree's own root, so no row can name a path outside it.

    As root, any operand that *resolves* to the real filesystem root is skipped
    rather than run. Those rows are refusals on both kernels and create nothing,
    but a probe run under sudo must not be the thing that discovers otherwise on
    somebody's real root. The unprivileged runs pin them, and privilege does not
    participate in either kernel's structural checks.
    """
    root = os.path.realpath(tempfile.mkdtemp(prefix="rn-"))
    try:
        build(root)
        srcpath = src if src.startswith("/") else os.path.join(root, src)
        dstpath = dst if dst.startswith("/") else os.path.join(root, dst)
        if os.geteuid() == 0 and (names_the_real_root(srcpath) or names_the_real_root(dstpath)):
            ROWS.append((section, label, 'skipped (resolves to "/" and this process is root)'))
            return
        try:
            os.rename(srcpath, dstpath)
            result = "ok"
        except OSError as e:
            result = errname(e.errno)
    finally:
        widen(root)
        shutil.rmtree(root, ignore_errors=True)
    ROWS.append((section, label, result))


# --------------------------------------------------------------- the objects

KINDS = ["file", "emptydir", "fulldir", "symfile", "symdir", "dangling", "fifo", "absent"]


def make(root, name, kind):
    """One object of each kind rename can be handed, bound at `name`."""
    p = os.path.join(root, name)
    if kind == "file":
        open(p, "w").close()
    elif kind == "emptydir":
        os.mkdir(p)
    elif kind == "fulldir":
        os.mkdir(p)
        open(os.path.join(p, "kid"), "w").close()
    elif kind == "symfile":
        open(p + "-t", "w").close()
        os.symlink(p + "-t", p)
    elif kind == "symdir":
        os.mkdir(p + "-t")
        os.symlink(p + "-t", p)
    elif kind == "dangling":
        os.symlink(os.path.join(root, "nowhere"), p)
    elif kind == "fifo":
        os.mkfifo(p)
    elif kind == "absent":
        pass
    else:
        raise AssertionError(kind)


def type_matrix():
    """Every source kind against every destination kind. Unanimous, and it
    collapses to: the only thing that matters about either object is whether it
    is a directory."""
    for s in KINDS:
        for d in KINDS:
            def build(root, s=s, d=d):
                make(root, "s", s)
                make(root, "d", d)
            row("type", "%s -> %s" % (s, d), build, "s", "d")


# ------------------------------------------------------------- structural

def structural():
    def nested(root):
        os.makedirs(os.path.join(root, "a", "b", "c"))
    row("struct", "rename(a, a/b) -- into own subtree", nested, "a", "a/b")
    row("struct", "rename(a/b, a/b/c/x) -- into own subtree", nested, "a/b", "a/b/c/x")

    def linked(root):
        os.makedirs(os.path.join(root, "a", "b", "inner"))
        os.symlink(os.path.join(root, "a", "b"), os.path.join(root, "link"))
    row("struct", "rename(a, link/inner) where link -> a/b", linked, "a", "link/inner")

    def withc(root):
        os.makedirs(os.path.join(root, "a", "b"))
    row("struct", "rename(a, a/b/../c) -- dest parent resolves back to a", withc, "a", "a/b/../c")
    row("struct", "rename(a/b/c, a/x) -- out of own subtree", nested, "a/b/c", "a/x")

    def prefix(root):
        os.mkdir(os.path.join(root, "a"))
        os.mkdir(os.path.join(root, "ab"))
    row("struct", "rename(a, ab) -- prefix, not ancestor (occupied)", prefix, "a", "ab")

    def justa(root):
        os.mkdir(os.path.join(root, "a"))
    row("struct", "rename(a, ab) -- prefix, not ancestor (free)", justa, "a", "ab")

    def onefile(root):
        open(os.path.join(root, "f"), "w").close()
    row("struct", "rename(f, f) -- self, file", onefile, "f", "f")
    row("struct", "rename(f, ./f) -- self via .", onefile, "f", "./f")
    row("struct", "rename(d, d) -- self, directory", justa, "a", "a")

    def fulldirself(root):
        os.mkdir(os.path.join(root, "d"))
        open(os.path.join(root, "d", "kid"), "w").close()
    row("struct", "rename(d, d) -- self, NON-EMPTY directory", fulldirself, "d", "d")

    def hardlink(root):
        open(os.path.join(root, "f"), "w").close()
        os.link(os.path.join(root, "f"), os.path.join(root, "g"))
    row("struct", "rename(f, g) where g is a hard link to f", hardlink, "f", "g")
    row("struct", "rename(g, f) -- the same pair, arguments reversed", hardlink, "g", "f")

    def linkself(root):
        open(os.path.join(root, "f"), "w").close()
        os.symlink(os.path.join(root, "f"), os.path.join(root, "lf"))
    row("struct", "rename(lf, f) where lf -> f (two inodes, so NOT a no-op)", linkself, "lf", "f")

    row("struct", "source absent", justa, "nope", "x")
    row("struct", "destination's parent absent", onefile, "f", "nodir/x")

    def dstparentfile(root):
        open(os.path.join(root, "f"), "w").close()
        open(os.path.join(root, "g"), "w").close()
    row("struct", "destination's parent is a regular file", dstparentfile, "f", "g/x")
    row("struct", "destination component 300 bytes", onefile, "f", LONG)
    row("struct", "source component 300 bytes", onefile, LONG, "g")

    def childontoparent(root):
        os.makedirs(os.path.join(root, "a", "b"))
    row("struct", "rename(a/b, a) -- destination is an ANCESTOR of the source", childontoparent, "a/b", "a")

    def subtree_freename(root):
        os.makedirs(os.path.join(root, "a", "b"))
    row("struct", "rename(a, a/b/free) -- into own subtree, destination name free",
        subtree_freename, "a", "a/b/free")

    def subtree_file(root):
        os.makedirs(os.path.join(root, "a", "b"))
        open(os.path.join(root, "a", "b", "f"), "w").close()
    row("struct", "rename(a, a/b/f) -- into own subtree AND destination is a regular file",
        subtree_file, "a", "a/b/f")


def navigation():
    """"." / ".." / "/" in each position. Linux spends one errno on all of them;
    Darwin spends three and splits the "/" destination by the source's kind."""
    def dirs(root):
        os.mkdir(os.path.join(root, "d"))
        open(os.path.join(root, "x"), "w").close()
        os.mkdir(os.path.join(root, "e"))
        os.mkdir(os.path.join(root, "full"))
        open(os.path.join(root, "full", "kid"), "w").close()
    row("nav", "rename(d/., x)", dirs, "d/.", "x2")
    row("nav", "rename(d/.., x)", dirs, "d/..", "x2")
    row("nav", "rename(x, d/.)", dirs, "x", "d/.")
    row("nav", "rename(x, d/..)", dirs, "x", "d/..")
    row("nav", 'rename("/", x)', dirs, "/", "x2")
    row("nav", 'rename("/.", x)', dirs, "/.", "x2")
    row("nav", 'rename("/..", x)', dirs, "/..", "x2")
    row("nav", 'rename(x, "/.")', dirs, "x", "/.")
    row("nav", 'rename(dir, "/")', dirs, "d", "/")
    row("nav", 'rename(file, "/")', dirs, "x", "/")
    row("nav", 'rename(fulldir, "/")', dirs, "full", "/")
    row("nav", 'rename(emptydir, "e/..")', dirs, "d", "e/..")
    row("nav", 'rename(fulldir, "e/..")', dirs, "full", "e/..")
    row("nav", 'rename(file, "e/..")', dirs, "x", "e/..")

    def nonancestor(root):
        os.makedirs(os.path.join(root, "A", "d"))
        os.makedirs(os.path.join(root, "B", "sub"))
    row("nav", 'rename(A/d, B/sub/..) -- the ".." is NOT an ancestor of the source',
        nonancestor, "A/d", "B/sub/..")

    def nonancestor_file(root):
        os.mkdir(os.path.join(root, "A"))
        open(os.path.join(root, "A", "x"), "w").close()
        os.makedirs(os.path.join(root, "B", "sub"))
    row("nav", 'rename(A/x, B/sub/..) -- file source, ".." not an ancestor',
        nonancestor_file, "A/x", "B/sub/..")

    def rootlink(root):
        os.mkdir(os.path.join(root, "d"))
        open(os.path.join(root, "f"), "w").close()
        os.symlink("/", os.path.join(root, "lroot"))
    row("nav", 'rename("lroot/.", x) where lroot -> "/"', rootlink, "lroot/.", "x2")
    row("nav", 'rename(d, "lroot/") where lroot -> "/"', rootlink, "d", "lroot/")
    row("nav", 'rename(f, "lroot/") where lroot -> "/"', rootlink, "f", "lroot/")

    def dirsonly(root):
        os.mkdir(os.path.join(root, "d"))
        os.mkdir(os.path.join(root, "e"))
        open(os.path.join(root, "x"), "w").close()
    row("nav", "rename(d/., nodir/y) -- source is a navigation AND destination parent absent",
        dirsonly, "d/.", "nodir/y")
    row("nav", "rename(nope, d/.) -- source absent AND destination is a navigation",
        dirsonly, "nope", "d/.")

    def unwritable(root):
        os.mkdir(os.path.join(root, "p"))
        os.mkdir(os.path.join(root, "p", "d"))
        open(os.path.join(root, "p", "x"), "w").close()
        os.chmod(os.path.join(root, "p"), 0o555)
    row("nav", "rename(p/x, p/d/..) -- destination is a navigation AND its parent unwritable",
        unwritable, "p/x", "p/d/..")


# ------------------------------------------------------------- permissions

def permissions():
    """Which directory's write bit each kernel demands, with only one bit
    removed at a time so nothing masks anything."""
    def narrowed(which, mode):
        def build(root):
            os.mkdir(os.path.join(root, "p"))
            open(os.path.join(root, "p", "f"), "w").close()
            os.mkdir(os.path.join(root, "p", "m"))
            os.mkdir(os.path.join(root, "q"))
            os.chmod(os.path.join(root, which), mode)
        return build

    row("perm", "write missing on the source's parent", narrowed("p", 0o555), "p/f", "q/g")
    row("perm", "write missing on the destination's parent", narrowed("q", 0o555), "p/f", "q/g")
    row("perm", "search missing on the source's parent", narrowed("p", 0o666), "p/f", "q/g")
    row("perm", "search missing on the destination's parent", narrowed("q", 0o666), "p/f", "q/g")

    def movedirnowrite(root):
        os.mkdir(os.path.join(root, "p"))
        os.mkdir(os.path.join(root, "p", "m"))
        os.mkdir(os.path.join(root, "q"))
        os.chmod(os.path.join(root, "p", "m"), 0o555)
    row("perm", "directory to a NEW parent, write missing on the moved directory",
        movedirnowrite, "p/m", "q/m")

    def movednosearch(root):
        os.mkdir(os.path.join(root, "p"))
        os.mkdir(os.path.join(root, "p", "m"))
        os.mkdir(os.path.join(root, "q"))
        os.chmod(os.path.join(root, "p", "m"), 0o666)
    row("perm", "directory to a NEW parent, SEARCH missing on the moved directory",
        movednosearch, "p/m", "q/m")

    def renamedirnowrite(root):
        os.mkdir(os.path.join(root, "p"))
        os.mkdir(os.path.join(root, "p", "m"))
        os.chmod(os.path.join(root, "p", "m"), 0o555)
    row("perm", "directory renamed WITHIN its parent, write missing on the moved directory",
        renamedirnowrite, "p/m", "p/m2")

    def filemovenowrite(root):
        os.mkdir(os.path.join(root, "p"))
        open(os.path.join(root, "p", "f"), "w").close()
        os.mkdir(os.path.join(root, "q"))
        os.chmod(os.path.join(root, "p", "f"), 0o000)
    row("perm", 'file to a new parent, mode 0 on the moved file (files have no "..")',
        filemovenowrite, "p/f", "q/f")

    def absentunder(mode):
        def build(root):
            os.mkdir(os.path.join(root, "p"))
            os.mkdir(os.path.join(root, "q"))
            os.chmod(os.path.join(root, "p"), mode)
        return build

    row("perm", "source absent under an unsearchable parent", absentunder(0o666), "p/nope", "q/g")
    row("perm", "source absent under a searchable but unwritable parent", absentunder(0o555), "p/nope", "q/g")

    def sticky(root):
        os.mkdir(os.path.join(root, "p"))
        open(os.path.join(root, "p", "f"), "w").close()
        os.mkdir(os.path.join(root, "q"))
        os.chmod(os.path.join(root, "q"), 0o1777)
    row("perm", "sticky destination parent", sticky, "p/f", "q/g")


def ordering():
    """Pairs where two checks both refuse, so the errno says which was asked
    first. A pair whose two checks carry the same errno proves nothing and is
    labelled as the control it is."""
    def pair(srckind, dstkind, narrow=None, dstmode=None, srcmode=None):
        def build(root):
            os.mkdir(os.path.join(root, "p"))
            os.mkdir(os.path.join(root, "q"))
            s = os.path.join(root, "p", "s")
            os.mkdir(s) if srckind.endswith("dir") else open(s, "w").close()
            if srckind == "fulldir":
                open(os.path.join(s, "kid"), "w").close()
            if dstkind != "absent":
                d = os.path.join(root, "q", "d")
                if dstkind == "file":
                    open(d, "w").close()
                elif dstkind == "hardlink":
                    os.link(s, d)
                else:
                    os.mkdir(d)
                    if dstkind == "fulldir":
                        open(os.path.join(d, "kid"), "w").close()
                if dstmode is not None:
                    os.chmod(d, dstmode)
            if srcmode is not None:
                os.chmod(s, srcmode)
            if narrow is not None:
                os.chmod(os.path.join(root, narrow), 0o555)
        return build

    row("order", "src parent unwritable AND destination is a directory",
        pair("file", "emptydir", narrow="p"), "p/s", "q/d")
    row("order", "dst parent unwritable AND destination is a directory",
        pair("file", "emptydir", narrow="q"), "p/s", "q/d")
    row("order", "dst parent unwritable AND destination is a NON-EMPTY directory",
        pair("dir", "fulldir", narrow="q"), "p/s", "q/d")
    row("order", "src parent unwritable AND directory source over a FILE destination",
        pair("dir", "file", narrow="p"), "p/s", "q/d")
    row("order", "same-inode no-op AND the SOURCE's parent unwritable",
        pair("file", "hardlink", narrow="p"), "p/s", "q/d")
    row("order", "same-inode no-op AND the DESTINATION's parent unwritable",
        pair("file", "hardlink", narrow="q"), "p/s", "q/d")
    row("order", "moved directory unwritable AND destination is a regular file",
        pair("dir", "file", srcmode=0o555), "p/s", "q/d")
    row("order", "moved directory unwritable AND destination is an empty directory",
        pair("dir", "emptydir", srcmode=0o555), "p/s", "q/d")
    row("order", "moved directory unwritable AND destination is a NON-EMPTY directory",
        pair("dir", "fulldir", srcmode=0o555), "p/s", "q/d")
    row("order", "dst parent unwritable AND source absent",
        pair("file", "absent", narrow="q"), "p/nope", "q/d")
    row("order", "dst parent unwritable AND source name 300 bytes",
        pair("file", "absent", narrow="q"), "p/" + LONG, "q/d")
    row("order", "src parent unwritable AND destination name 300 bytes",
        pair("file", "absent", narrow="p"), "p/s", "q/" + LONG)
    row("order", "src parent unwritable AND trailing separator on a file source",
        pair("file", "absent", narrow="p"), "p/s/", "q/d")

    def selfrename(mode, kind, nonempty=False, parentmode=0o755):
        def build(root):
            os.mkdir(os.path.join(root, "p"))
            s = os.path.join(root, "p", "s")
            os.mkdir(s) if kind == "dir" else open(s, "w").close()
            if nonempty:
                open(os.path.join(s, "kid"), "w").close()
            if kind == "dir":
                os.chmod(s, mode)
            os.chmod(os.path.join(root, "p"), parentmode)
        return build

    row("order", "self-rename of a file AND parent unwritable",
        selfrename(0o755, "file", parentmode=0o555), "p/s", "p/s")
    row("order", "self-rename of a directory AND parent unwritable",
        selfrename(0o755, "dir", parentmode=0o555), "p/s", "p/s")
    row("order", "self-rename of a directory whose own write bit is missing",
        selfrename(0o555, "dir"), "p/s", "p/s")
    row("order", "self-rename of a NON-EMPTY directory whose own write bit is missing",
        selfrename(0o555, "dir", nonempty=True), "p/s", "p/s")
    row("order", "self-rename of a directory, own write bit present (control)",
        selfrename(0o755, "dir"), "p/s", "p/s")

    def subtree(narrow=None, dstkind=None):
        def build(root):
            os.makedirs(os.path.join(root, "p", "a", "b"))
            if dstkind == "file":
                open(os.path.join(root, "p", "a", "b", "f"), "w").close()
            elif dstkind == "fulldir":
                open(os.path.join(root, "p", "a", "b", "kid"), "w").close()
            if narrow is not None:
                os.chmod(os.path.join(root, narrow), 0o555)
        return build

    row("order", "into own subtree AND the source's parent unwritable",
        subtree(narrow="p"), "p/a", "p/a/b")
    row("order", "into own subtree AND the moved directory unwritable",
        subtree(narrow="p/a"), "p/a", "p/a/b")
    row("order", "into own subtree AND destination non-empty",
        subtree(dstkind="fulldir"), "p/a", "p/a/b")
    row("order", "into own subtree AND destination is a regular file",
        subtree(dstkind="file"), "p/a", "p/a/b/f")

    def noop_trailing(root):
        open(os.path.join(root, "f"), "w").close()
        os.link(os.path.join(root, "f"), os.path.join(root, "g"))
    row("order", "same-inode no-op AND a trailing separator on the destination",
        noop_trailing, "f", "g/")

    def self_trailing(root):
        open(os.path.join(root, "f"), "w").close()
    row("order", "self-rename AND a trailing separator on the destination",
        self_trailing, "f", "f/")

    def file_over_fulldir(root):
        open(os.path.join(root, "f"), "w").close()
        os.mkdir(os.path.join(root, "d"))
        open(os.path.join(root, "d", "kid"), "w").close()
    row("order", "non-directory source AND non-empty directory destination",
        file_over_fulldir, "f", "d")


def displaced_directory():
    """Darwin consults the *displaced* directory's own write bit when a
    directory replaces a directory, and never looks at the directory holding
    it; Linux does the exact opposite. Each row varies one mode."""
    def build(qmode, dmode, srckind="dir", dstkind="emptydir"):
        def go(root):
            os.mkdir(os.path.join(root, "p"))
            os.mkdir(os.path.join(root, "q"))
            s = os.path.join(root, "p", "s")
            os.mkdir(s) if srckind == "dir" else open(s, "w").close()
            d = os.path.join(root, "q", "d")
            if dstkind == "file":
                open(d, "w").close()
            else:
                os.mkdir(d)
                if dstkind == "fulldir":
                    open(os.path.join(d, "kid"), "w").close()
            os.chmod(d, dmode)
            os.chmod(os.path.join(root, "q"), qmode)
        return go

    for qmode, dmode, note in [
        (0o755, 0o755, "control: everything writable"),
        (0o555, 0o755, "holder unwritable, displaced writable"),
        (0o755, 0o000, "holder writable, displaced mode 0"),
        (0o755, 0o555, "holder writable, displaced read+search only"),
        (0o755, 0o300, "holder writable, displaced write+search only"),
        (0o555, 0o300, "holder unwritable, displaced write+search only"),
        (0o555, 0o200, "holder unwritable, displaced write only"),
        (0o444, 0o755, "holder unsearchable: the walk answers, not the verdict"),
    ]:
        row("displaced", "dir -> emptydir, q=%04o d=%04o (%s)" % (qmode, dmode, note),
            build(qmode, dmode), "p/s", "q/d")

    row("displaced", "dir -> NON-EMPTY dir, q=0755 d=0000 (EACCES against ENOTEMPTY)",
        build(0o755, 0o000, dstkind="fulldir"), "p/s", "q/d")
    row("displaced", "dir -> FILE, q=0755 d=0000 (control: the type rule pre-empts)",
        build(0o755, 0o000, dstkind="file"), "p/s", "q/d")
    row("displaced", "file -> file, q=0755 d=0000 (control: a non-directory's own mode is never asked)",
        build(0o755, 0o000, srckind="file", dstkind="file"), "p/s", "q/d")
    row("displaced", "file -> file, q=0555 d=0777 (control: the holder still refuses)",
        build(0o555, 0o777, srckind="file", dstkind="file"), "p/s", "q/d")

    def within_parent(dmode):
        def go(root):
            os.mkdir(os.path.join(root, "a"))
            os.mkdir(os.path.join(root, "b"))
            os.chmod(os.path.join(root, "b"), dmode)
        return go

    row("displaced", "within one parent: dir a -> empty dir b, b=0000", within_parent(0o000), "a", "b")
    row("displaced", "within one parent: dir a -> empty dir b, b=0755 (control)",
        within_parent(0o755), "a", "b")


# --------------------------------------------------------- trailing separators

def trailing():
    """The walk each kernel resolves both paths with: Linux never traverses a
    final symlink and enforces the demand afterwards; Darwin traverses it and
    then demands a directory. The two therefore destroy different objects."""
    def linkdir(root):
        os.mkdir(os.path.join(root, "real"))
        os.mkdir(os.path.join(root, "src"))
        os.symlink(os.path.join(root, "real"), os.path.join(root, "s"))
        open(os.path.join(root, "f"), "w").close()
    row("trail", 'rename("s/", "moved") where s -> a real directory', linkdir, "s/", "moved")
    row("trail", 'rename("src", "s/") where s -> a real directory', linkdir, "src", "s/")
    row("trail", 'rename("f", "s/") where s -> a real directory', linkdir, "f", "s/")
    row("trail", 'rename("s", "moved") -- the same link with no separator', linkdir, "s", "moved")

    def objects(root):
        os.mkdir(os.path.join(root, "d"))
        os.mkdir(os.path.join(root, "d2"))
        open(os.path.join(root, "f"), "w").close()
        open(os.path.join(root, "t"), "w").close()
        os.symlink("t", os.path.join(root, "lf"))
        os.symlink("nowhere", os.path.join(root, "dang"))
        os.symlink("/", os.path.join(root, "lroot"))
    row("trail", 'rename("f/", "g") -- separator on a regular-file source', objects, "f/", "g")
    # The symlink targets in `objects` are *relative*, and that is load-bearing
    # rather than incidental: with an absolute target this row is nondeterministic
    # on macOS. See `unstable_trailing_symlink` below.
    row("trail", 'rename("lf/", "g") -- separator on a symlink to a file', objects, "lf/", "g")
    row("trail", 'rename("dang/", "g") -- separator on a dangling symlink', objects, "dang/", "g")
    row("trail", 'rename("lroot/", "g") where lroot -> "/"', objects, "lroot/", "g")
    row("trail", 'rename("nope/", "g") -- separator on an absent source', objects, "nope/", "g")
    row("trail", 'rename("d/", "moved") -- separator on a directory source', objects, "d/", "moved")
    row("trail", 'rename("d/", "d2/") -- separators on both', objects, "d/", "d2/")
    row("trail", 'rename("f", "absent/") -- file source, free destination + separator', objects, "f", "absent/")
    row("trail", 'rename("d", "absent/") -- directory source, free destination + separator',
        objects, "d", "absent/")
    row("trail", 'rename("f", "d/") -- file source, directory destination + separator', objects, "f", "d/")
    row("trail", 'rename("d", "d2/") -- directory source, directory destination + separator',
        objects, "d", "d2/")

    def linkdest(root):
        os.mkdir(os.path.join(root, "d"))
        os.mkdir(os.path.join(root, "realdir"))
        os.symlink(os.path.join(root, "realdir"), os.path.join(root, "ld"))
    row("trail", 'rename("d", "ld/") where ld -> realdir', linkdest, "d", "ld/")

    def freeundernowrite(root):
        os.mkdir(os.path.join(root, "p"))
        open(os.path.join(root, "p", "f"), "w").close()
        os.mkdir(os.path.join(root, "q"))
        os.chmod(os.path.join(root, "q"), 0o555)
    row("trail", 'rename(p/f, "q/absent/") -- the demand against the parent\'s write bit',
        freeundernowrite, "p/f", "q/absent/")

    def subtreetrailing(root):
        os.makedirs(os.path.join(root, "a", "b"))
    row("trail", 'rename(a, "a/b/new/") -- the subtree rule against the demand',
        subtreetrailing, "a", "a/b/new/")

    # Does the demand on the *destination* say anything about the destination
    # itself, or only about the source? With a writable parent the two readings
    # both answer ENOTDIR -- once from the demand and once from the ordinary
    # type rule below it -- so only an unwritable parent, whose write check sits
    # between them, can tell them apart.
    def narrowed(dstkind):
        def go(root):
            os.mkdir(os.path.join(root, "d"))
            open(os.path.join(root, "f"), "w").close()
            os.mkdir(os.path.join(root, "realdir"))
            os.mkdir(os.path.join(root, "q"))
            t = os.path.join(root, "q", "t")
            if dstkind == "symdir":
                os.symlink(os.path.join(root, "realdir"), t)
            elif dstkind == "file":
                open(t, "w").close()
            elif dstkind == "emptydir":
                os.mkdir(t)
            os.chmod(os.path.join(root, "q"), 0o555)
        return go

    def writable(dstkind):
        def go(root):
            narrowed(dstkind)(root)
            os.chmod(os.path.join(root, "q"), 0o755)
        return go

    row("trail", 'rename(d, "q/t/") where q/t is a symlink to a dir, q writable',
        writable("symdir"), "d", "q/t/")
    row("trail", 'rename(d, "q/t/") where q/t is a symlink to a dir, q UNWRITABLE',
        narrowed("symdir"), "d", "q/t/")
    row("trail", 'rename(d, "q/t/") where q/t is a regular file, q UNWRITABLE',
        narrowed("file"), "d", "q/t/")
    row("trail", 'rename(d, "q/t/") where q/t is an empty dir, q UNWRITABLE (control)',
        narrowed("emptydir"), "d", "q/t/")
    row("trail", 'rename(f, "q/t/") where q/t is a symlink to a dir, q UNWRITABLE',
        narrowed("symdir"), "f", "q/t/")


# -------------------------------------------- an XNU race, not a rename rule

def unstable_trailing_symlink():
    """`rename("l/", "g")` where `l` is a symlink to a regular file is
    **nondeterministic on macOS**, and this samples it rather than asserting it.

    Measured 2026-08-23 on macOS 26.6/APFS: with an *absolute* target the call
    is usually ENOTDIR but sometimes succeeds -- and a success **moves the
    link's target**, leaving `l` dangling. The rate scales with how many
    components the absolute target has to traverse: ~1% under /private/tmp,
    ~10% under a /private/var/folders temp directory. With a *relative* target
    it is ENOTDIR every time, 600/600 across both locations.

    So the rule really is "a trailing separator over a symlink to a
    non-directory is ENOTDIR", and the absolute-target case is a race in the
    walk -- consistent with the trailing-slash demand being lost when an
    absolute symlink target restarts namei from the root.

    PawPrint must answer deterministically, so it answers ENOTDIR: the stable
    result, the overwhelming majority of the unstable one, and the only one of
    the two that destroys nothing. That is a *choice*, recorded here so nobody
    later reads a single ENOTDIR sample as proof there was nothing to choose.

    This row must never go in the host-equality tier: it would flake.
    """
    trials = 200
    for style in ("relative", "absolute"):
        outcomes = {}
        for _ in range(trials):
            root = os.path.realpath(tempfile.mkdtemp(prefix="rn-"))
            try:
                target = os.path.join(root, "t")
                open(target, "w").close()
                link = os.path.join(root, "l")
                os.symlink(target if style == "absolute" else "t", link)
                try:
                    os.rename(link + "/", os.path.join(root, "g"))
                    key = "ok (moved the target)" if not os.path.lexists(target) else "ok (moved nothing)"
                except OSError as e:
                    key = errname(e.errno)
                outcomes[key] = outcomes.get(key, 0) + 1
            finally:
                shutil.rmtree(root, ignore_errors=True)
        summary = ", ".join("%s x%d" % (k, v) for k, v in sorted(outcomes.items(), key=lambda kv: -kv[1]))
        ROWS.append(("unstable", 'rename("l/", "g"), %s target, %d trials' % (style, trials), summary))


# ---------------------------------------------- a destination parent with no name

def orphaned_destination_parent():
    """An orphan is only reachable as the process's current directory, so every
    row here chdirs into a directory and rmdirs it out from under itself. The
    source is always absolute, so only the destination's parent is the orphan."""
    def orphan_row(label, build, src, dst):
        root = os.path.realpath(tempfile.mkdtemp(prefix="rn-"))
        saved = os.getcwd()
        try:
            build(root)
            gone = os.path.join(root, "gone")
            os.mkdir(gone)
            os.chdir(gone)
            os.rmdir(gone)
            try:
                os.rename(os.path.join(root, src), dst)
                result = "ok"
            except OSError as e:
                result = errname(e.errno)
        finally:
            os.chdir(saved)
            widen(root)
            shutil.rmtree(root, ignore_errors=True)
        ROWS.append(("orphan", label, result))

    def onefile(root):
        open(os.path.join(root, "f"), "w").close()

    def adir(root):
        os.mkdir(os.path.join(root, "d"))

    def unwritable(root):
        os.mkdir(os.path.join(root, "p"))
        open(os.path.join(root, "p", "f"), "w").close()
        os.chmod(os.path.join(root, "p"), 0o555)

    orphan_row("ordinary file source (control)", onefile, "f", "x")
    orphan_row("AND source absent", onefile, "nope", "x")
    orphan_row("AND the source's parent unwritable", unwritable, "p/f", "x")
    orphan_row("AND the source is a navigation", adir, "d/.", "x")
    orphan_row("directory source", adir, "d", "x")
    orphan_row("AND destination name 300 bytes", onefile, "f", LONG)
    orphan_row("AND a trailing separator on a file source", onefile, "f/", "x")
    orphan_row("AND a trailing separator on the destination", onefile, "f", "x/")


# ------------------------------------------- which path is resolved first

def walk_order():
    """Rows where *both* paths are bad, so the errno says which was resolved
    first and how far. Only pairs that disagree are here: a pair answering the
    same errno either way cannot tell the two orders apart.

    Linux's answers are only consistent with four phases -- resolve the
    source's parent, the destination's parent, then look the source up, then
    the destination -- which is `do_renameat2`'s shape. Darwin's are consistent
    with resolving the source completely and then the destination."""
    def dstparentfile(root):
        open(os.path.join(root, "g"), "w").close()
    row("walk", "source absent  X  destination's parent is a regular file", dstparentfile, "nope", "g/x")

    def nothing(root):
        pass
    row("walk", "source name 300 bytes  X  destination's parent absent", nothing, LONG, "nodir/x")
    row("walk", "source absent  X  destination name 300 bytes", nothing, "nope", LONG)

    def srcnosearch(root):
        os.mkdir(os.path.join(root, "p"))
        os.chmod(os.path.join(root, "p"), 0o666)
    row("walk", "source's parent unsearchable  X  destination's parent absent", srcnosearch, "p/f", "nodir/x")

    def srcnosearch_dstfile(root):
        os.mkdir(os.path.join(root, "p"))
        open(os.path.join(root, "p", "f"), "w").close()
        os.chmod(os.path.join(root, "p"), 0o666)
        open(os.path.join(root, "g"), "w").close()
    row("walk", "source's parent unsearchable  X  destination's parent is a file",
        srcnosearch_dstfile, "p/f", "g/x")

    def srcparentfile(root):
        open(os.path.join(root, "p"), "w").close()
        os.mkdir(os.path.join(root, "d"))
    row("walk", "source's parent is a regular file  X  destination is a directory", srcparentfile, "p/f", "d")


# --------------------------------------------------- the root of a real mount

def mount_root():
    """The Darwin rows that need a filesystem root which is not "/".

    A mount root reached by a final "." cannot be measured at all: for such a
    path the source's parent directory is the mount's parent, on the containing
    filesystem, so EXDEV pre-empts the rule. Reached by a final ".." it answers
    EINVAL like any other directory -- and getting that row right requires
    descending into a child of the mount first, since `base/..` is the mount's
    *parent*, a different inode on a different filesystem.

    Everything this section creates lives inside one `mkdtemp` directory made
    directly under the mount. That is what makes it safe on a caller-supplied
    real mount, and it is not merely defensive: `mkdtemp` creates atomically and
    uniquely, so there is no collision to lose a race against, no ownership to
    verify later, and nothing to clean up but the one directory. The only names
    outside it are `base`, `base/.`, `base/..`, `base/<private>/..` and one
    derived sibling of `private` -- needed by the two rows whose destination's
    parent must be the mount root itself, and checked for a collision before
    use.

    `base/<private>` doubles as the child the ".." rows descend through: it is a
    directory directly under the mount, which is exactly what they need.
    """
    if BASE is None:
        ROWS.append(("mountroot", "(pass a mount point as argv[1] to run this section)", "skipped"))
        return

    base = os.path.realpath(BASE)

    try:
        private = tempfile.mkdtemp(prefix="renameprobe-", dir=base)
    except OSError as e:
        ROWS.append(("mountroot", "could not create a working directory under the mount", str(e)))
        return

    src_file = os.path.join(private, "file")
    src_dir = os.path.join(private, "dir")
    moved = os.path.join(private, "moved")

    # Two rows need a destination whose *parent* is the mount root itself, which
    # cannot live inside `private`. Its name is derived from `private`'s, so it
    # inherits mkdtemp's uniqueness rather than inventing a fresh guess -- and it
    # is still checked, because deriving a name is not reserving one.
    outside = private + "-outside"
    if os.path.lexists(outside):
        ROWS.append(("mountroot",
                     "REFUSING: %s already exists under the mount" % os.path.basename(outside),
                     "skipped"))
        shutil.rmtree(private, ignore_errors=True)
        return

    def setup():
        for path in (src_file, src_dir, moved):
            if os.path.isdir(path) and not os.path.islink(path):
                shutil.rmtree(path, ignore_errors=True)
            elif os.path.lexists(path):
                os.unlink(path)
        open(src_file, "w").close()
        os.mkdir(src_dir)

    def absolute_row(label, src, dst):
        try:
            setup()
            try:
                os.rename(src, dst)
                result = "ok"
            except OSError as e:
                result = errname(e.errno)
        except OSError as e:
            result = "setup failed: %s" % e
        ROWS.append(("mountroot", label, result))

    # The four rows that matter, and why they are a 2x2 rather than a list.
    #
    # A filesystem root that is not "/" is a *mount* root, and renaming one is
    # liable to EXDEV -- which masks the rule this section exists to measure.
    # Measured 40 trials per row on a fresh APFS image, all stable: the
    # discriminator is not "." against ".." but whether the source's parent
    # directory and the destination's parent directory are the same object.
    # Where they differ, EXDEV stays quiet and the root answers EINVAL, exactly
    # as any other directory does; where they coincide, EXDEV pre-empts it.
    #
    # So the conclusion is "the root is not a special case for rename on
    # Darwin", and these four rows are what carries it. Dropping either column
    # would leave a table that looks like `.` and `..` disagree.
    try:
        absolute_row('mount root via "." , dst parent DIFFERS from src parent',
                     os.path.join(base, "."), moved)
        absolute_row('mount root via "." , dst parent SAME as src parent',
                     os.path.join(base, "."), outside)
        absolute_row('mount root via ".." , dst parent SAME as src parent',
                     os.path.join(private, ".."), moved)
        absolute_row('mount root via ".." , dst parent DIFFERS from src parent',
                     os.path.join(private, ".."), outside)
        absolute_row('an ordinary directory via ".." (control: never the root)',
                     os.path.join(src_dir, ".."), moved)
        # `private` is a child of the mount, so `private/..` is the mount root
        # itself reached by a final "..". `base/..` would be the directory
        # *containing* the mount -- the control row below measures that, and it
        # is a different filesystem.
        absolute_row("the mount's PARENT via base/.. (control: a different inode)",
                     os.path.join(base, ".."), moved)
        absolute_row('directory -> mount root via ".." (through a child)',
                     src_dir, os.path.join(private, ".."))
        absolute_row("mount root named directly as source", base, moved)
        absolute_row('file -> mount root via "."', src_file, os.path.join(base, "."))
        absolute_row('directory -> mount root via "."', src_dir, os.path.join(base, "."))
        absolute_row("file -> mount root named directly", src_file, base)
        absolute_row("directory -> mount root named directly", src_dir, base)
    finally:
        # One directory, created atomically by this process and named by nobody
        # else. Nothing here has to decide whether an object is ours.
        shutil.rmtree(private, ignore_errors=True)
        # `outside` is the one name this section uses that is not inside
        # `private`, and it is never deleted -- only reported.
        #
        # Three rounds of review went into trying to delete it safely, and the
        # conclusion is that it cannot be done: POSIX has no verified
        # delete-by-path. An `(st_dev, st_ino)` check before `unlink` proves
        # nothing, because `unlink` resolves the name again and another process
        # can have replaced the object in between; and tracking which inodes are
        # ours founders on inode reuse. There is no primitive that says "remove
        # the object I looked at".
        #
        # Leaving it is cheap. Every row on both measured kernels refuses, so
        # nothing is ever created here at all; only a kernel neither of us has
        # seen would leave one of this run's objects behind, and then a line of
        # output on a hand-attached disk image is a far smaller cost than
        # deleting a stranger's file on a shared mount.
        if os.path.lexists(outside):
            ROWS.append(("mountroot",
                         "LEFT BEHIND: %s -- remove it by hand" % os.path.basename(outside),
                         "not removed"))

def main():
    print("uid=%d euid=%d platform=%s base=%s" % (os.getuid(), os.geteuid(), sys.platform, BASE))
    type_matrix()
    structural()
    navigation()
    permissions()
    ordering()
    displaced_directory()
    trailing()
    unstable_trailing_symlink()
    orphaned_destination_parent()
    walk_order()
    mount_root()
    width = max(len(label) for _, label, _ in ROWS)
    section = None
    for s, label, result in ROWS:
        if s != section:
            section = s
            print("\n## %s" % s)
        print("%-*s  %s" % (width, label, result))


main()

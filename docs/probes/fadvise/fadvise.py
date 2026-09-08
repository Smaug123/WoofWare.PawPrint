#!/usr/bin/env python3
"""posix_fadvise(2) over every descriptor kind this kernel models, over each
advice value and over out-of-range offsets and lengths, measured on both
flavours.

posix_fadvise returns the error number directly and leaves errno alone, so the
rows below report the return value only. Darwin's libc has no such symbol at
all, which is why System.Native's shim compiles its body out there
(HAVE_POSIX_ADVISE) and answers ENOTSUP without looking at the descriptor.
Measured outputs sit beside this file.

    python3 fadvise.py
    container run --rm -v "$PWD:/probe" python:3-slim python3 /probe/fadvise.py
"""
import ctypes, errno, os, select, socket, tempfile

libc = ctypes.CDLL(None, use_errno=True)
print(os.uname().sysname, os.uname().release, os.uname().machine)
if not hasattr(libc, "posix_fadvise"):
    print("libc has no posix_fadvise symbol")
    raise SystemExit(0)

libc.posix_fadvise.argtypes = [ctypes.c_int, ctypes.c_int64, ctypes.c_int64, ctypes.c_int]
libc.posix_fadvise.restype = ctypes.c_int


def fadvise(fd, offset, length, advice):
    ctypes.set_errno(0)
    r = libc.posix_fadvise(fd, offset, length, advice)
    return r, ctypes.get_errno()


path = tempfile.mkstemp(prefix="fadviseprobe")[1]
with open(path, "wb") as f:
    f.write(b"x" * 4096)
rdwr = os.open(path, os.O_RDWR)
rdonly = os.open(path, os.O_RDONLY)
wronly = os.open(path, os.O_WRONLY)
directory = os.open(os.path.dirname(path), os.O_RDONLY)
devnull = os.open("/dev/null", os.O_RDWR)
pipe_r, pipe_w = os.pipe()
tcp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
unixsock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
epoll = select.epoll()

kinds = [
    ("closed fd 99", 99),
    ("regular O_RDWR", rdwr),
    ("regular O_RDONLY", rdonly),
    ("regular O_WRONLY", wronly),
    ("directory", directory),
    ("/dev/null", devnull),
    ("pipe read end", pipe_r),
    ("pipe write end", pipe_w),
    ("TCP socket", tcp.fileno()),
    ("unix socket", unixsock.fileno()),
    ("epoll port", epoll.fileno()),
]
advices = [
    ("NORMAL 0", 0), ("RANDOM 1", 1), ("SEQUENTIAL 2", 2),
    ("WILLNEED 3", 3), ("DONTNEED 4", 4), ("NOREUSE 5", 5),
    ("out of range 6", 6), ("out of range -1", -1),
]

print("== descriptor kind x advice, offset 0 length 0")
for kname, fd in kinds:
    for aname, advice in advices:
        r, e = fadvise(fd, 0, 0, advice)
        print(f"fadvise({kname:17}, 0, 0, {aname:16}) -> ret={r} {errno.errorcode.get(r, '')} errno={e}")

INT64_MAX = (1 << 63) - 1
ranges = [
    ("offset 0 length 0", 0, 0),
    ("offset 0 length 4096", 0, 4096),
    ("offset 0 length -1", 0, -1),
    ("offset -1 length 0", -1, 0),
    ("offset -1 length -1", -1, -1),
    ("offset 0 length INT64_MAX", 0, INT64_MAX),
    ("offset INT64_MAX length 1", INT64_MAX, 1),
    ("offset INT64_MAX length INT64_MAX", INT64_MAX, INT64_MAX),
]
print("== regular file O_RDWR x range, advice NORMAL then DONTNEED")
for rname, off, length in ranges:
    for aname, advice in [("NORMAL 0", 0), ("DONTNEED 4", 4)]:
        r, e = fadvise(rdwr, off, length, advice)
        print(f"fadvise(regular, {rname:33}, {aname:11}) -> ret={r} {errno.errorcode.get(r, '')} errno={e}")

INT64_MIN = -(1 << 63)
print("== pipe read end x range: does ESPIPE precede the range and advice checks?")
for rname, off, length in [("offset 0 length 0", 0, 0), ("offset 0 length -1", 0, -1), ("offset -1 length -1", -1, -1)]:
    for aname, advice in [("NORMAL 0", 0), ("out of range 6", 6)]:
        r, e = fadvise(pipe_r, off, length, advice)
        print(f"fadvise(pipe read end, {rname:19}, {aname:14}) -> ret={r} {errno.errorcode.get(r, '')} errno={e}")

print("== regular file: negative offset with a positive length, and INT64_MIN")
for rname, off, length in [
    ("offset -1 length 4096", -1, 4096),
    ("offset -4096 length 4096", -4096, 4096),
    ("offset INT64_MIN length 0", INT64_MIN, 0),
    ("offset 0 length INT64_MIN", 0, INT64_MIN),
    ("offset INT64_MAX length INT64_MIN", INT64_MAX, INT64_MIN),
]:
    r, e = fadvise(rdwr, off, length, 0)
    print(f"fadvise(regular, {rname:33}, NORMAL 0   ) -> ret={r} {errno.errorcode.get(r, '')} errno={e}")

print("== ordering: bad fd with out-of-range advice, and bad range")
for kname, fd in [("closed fd 99", 99), ("regular O_RDWR", rdwr)]:
    for aname, advice in [("out of range 6", 6)]:
        for rname, off, length in [("offset 0 length 0", 0, 0), ("offset -1 length -1", -1, -1)]:
            r, e = fadvise(fd, off, length, advice)
            print(f"fadvise({kname:15}, {rname:19}, {aname}) -> ret={r} {errno.errorcode.get(r, '')} errno={e}")

#!/usr/bin/env python3
"""flock(2) with LOCK_MAND (bit 32) and other malformed operations, on a
closed descriptor and on an open file, measured on both flavours.

Linux's flock syscall answers 0 to any LOCK_MAND request before it looks the
descriptor up (fs/locks.c: "This support has been removed and the request
ignored"), and EINVAL to every other malformed operation, again before the
lookup. Darwin looks the descriptor up first, so a closed one is EBADF for
every operation. Measured outputs sit beside this file.

    python3 lock-mand.py
    container run --rm -v "$PWD:/probe" python:3-slim python3 /probe/lock-mand.py
"""
import ctypes, os, errno, sys, tempfile
libc = ctypes.CDLL(None, use_errno=True)
libc.flock.argtypes = [ctypes.c_int, ctypes.c_int]
print(os.uname().sysname, os.uname().release)
fd_file = os.open(tempfile.mkstemp(prefix="flockprobe")[1], os.O_RDWR)
LOCK_SH, LOCK_EX, LOCK_NB, LOCK_UN, LOCK_MAND = 1, 2, 4, 8, 32
ops = [("LOCK_MAND", 32), ("LOCK_MAND|LOCK_SH", 33), ("LOCK_MAND|LOCK_EX", 34), ("LOCK_MAND|LOCK_UN", 40), ("LOCK_MAND|LOCK_NB", 36), ("0", 0), ("LOCK_NB", 4), ("LOCK_SH|LOCK_EX", 3), ("unknown bit 16", 16), ("LOCK_SH|16", 17)]
for name, op in ops:
    for fdname, fd in [("closed fd 99", 99), ("open file", fd_file)]:
        ctypes.set_errno(0)
        r = libc.flock(fd, op)
        e = ctypes.get_errno()
        print(f"flock({fdname:12}, {name:18}) -> ret={r} errno={e} {errno.errorcode.get(e,'')}")
        if r == 0 and fd == fd_file:
            libc.flock(fd_file, LOCK_UN)

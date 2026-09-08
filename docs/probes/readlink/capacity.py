# readlink(2) with a non-positive buffer size, measured on both flavours.
# Run as `python3 capacity.py <scratch directory>`; it creates <scratch>/link -> target.

import ctypes, os, errno, sys
libc = ctypes.CDLL(None, use_errno=True)
libc.readlink.argtypes = [ctypes.c_char_p, ctypes.c_void_p, ctypes.c_size_t]
libc.readlink.restype = ctypes.c_ssize_t
base = sys.argv[1].encode()
os.makedirs(base, exist_ok=True)
link = base + b"/link"
try: os.unlink(link)
except FileNotFoundError: pass
os.symlink("target", link)
NEG1 = ctypes.c_size_t(-1).value
def row(name, path, size, buf):
    ctypes.set_errno(0)
    r = libc.readlink(path, buf, size)
    e = ctypes.get_errno()
    print(f"{name:28} size={'-1' if size==NEG1 else size:>4} buf={'NULL' if buf is None else 'ok':4}: ret={r} errno={e} {errno.errorcode.get(e,'')}")
mk = lambda: ctypes.cast(ctypes.create_string_buffer(8192), ctypes.c_void_p)
print(os.uname().sysname, os.uname().release)
for size in [0, NEG1]:
    row("link", link, size, mk())
    row("link NULL buffer", link, size, None)
    row("directory", base, size, mk())
    row("missing", base + b"/nope", size, mk())
    row("missing NULL buffer", base + b"/nope", size, None)
row("link", link, 4096, mk())
row("link NULL buffer", link, 4096, None)

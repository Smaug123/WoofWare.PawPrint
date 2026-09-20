# Produces the plan's Linux column throughout §1.1-§1.5.
# ext4 stores names as bytes and asks no questions: every encoding APFS refuses
# binds here and readdirs back exactly, NAME_MAX is a raw byte count (confirmed
# with 255 x 0xFF), and getcwd returns the raw bytes of a non-UTF-8 directory.
# Run: container run --rm -v "$PWD:/src:ro" -w /work python:3-slim python3 /src/linux-names-are-bytes.py
# Measured: Linux 6.18.5, ext4 (statfs f_type = 0xef53).
import os, errno, ctypes, sys
libc = ctypes.CDLL("libc.so.6", use_errno=True)

def R(label, fn):
    try:
        r = fn()
        print(f"{label:<48} OK ({r!r})")
    except OSError as e:
        print(f"{label:<48} errno={e.errno:<3} {errno.errorcode.get(e.errno)} ({e.strerror})")

buf = ctypes.create_string_buffer(4096*4)
libc.statfs(b".", buf)
print("statfs f_type =", hex(int.from_bytes(buf.raw[0:8], 'little')), "(ext4=0xef53 overlay=0x794c7630 tmpfs=0x1021994)")

os.makedirs(b"d", exist_ok=True)
open(b"d/f", "wb").close()

print("-- creating a binding with a non-UTF-8 name --")
R('open(b"d/\\xff", O_CREAT)', lambda: os.close(os.open(b"d/\xff", os.O_CREAT|os.O_WRONLY, 0o644)))
R('mkdir(b"d/\\xe4\\xb8") truncated UTF-8', lambda: os.mkdir(b"d/\xe4\xb8"))
R('symlink(t, b"d/\\xc0\\x80") overlong', lambda: os.symlink(b"t", b"d/\xc0\x80"))

print("-- readdir round-trip --")
for n in sorted(os.listdir(b"d")):
    print("   d_name bytes:", " ".join(f"{c:02X}" for c in n), f"({len(n)})")

print("-- lookup of a bad name that is NOT bound --")
R('open(b"d/\\xfe") unbound', lambda: os.open(b"d/\xfe", os.O_RDONLY))
R('open(b"d/\\xff") bound above', lambda: os.close(os.open(b"d/\xff", os.O_RDONLY)))

print("-- NAME_MAX counted in raw bytes? --")
R('open(255 x 0xff, O_CREAT)', lambda: os.close(os.open(b"d/" + b"\xff"*255, os.O_CREAT|os.O_WRONLY, 0o644)))
R('open(256 x 0xff, O_CREAT)', lambda: os.close(os.open(b"d/" + b"\xff"*256, os.O_CREAT|os.O_WRONLY, 0o644)))

print("-- over-long AND invalid: which wins? --")
big = b"/" + b"a"*99 + b"\xff" + b"a"*4899
R(f'open({len(big)}-byte path w/ 0xff)', lambda: os.open(big, os.O_RDONLY))

print("-- symlink target of invalid bytes --")
R('symlink(b"/tmp/\\xff", b"d/lnk")', lambda: os.symlink(b"/tmp/\xff", b"d/lnk"))
t = os.readlink(b"d/lnk"); print("   readlink:", " ".join(f"{c:02X}" for c in t))

print("-- getcwd of a non-UTF-8 directory --")
os.mkdir(b"d/\xff\xfe"); os.chdir(b"d/\xff\xfe")
print("   cwd bytes:", " ".join(f"{c:02X}" for c in os.getcwdb()))
print("   uname:", os.uname().release)

#!/usr/bin/env python3
"""What a UDP socket does after connect(AF_UNSPEC) on each flavour: a later
connect, a rebind to each kind of address and port, and an unconnected
sendto. Linux leaves a socket whose bind chose only the address half-bound at
address:0, and both a rebind and a connect complete it. Measured on both
flavours; outputs sit beside this file.

    python3 half-bound.py
    container run --rm -v "$PWD:/probe" python:3-slim python3 /probe/half-bound.py
"""
import socket, os, errno, struct, ctypes
print(os.uname().sysname, os.uname().release)
libc = ctypes.CDLL(None, use_errno=True)
def unspec(s):
    sa = ctypes.create_string_buffer(struct.pack("=H", 0) + b"\0" * 14, 16)
    ctypes.set_errno(0); r = libc.connect(s.fileno(), sa, 16); return r, ctypes.get_errno()
def name(s):
    try: return s.getsockname()
    except OSError as ex: return errno.errorcode[ex.errno]
def half(bind_to):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM); s.bind(bind_to); s.connect(("127.0.0.1", 9000)); unspec(s); return s
for label, bind_to in [("127.0.0.1:0", ("127.0.0.1", 0)), ("0.0.0.0:0", ("0.0.0.0", 0)), ("0.0.0.0:5555", ("0.0.0.0", 5555)), ("127.0.0.1:5556", ("127.0.0.1", 5556))]:
    s = half(bind_to); before = name(s)
    try: s.connect(("127.0.0.1", 9001)); r = f"connect ok -> {name(s)}"
    except OSError as ex: r = "connect " + errno.errorcode[ex.errno]
    print(f"after unspec from bind({label}): {before}; {r}"); s.close()
    for rebind in [("127.0.0.1", 0), ("0.0.0.0", 0), ("127.0.0.1", 7777), ("0.0.0.0", 7778)]:
        s = half(bind_to)
        try: s.bind(rebind); r = f"ok -> {name(s)}"
        except OSError as ex: r = errno.errorcode[ex.errno]
        print(f"   rebind {rebind}: {r}"); s.close()
    # and getpeername / send after dissolve: does the socket still work?
    s = half(bind_to)
    try: s.sendto(b"x", ("127.0.0.1", 9002)); r = f"sendto ok -> {name(s)}"
    except OSError as ex: r = "sendto " + errno.errorcode[ex.errno]
    print(f"   {r}"); s.close()

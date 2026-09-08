#!/usr/bin/env python3
"""UDP connect(AF_UNSPEC): what it answers and what it leaves of the local
binding and the peer, by how the socket was bound and whether it was
connected. Measured on both flavours; outputs sit beside this file.

    python3 dissolve.py
    container run --rm -v "$PWD:/probe" python:3-slim python3 /probe/dissolve.py
"""
import socket, os, errno, struct, ctypes, sys
print(os.uname().sysname, os.uname().release)
def unspec(s):
    # connect(AF_UNSPEC) with a 16-byte sockaddr, which Socket.Disconnect / SystemNative_Disconnect issues
    libc = ctypes.CDLL(None, use_errno=True)
    sa = ctypes.create_string_buffer(struct.pack("=H", 0) + b"\0" * 14, 16)
    ctypes.set_errno(0)
    r = libc.connect(s.fileno(), sa, 16)
    e = ctypes.get_errno()
    return r, e
def row(name, bind_to, connect_to=("127.0.0.1", 9000)):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        if bind_to is not None:
            s.bind(bind_to)
        before = s.getsockname()
        if connect_to is not None:
            s.connect(connect_to)
        after_connect = s.getsockname()
        r, e = unspec(s)
        after = s.getsockname()
        try:
            peer = "peer=" + str(s.getpeername())
        except OSError as ex:
            peer = "peer " + errno.errorcode[ex.errno]
        try:
            s.bind(("0.0.0.0", 0)); rebind = "bind(0.0.0.0:0) ok -> " + str(s.getsockname())
        except OSError as ex:
            rebind = "bind(0.0.0.0:0) " + errno.errorcode[ex.errno]
        print(f"{name:42}: bound={before} connected={after_connect} unspec ret={r} errno={e} {errno.errorcode.get(e,'')} after={after} {peer}; {rebind}")
    except OSError as ex:
        print(f"{name:42}: OSError {errno.errorcode[ex.errno]}")
    finally:
        s.close()
row("bind(0.0.0.0:5555), connect, unspec", ("0.0.0.0", 5555))
row("bind(0.0.0.0:0), connect, unspec", ("0.0.0.0", 0))
row("bind(127.0.0.1:5556), connect, unspec", ("127.0.0.1", 5556))
row("bind(127.0.0.1:0), connect, unspec", ("127.0.0.1", 0))
row("unbound, connect, unspec", None)
row("bind(0.0.0.0:5557), no connect, unspec", ("0.0.0.0", 5557), None)
row("bind(127.0.0.1:5558), no connect, unspec", ("127.0.0.1", 5558), None)
row("unbound, no connect, unspec", None, None)

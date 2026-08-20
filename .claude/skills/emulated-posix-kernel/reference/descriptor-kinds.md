# What each descriptor kind answers

Measured on Linux 6.18.5 aarch64 and Darwin 25.6.0 arm64. Re-derive rather than
trust if the pinned kernel moves.

## Socket event port (`epoll_create1` / `kqueue`)

Only `pread`/`pwrite` agree.

| op | Linux | Darwin |
| --- | --- | --- |
| `read` / `write` | `EINVAL` | `ENXIO` |
| `pread` / `pwrite` | `ESPIPE` | `ESPIPE` |
| `lseek`, whence 0–4, any offset | succeeds, reports 0 | `ESPIPE` |
| `lseek`, whence ≥ 5 | `EINVAL` | `ESPIPE` |
| `flock` | succeeds | `ENOTSUP` (45) |
| `fstat` `st_mode` | `0600`, no type bits | `S_IFIFO`, no permission bits |

**Two rows that source-reading gets wrong**, which is why they were measured:

1. `lseek` on an epoll fd is `noop_llseek`, **not** `ESPIPE`: it succeeds and
   reports 0 for `SEEK_SET` with `-1` and with `INT64_MAX` alike.
2. `do_epoll_wait`'s widely-reproduced check order (maxevents → `access_ok` →
   `fdget`) is stale. Measured on 6.18.5 the order is **EBADF → EINVAL
   (maxevents) → EFAULT (buffer) → EINVAL (not an epoll fd)**.

`access_ok` rejects only *kernel-range* addresses, so a userspace-but-unmapped
buffer passes it and `epoll_wait` blocks. Do not eagerly validate a buffer before
parking; `UserBufferCheck.BeforeOperation` already models this.

## Socket

On a fresh, unbound, unconnected socket (`O_NONBLOCK` set, so a would-block shows
as `EAGAIN` rather than hanging):

| op | Linux (euid 1000) | Darwin |
| --- | --- | --- |
| `lseek`, whence 0–4, any offset | `ESPIPE` | `ESPIPE` |
| `lseek`, whence 9 | `EINVAL` | `ESPIPE` |
| `pread` / `pwrite` | `ESPIPE` | `ESPIPE` |
| `flock(LOCK_EX\|LOCK_NB)` | succeeds | `ENOTSUP` (45) |
| `ftruncate` / `fsync` | `EINVAL` | `EINVAL` |
| `fstat` `st_mode` | `S_IFSOCK\|0777` | `S_IFSOCK\|0666` |
| `fstat` `st_dev` / `st_ino` | sockfs dev, one inode per socket | 0 / 0 for `AF_INET`, real for `AF_UNIX` |
| `fstat` `st_blksize` | 4096 | 131072 TCP, 9216 UDP, 8192 unix |
| `epoll_wait` / `kevent` on it | `EINVAL` (after the `EFAULT` buffer screen) | `EBADF` |

`read` and `write` on a socket are refused rather than answered, because every
answer is a claim about connection state that PawPrint does not model: `read` is
`ENOTCONN` for TCP on both, and `EINVAL` on Linux against `ENOTCONN` on Darwin for
a Unix-domain stream socket. For a datagram socket the answer depends on the
descriptor's own blocking mode, which is why the probe above set `O_NONBLOCK`: it
saw `EAGAIN`, but a socket straight from `socket(2)` is *blocking*, and a real
`read` on one **blocks with no wake source**. `write` is `EPIPE` on Linux against
`ENOTCONN` on Darwin for TCP, and `EDESTADDRREQ` for a datagram socket. The Linux TCP row raises `SIGPIPE` as well,
but a .NET guest never sees it: CoreCLR installs `signal(SIGPIPE, SIG_IGN)`
process-wide (`src/coreclr/pal/src/exception/signal.cpp`).

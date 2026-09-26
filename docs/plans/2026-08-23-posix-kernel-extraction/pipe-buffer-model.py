#!/usr/bin/env python3
"""Replay pipe-buffer-trace.c's (and pipe-buffer-growth.c's and
pipe-buffer-doubling.c's) output against candidate models of a pipe's buffer,
and report the first disagreement per seed, plus totals. This is how the rules
PipeBuffer states were chosen: `linux` and `darwin-dyn` (at its default initial
size of 512) agree with every recorded row, and the other models are the
alternatives the rows falsify. `TestPipeBuffer`'s reference models are the F#
statements of the two that survive.

Usage: pipe-buffer-model.py linux|linux-bytes|darwin|darwin-dyn TRACEFILE [variant]
  linux variant `nomerge` drops the merge into the newest slot;
  darwin variant N sets the free space below which the write end is not ready;
  darwin-dyn variant `INITIAL,FLOOR` sets the starting size and the ready floor.
"""
import sys

PAGE = 4096


class LinuxSlots:
    """pipe_write/pipe_read as read in fs/pipe.c (6.x): a ring of 16 page-sized
    buffers; a write merges its sub-page remainder into the last buffer when it
    fits, otherwise takes whole new slots; POLLOUT means a free slot."""

    def __init__(self, slots=16, merge=True):
        self.bufs = []  # list of [offset, len]
        self.slots = slots
        self.merge = merge

    def avail(self):
        return sum(l for _, l in self.bufs)

    def write(self, n):
        if n == 0:
            return 0, 0
        ret = 0
        remaining = n
        chars = n & (PAGE - 1)
        if self.merge and chars and self.bufs:
            off, l = self.bufs[-1]
            if off + l + chars <= PAGE:
                self.bufs[-1][1] += chars
                ret += chars
                remaining -= chars
                if remaining == 0:
                    return ret, 0
        while True:
            if len(self.bufs) < self.slots:
                c = min(PAGE, remaining)
                self.bufs.append([0, c])
                ret += c
                remaining -= c
                if remaining == 0:
                    return ret, 0
                continue
            return (ret, 0) if ret else (-1, 11)

    def read(self, n):
        if n == 0:
            return 0, 0
        if not self.bufs:
            return -1, 11
        got = 0
        while n > 0 and self.bufs:
            off, l = self.bufs[0]
            c = min(n, l)
            self.bufs[0] = [off + c, l - c]
            got += c
            n -= c
            if self.bufs[0][1] == 0:
                self.bufs.pop(0)
        return got, 0

    def poll(self):
        r = 0x41 if self.bufs else 0
        w = 0x104 if len(self.bufs) < self.slots else 0
        return r, w

    def st_size(self):
        return 0, 0


class LinuxBytes:
    """The rejected alternative for Linux, with Linux's own observables: a byte
    counter against 64 KiB, writes of at most PIPE_BUF (4096) all-or-nothing,
    larger ones taking what fits, the write end ready while any byte is free,
    st_size always 0, EAGAIN 11."""

    def __init__(self):
        self.cnt = 0

    def avail(self):
        return self.cnt

    def write(self, n):
        if n == 0:
            return 0, 0
        free = 65536 - self.cnt
        c = (n if free >= n else 0) if n <= 4096 else min(n, free)
        if c == 0:
            return -1, 11
        self.cnt += c
        return c, 0

    def read(self, n):
        if n == 0:
            return 0, 0
        if self.cnt == 0:
            return -1, 11
        c = min(n, self.cnt)
        self.cnt -= c
        return c, 0

    def poll(self):
        return (0x41 if self.cnt else 0), (0x104 if self.cnt < 65536 else 0)

    def st_size(self):
        return 0, 0


class DarwinRing:
    """A byte counter against a fixed capacity; writes of at most PIPE_BUF are
    all-or-nothing, larger ones take what fits."""

    def __init__(self, cap=65536, pipe_buf=512, out_threshold=None):
        self.cnt = 0
        self.cap = cap
        self.pipe_buf = pipe_buf
        self.out_threshold = pipe_buf if out_threshold is None else out_threshold

    def avail(self):
        return self.cnt

    def write(self, n):
        if n == 0:
            return 0, 0
        free = self.cap - self.cnt
        if n <= self.pipe_buf:
            if free >= n:
                self.cnt += n
                return n, 0
            return -1, 35
        if free == 0:
            return -1, 35
        c = min(n, free)
        self.cnt += c
        return c, 0

    def read(self, n):
        if n == 0:
            return 0, 0
        if self.cnt == 0:
            return -1, 35
        c = min(n, self.cnt)
        self.cnt -= c
        return c, 0

    def poll(self):
        r = 0xc3 if self.cnt else 0
        w = 0x104 if self.cap - self.cnt >= self.out_threshold else 0
        return r, w

    def st_size(self):
        return self.cnt, self.cnt


BLOCKS = [512, 1024, 2048, 4096, 8192, 16384, 65536]


def choose(current, expected):
    target = max(current, expected)
    i = len(BLOCKS) - 1
    while i > 0 and BLOCKS[i - 1] > target:
        i -= 1
    return BLOCKS[i]


class DarwinDynamic(DarwinRing):
    """XNU's sys_pipe.c as read: the buffer starts at `initial` and a write that
    does not fit grows it to choose_pipespace(size, cnt + n) (at most 64 KiB);
    POLLOUT means MAX(PIPE_SIZE, size) - cnt >= PIPE_BUF."""

    def __init__(self, initial=512, floor=16384):
        super().__init__()
        self.size = initial
        self.floor = floor

    def write(self, n):
        if n and (self.size == 0 or n > self.size - self.cnt):
            self.size = max(self.size, choose(self.size, self.cnt + n))
        self.cap = self.size
        return super().write(n)

    def poll(self):
        r = 0xc3 if self.cnt else 0
        w = 0x104 if max(self.floor, self.size) - self.cnt >= self.pipe_buf else 0
        return r, w


def parse(path):
    for line in open(path):
        if line.startswith('#') or not line.strip():
            continue
        left, fion, polls, sizes = [x.strip() for x in line.split('|')]
        seed, op, n, _, ret, err = left.split()
        pr, pw = [int(x, 16) for x in polls.split()]
        sr, sw = [int(x) for x in sizes.split()]
        yield int(seed), op, int(n), int(ret), int(err), int(fion), pr, pw, sr, sw


def main():
    flavour, path = sys.argv[1], sys.argv[2]
    variant = sys.argv[3] if len(sys.argv) > 3 else ''
    mk = {
        'linux': lambda: LinuxSlots(merge=(variant != 'nomerge')),
        'linux-bytes': LinuxBytes,
        'darwin': lambda: DarwinRing(out_threshold=int(variant) if variant else None),
        'darwin-dyn': lambda: DarwinDynamic(*[int(x) for x in variant.split(',')] if variant else []),
    }[flavour]
    models = {}
    rows = bad = 0
    first_bad = {}
    for seed, op, n, ret, err, fion, pr, pw, sr, sw in parse(path):
        m = models.setdefault(seed, mk())
        if seed in first_bad:
            continue
        got = m.write(n) if op == 'W' else m.read(n)
        rows += 1
        exp = (ret, err if ret < 0 else 0)
        obs = (got, m.avail(), m.poll(), m.st_size())
        want = (exp, fion, (pr, pw), (sr, sw))
        if obs != want:
            bad += 1
            first_bad[seed] = (op, n, 'model', obs, 'kernel', want)
    print(f'{flavour}{"/" + variant if variant else ""}: {rows} rows replayed, {len(first_bad)} of {len(models)} seeds diverged')
    for s, d in list(first_bad.items())[:5]:
        print('  seed', s, d)


main()

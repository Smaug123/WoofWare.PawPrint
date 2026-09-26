#!/usr/bin/env python3
"""Check directory-mutation.c's output against each filesystem's model.

Usage: directory-mutation-models.py tmpfs|apfs <output>...

The models are the ones docs/divergences.md states:

* tmpfs (Linux 6.18.5): the directory is a list in newest-link-first order; each
  link takes an offset from a per-directory counter starting at 3, a rename
  takes a fresh one, and a rename over an existing name inherits the target's.
  After a read, the position is the offset of the next entry. A read resumes at
  the entry with the greatest offset at or below the position and walks the list
  from there; with none, it restarts from the newest entry.
* APFS (Darwin 27): names are ordered by (22-bit hash of the case-folded NFD
  name, name bytes). A read yields the least key above the last one returned;
  once a call has returned the last entry, end-of-directory is sticky.

Measured 2026-09-26: 1280 of 1280 scripts per filesystem, and 320 of 320
order trials.
"""
import re
import sys
import unicodedata
from collections import Counter

DOTS = {".", ".."}


def crc32c(data):
    crc = 0xFFFFFFFF
    for b in data:
        crc ^= b
        for _ in range(8):
            crc = (crc >> 1) ^ (0x82F63B78 if crc & 1 else 0)
    return crc ^ 0xFFFFFFFF


def apfs_key(name):
    folded = unicodedata.normalize("NFD", name).casefold()
    return ((~crc32c(folded.encode("utf-32-le"))) & 0x3FFFFF, name.encode())


def scripts(paths):
    for path in paths:
        lines = open(path).read().splitlines()
        for i, line in enumerate(lines):
            m = re.match(r"MUTATE t=\d+ op=(\w+) k=\d+ before=(.*)$", line)
            if not m:
                continue
            s = re.match(r"MUTATE t=\d+ stream=(.*)\| op (.*) \| (.*)$", lines[i + 1])
            yield m.group(1), m.group(2).split(), s.group(1).split(), s.group(2), s.group(3).split()


def apply_op(desc, names):
    """The directory's names after the operation, in no particular order."""
    names = list(names)
    for tok in desc.split():
        if tok.startswith("+"):
            names.append(tok[1:].rstrip("/"))
        elif tok.startswith("-"):
            names.remove(tok[1:])
        elif ">>" in tok:
            a, b = tok.split(">>")
            names.remove(a)
        elif ">" in tok:
            a, b = tok.split(">")
            names.remove(a)
            names.append(b)
    return names


def apfs_predict(before, prefix, desc):
    after = sorted(apply_op(desc, before), key=apfs_key)
    full = [".", ".."] + sorted(before, key=apfs_key)
    assert prefix == full[: len(prefix)], (prefix, full)
    if not prefix:
        return [".", ".."] + after
    if prefix[-1] == ".":
        return [".."] + after
    if prefix[-1] == "..":
        return after
    if len(prefix) == len(full):
        return []
    last = apfs_key(prefix[-1])
    return [x for x in after if apfs_key(x) > last]


def tmpfs_predict(before, prefix, desc):
    order = list(before)
    offset = {x: 3 + (len(before) - 1 - j) for j, x in enumerate(before)}
    counter = max(offset.values(), default=2)
    full = [".", ".."] + order
    assert prefix == full[: len(prefix)], (prefix, full)
    n = len(prefix)
    cookie = None if n < 2 else ("EOD" if n == len(full) else offset[full[n]])
    for tok in desc.split():
        if tok.startswith("+"):
            x = tok[1:].rstrip("/")
            counter += 1
            offset[x] = counter
            order.insert(0, x)
        elif tok.startswith("-"):
            x = tok[1:]
            order.remove(x)
            del offset[x]
        elif ">>" in tok:
            a, b = tok.split(">>")
            inherited = offset.pop(b)
            order.remove(b)
            offset.pop(a)
            order.remove(a)
            order.insert(0, b)
            offset[b] = inherited
        elif ">" in tok:
            a, b = tok.split(">")
            offset.pop(a)
            order.remove(a)
            order.insert(0, b)
            counter += 1
            offset[b] = counter
    if n == 0:
        return [".", ".."] + order
    if n == 1:
        return [".."] + order
    if cookie == "EOD":
        return []
    at_or_below = [x for x in order if offset[x] <= cookie]
    if at_or_below:
        start = max(at_or_below, key=lambda x: offset[x])
        return order[order.index(start):]
    return order


def main():
    model, paths = sys.argv[1], sys.argv[2:]
    predict = {"tmpfs": tmpfs_predict, "apfs": apfs_predict}[model]
    results = Counter()
    for op, before, prefix, desc, rest in scripts(paths):
        ok = predict(before, prefix, desc) == rest
        results[ok] += 1
        if not ok:
            print("MISMATCH", op, desc, prefix[-3:], rest)
    print(f"{model}: {results[True]} of {results[True] + results[False]} scripts match the model")
    sys.exit(0 if not results[False] else 1)


if __name__ == "__main__":
    main()

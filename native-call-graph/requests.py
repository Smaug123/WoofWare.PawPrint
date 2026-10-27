#!/usr/bin/env python3
"""The C++ routines a call graph describes, read from the native contract's tables.

Usage: requests.py NativeContractTable.tsv NativeContractCallees.tsv > requests.tsv

They are every C++ routine a row of the contract table cites in its sources, and every C++
function or macro that a row of the callees table vouches for by path:symbol. A vouch for a
function of the C library (-:symbol), for the calls one routine makes (path:routine::<...>), or
for anything a file other than C or C++ defines (assembly, an event manifest) names no C++
definition, so it requests none.

Each line of the output is tab-separated: the key, as its table writes it; the path of the file
the key names, relative to the runtime's root; and the key's symbol, without the spelling that
may follow `=`. As in the tables, a symbol names a function whose fully qualified name is the
symbol or ends in :: and the symbol.
"""

import os
import sys

CXX_EXTENSIONS = {".c", ".cpp", ".h", ".hpp", ".inl"}


def rows(path):
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\n")
            if line and not line.startswith("#"):
                yield line.split("\t")


def request(key):
    """The request a key makes, or None if it names no C++ definition."""
    path, sep, symbol = key.partition(":")
    if not sep or not symbol:
        sys.exit(f"requests.py: not path:symbol: {key}")
    if path == "-" or "::<" in symbol:
        return None
    if os.path.splitext(path)[1] not in CXX_EXTENSIONS:
        return None
    name = symbol.partition("=")[0]
    return (key, os.path.normpath(os.path.join("src/coreclr", path)), name)


def main(argv):
    if len(argv) != 3:
        sys.exit(__doc__)
    table, callees = argv[1], argv[2]
    keys = set()
    for fields in rows(table):
        if len(fields) != 9:
            sys.exit(f"requests.py: expected 9 fields in {table}: {fields}")
        keys.update(fields[7].split("|"))
    for fields in rows(callees):
        if len(fields) != 3:
            sys.exit(f"requests.py: expected 3 fields in {callees}: {fields}")
        keys.add(fields[0])
    for r in sorted(filter(None, map(request, keys))):
        print("\t".join(r))


if __name__ == "__main__":
    main(sys.argv)

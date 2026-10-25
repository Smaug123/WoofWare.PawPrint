#!/usr/bin/env python3
"""Readies a configured CMake/Ninja build for parsing the units that make up some of its targets.

Usage: program.py BUILD UNITS TARGET...

Generates every file that a custom command writes and the targets' objects read, such as the
eventing headers and sources that CoreCLR's build generates from its event manifest, and writes
to UNITS the absolute path of every object the targets link, one per line. Compiles and links
nothing.
"""

import os
import subprocess
import sys

COMPILE_RULE_PREFIXES = ("C_COMPILER__", "CXX_COMPILER__", "ASM_COMPILER__")


def ninja(build, *args):
    return subprocess.run(["ninja", "-C", build, *args], capture_output=True, text=True, check=True).stdout


def main(argv):
    if len(argv) < 4:
        sys.exit(__doc__)
    build, units_path, targets = argv[1], argv[2], argv[3:]
    rules = {}
    for line in ninja(build, "-t", "targets", "all").splitlines():
        output, sep, rule = line.rpartition(": ")
        if not sep:
            sys.exit(f"program.py: cannot read ninja's target {line}")
        rules[output] = rule
    for target in targets:
        if target not in rules:
            sys.exit(f"program.py: ninja has no target {target}")
    inputs = set(ninja(build, "-t", "inputs", *targets).split())
    generated = sorted(i for i in inputs if rules.get(i) == "CUSTOM_COMMAND")
    objects = sorted(i for i in inputs if rules.get(i, "").startswith(COMPILE_RULE_PREFIXES))
    if not objects:
        sys.exit(f"program.py: {targets} link no objects")
    subprocess.run(["ninja", "-C", build, *generated], check=True, stdout=subprocess.DEVNULL)
    with open(units_path, "w", encoding="utf-8") as f:
        for o in objects:
            f.write(os.path.normpath(os.path.join(os.path.abspath(build), o)) + "\n")


if __name__ == "__main__":
    main(sys.argv)

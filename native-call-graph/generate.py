#!/usr/bin/env python3
"""Writes the call graph of requested routines, from every unit of one program.

Usage: generate.py --tool TOOL --db compile_commands.json [--units UNITS] --requests REQUESTS
                   --root ROOT [--prefix NAME=PATH]... [--header LINE]... [--jobs N] --out GRAPH

The program is the units of the compilation database whose outputs UNITS lists, one absolute
path per line, or every unit of it without --units; a unit of assembly is skipped. The tool
(callgraph.cpp) runs over each unit twice: first for the requested routines' definitions and
their calls, then, for every virtual call those make, for the final overrider of the method in
every class that is or derives from the receiver's class and is not abstract. A virtual call
becomes one row per overrider found in any unit, or, where no unit has one, a single row naming
the method the call names.

The graph is tab-separated, its lines sorted within each kind, every path relative to ROOT or to
a PREFIX. It begins with the --header lines, each after "# ", and a description of its rows.
"""

import argparse
import concurrent.futures
import json
import os
import subprocess
import sys
import tempfile

ASSEMBLY_EXTENSIONS = {".S", ".s", ".asm"}

COLUMNS = """\
# Rows, by their first field:
#   routine         KEY NAME SIGNATURE USR PATH:LINE   a definition of the routine KEY requests
#   uninstantiated  KEY                                KEY names a template no unit instantiates
#   undefined       KEY                                no unit defines a function KEY names
#   call            ROUTINE-NAME ROUTINE-SIGNATURE ROUTINE-USR PATH:LINE MACRO KIND CALLEE-NAME CALLEE-SIGNATURE CALLEE-USR ENUMS
# A call's MACRO is PATH:NAME of the outermost macro whose body makes the call, or -. KIND is
# direct, virtual (one row per final overrider), virtual-unimplemented (no class the program
# defines implements the method named), indirect (through a pointer; no callee), construct,
# destruct, new (operator new), delete (operator delete), throw (CALLEE-NAME is the type thrown,
# or - to rethrow) or asm (inline assembly). ENUMS lists, separated by |, each argument that names
# an enumerator, as N=Enum::Enumerator, numbering the arguments from 1, defaults included.
"""


def parse_args(argv):
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--tool", required=True)
    p.add_argument("--db", required=True)
    p.add_argument("--units")
    p.add_argument("--requests", required=True)
    p.add_argument("--root", required=True)
    p.add_argument("--prefix", action="append", default=[])
    p.add_argument("--header", action="append", default=[])
    p.add_argument("--jobs", type=int, default=os.cpu_count())
    p.add_argument("--out", required=True)
    return p.parse_args(argv)


def select_units(db_path, units_path):
    with open(db_path, encoding="utf-8") as f:
        db = json.load(f)
    by_output = {}
    for entry in db:
        out = os.path.normpath(os.path.join(entry["directory"], entry["output"]))
        if out in by_output:
            sys.exit(f"generate.py: two commands write {out}")
        by_output[out] = entry
    if units_path is None:
        wanted = sorted(by_output)
    else:
        with open(units_path, encoding="utf-8") as f:
            wanted = sorted({os.path.normpath(line.strip()) for line in f if line.strip()})
    units = []
    for out in wanted:
        entry = by_output.get(out)
        if entry is None:
            sys.exit(f"generate.py: no command writes {out}")
        if os.path.splitext(entry["file"])[1] not in ASSEMBLY_EXTENSIONS:
            units.append(entry)
    if not units:
        sys.exit("generate.py: no units")
    return units


def run_all(args, units, mode, input_path, scratch):
    """Every fact the tool states in mode, over every unit."""

    def one(index):
        unit_db = os.path.join(scratch, f"{mode}-{index}.json")
        with open(unit_db, "w", encoding="utf-8") as f:
            json.dump([units[index]], f)
        cmd = [args.tool, mode, unit_db, input_path, args.root, *args.prefix]
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode != 0:
            return (units[index]["file"], result.stderr)
        return (None, result.stdout)

    facts = set()
    failures = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        for failed, text in pool.map(one, range(len(units))):
            if failed is not None:
                failures.append((failed, text))
            else:
                facts.update(line for line in text.split("\n") if line)
    if failures:
        for file, stderr in sorted(failures):
            sys.stderr.write(f"generate.py: {mode} failed on {file}:\n{stderr}\n")
        sys.exit(1)
    return facts


def line_order(location):
    path, _, line = location.rpartition(":")
    return (path, int(line))


def main(argv):
    args = parse_args(argv)
    units = select_units(args.db, args.units)
    with open(args.requests, encoding="utf-8") as f:
        requested = sorted({line.split("\t")[0] for line in f if line.strip()})

    with tempfile.TemporaryDirectory() as scratch:
        routines = {}  # usr -> (name, signature, location)
        keys = {}  # key -> {usr}
        templates = set()
        calls = set()
        for fact in run_all(args, units, "routines", args.requests, scratch):
            fields = fact.split("\t")
            if fields[0] == "routine":
                _, key, usr, name, signature, location = fields
                seen = routines.setdefault(usr, (name, signature, location))
                if seen != (name, signature, location):
                    sys.exit(f"generate.py: units disagree on the routine {usr}: {seen} and {fields[3:]}")
                keys.setdefault(key, set()).add(usr)
            elif fields[0] == "template":
                templates.add(fields[1])
            elif fields[0] == "call":
                calls.add(tuple(fields[1:]))
            else:
                sys.exit(f"generate.py: an unknown fact: {fact}")

        dispatches = {"dispatch": "call", "dispatch-delete": "delete"}
        needs = sorted({(c[8], c[6], dispatches[c[3]]) for c in calls if c[3] in dispatches})
        overriders = {}  # (receiver, method, call or delete) -> {(name, signature, usr)}
        deletes = {}  # (receiver, method, delete, class) -> {(name, signature, usr)}
        reached = set()  # (receiver, method, delete, class), for each class a deleting call reaches
        if needs:
            needs_path = os.path.join(scratch, "needs.tsv")
            with open(needs_path, "w", encoding="utf-8") as f:
                for need in needs:
                    f.write("\t".join(need) + "\n")
            for fact in run_all(args, units, "overriders", needs_path, scratch):
                fields = fact.split("\t")
                if fields[0] == "overrider":
                    _, receiver, method, variant, cls, name, signature, usr = fields
                    overriders.setdefault((receiver, method, variant), set()).add((name, signature, usr))
                    if variant == "delete":
                        reached.add((receiver, method, variant, cls))
                elif fields[0] == "overrider-delete":
                    _, receiver, method, variant, cls, name, signature, usr = fields
                    deletes.setdefault((receiver, method, variant, cls), set()).add((name, signature, usr))
                else:
                    sys.exit(f"generate.py: an unknown fact: {fact}")
        unfreed = sorted(reached - set(deletes))
        if unfreed:
            sys.exit(f"generate.py: no unit names the operator delete of these deleting destructors: {unfreed}")

    unknown = set(keys) | templates
    unknown -= set(requested)
    if unknown:
        sys.exit(f"generate.py: facts for keys never requested: {sorted(unknown)}")

    lines = []
    for key in requested:
        if key in keys:
            for usr in sorted(keys[key], key=lambda u: (routines[u], u)):
                name, signature, location = routines[usr]
                lines.append(("routine", key, name, signature, usr, location))
        elif key in templates:
            lines.append(("uninstantiated", key))
        else:
            lines.append(("undefined", key))

    rows = set()
    for routine_usr, location, macro, kind, name, signature, usr, enums, receiver in calls:
        if routine_usr not in routines:
            sys.exit(f"generate.py: a call from a routine no unit defines: {routine_usr}")
        routine = routines[routine_usr]
        head = (routine[0], routine[1], routine_usr, location, macro)
        if kind in dispatches:
            need = (receiver, usr, dispatches[kind])
            found = overriders.get(need, set())
            if found:
                for o_name, o_signature, o_usr in found:
                    rows.add(head + ("virtual", o_name, o_signature, o_usr, enums))
            else:
                rows.add(head + ("virtual-unimplemented", name, signature, usr, enums))
            for (r, m, v, _cls), freed in deletes.items():
                if (r, m, v) == need:
                    for d_name, d_signature, d_usr in freed:
                        rows.add(head + ("delete", d_name, d_signature, d_usr, "-"))
        else:
            rows.add(head + (kind, name, signature, usr, enums))
    for row in sorted(rows, key=lambda r: (r[0], r[1], r[2], line_order(r[3]), r[4:])):
        lines.append(("call",) + row)

    text = "".join(f"# {h}\n" for h in args.header) + COLUMNS + "".join("\t".join(l) + "\n" for l in lines)
    for forbidden in ["/nix/store", os.path.realpath(args.root), args.root]:
        if forbidden in text:
            sys.exit(f"generate.py: the graph names a path it should not: {forbidden}")
    with open(args.out, "w", encoding="utf-8") as f:
        f.write(text)


if __name__ == "__main__":
    main(sys.argv[1:])

#!/usr/bin/env python3
"""The paths in this repository that each package's build, and the test suite, read.

A *footprint* is a set of repository paths, each a file or a directory, and it
covers a file when it names that file or a directory above it. Three consumers
read the same footprints, and agree on what they cover:
  * Nerdbank.GitVersioning reads a package's footprint as the `pathFilters` in
    its version.json, so the package's version moves exactly when something it
    is built from does.
  * CI reads the test footprint from .github/test-footprint.json, runs the tests
    in a checkout holding only that footprint, and skips them on a pull request
    that changes nothing in it.
  * git's sparse checkout materialises a footprint, so that `clone` and `verify`
    can build from one alone: a read it does not list then fails, rather than
    going unnoticed.

Usage:
  scripts/footprint.py generate          write every footprint, from the projects
  scripts/footprint.py check             exit 1 unless the committed footprints are the generated ones
  scripts/footprint.py affected BASE HEAD
  scripts/footprint.py affected --merge REV
                                         print `true` if the change can affect the tests, else `false`
  scripts/footprint.py clone TARGET DEST [--rev REV]
                                         check out REV (default HEAD) at DEST, holding only TARGET's
                                         footprint: `tests`, a package's directory, or `whole` for all
  scripts/footprint.py test-projects     print each test project, one per line
  scripts/footprint.py verify {packages,tests,all}
                                         build each footprint from a checkout of it alone

`generate`, `check` and `verify` run MSBuild, so run them from the devshell:
`nix develop -c python3 scripts/footprint.py generate`. The others need only git.

Exit status: 0 on success, 1 when `check` or `verify` finds a difference, 2 when
the question could not be answered.

How a footprint is computed. A project's footprint is the union of
  * its own directory;
  * every tracked file its evaluation names: its items of the types in
    ITEM_TYPES, the files it imports, and its `FootprintInput` items, which are how
    a project declares a read MSBuild cannot see (a target that reads a file by
    path, a test that reads one at run time). A FootprintInput may name a directory;
  * every tracked file at the repository root except those in
    ROOT_FILES_NOT_READ_BY_EVERY_BUILD;
  * the footprints of the projects it references.
A package is a project with a version.json beside it, and its footprint is its
project's. The test footprint is the union of every test project's, with
TEST_RUNNER_INPUTS.

How each is checked. `check` says the committed footprints are what the projects
describe now, so a new reference or a new item outside a project's directory
changes them in the same commit. `verify packages` packs each package twice at the
same commit, once from the whole tree and once from a checkout of its footprint
alone, and requires the two packages to be byte-identical; the builds are
deterministic, so a difference means something outside the footprint changed the
output. `verify tests` builds each test project from the test footprint alone, and
CI goes further, running the whole suite there.

WHAT THIS CANNOT SEE:
  * A read that tolerates absence. A test that lists a directory and checks each
    file, or reads a file only if it exists, passes in a checkout without it. The
    tests that read the repository at run time all locate it by walking up to the
    solution file, and each fails on a missing file.
  * Inputs from outside the repository: the toolchain (pinned by flake.lock, which
    every footprint includes), NuGet packages (pinned by version, in project files),
    the CI runner's image and the environment.
  * Which of a package's inputs only its packing reads (its readme, its icon). They
    are in the footprint of every project that references it, including the tests',
    so changing one runs the tests. This over-approximates, which is safe.
"""

import argparse
import concurrent.futures
import dataclasses
import difflib
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ElementTree
import zipfile
from typing import Iterable, Mapping, Optional, Sequence

SOLUTION = "WoofWare.PawPrint.slnx"
TEST_FOOTPRINT_FILE = ".github/test-footprint.json"

# What decides how CI runs the tests, and so belongs in the test footprint although
# no test project reads it: the workflow, the footprint file itself, and this
# script, which CI asks whether to run the tests and which builds the checkout they
# run in.
TEST_RUNNER_INPUTS = frozenset({".github", "scripts/footprint.py"})

# Root files that not every build reads. Every other tracked file at the root is in
# every footprint, because the tools look for files there by name: MSBuild for
# Directory.Build.*, NuGet for NuGet.config, the SDK for global.json, and SourceLink
# embeds in a PDB only the generated sources `.gitignore` ignores (measured: a package
# packed without `.gitignore` lacks its version file's source). So a new root file is
# in every footprint until it is listed here. A project that reads one of these says
# so with a FootprintInput, and `verify` catches one that is read without saying so,
# since the build then runs without it.
ROOT_FILES_NOT_READ_BY_EVERY_BUILD = frozenset(
    {
        ".editorconfig",
        ".envrc",
        ".fantomasignore",
        "AGENTS.md",
        "CLAUDE.md",
        "LICENCE.md",
        "LICENCE_dotnet.md",
        "README.md",
        SOLUTION,
        "dotnet-tools.json",
    }
)

# The item types whose members are files a build reads. One missing from this list
# is not seen by `generate`, and is caught by `verify`.
ITEM_TYPES = (
    "Compile",
    "CompileBefore",
    "CompileAfter",
    "EmbeddedResource",
    "None",
    "Content",
    "Resource",
    "FootprintInput",
)

# The configuration CI builds, tests and packs, and so the one whose items count.
CONFIGURATION = "Release"


class FootprintError(Exception):
    """A question this script could not answer, or a footprint it will not write."""


# ---------------------------------------------------------------------------------
# The pure core.

_COMPONENT = re.compile(r"[A-Za-z0-9_.+-]+")


def check_path(path: str) -> str:
    """Returns `path` if it is a repository-relative path every consumer reads the same
    way without escaping: '/'-separated components of letters, digits and `_.+-`, none
    of them `.` or `..`. Raises FootprintError otherwise."""
    if not all(_COMPONENT.fullmatch(c) and c not in (".", "..") for c in path.split("/")):
        raise FootprintError(
            f"{path!r} is not a path that a sparse-checkout pattern and a pathFilter both read literally"
        )
    return path


def covers(footprint: Iterable[str], path: str) -> bool:
    """Whether some entry of the footprint is `path` or a directory above it."""
    return any(path == e or path.startswith(e + "/") for e in footprint)


def minimise(paths: Iterable[str]) -> tuple:
    """The footprint covering what `paths` covers, with no entry under another, sorted."""
    distinct = {check_path(p) for p in paths}
    if not distinct:
        raise FootprintError("a footprint must name something: an empty one would say nothing is read")
    return tuple(sorted(p for p in distinct if not covers(distinct - {p}, p)))


@dataclasses.dataclass(frozen=True)
class Project:
    """What one project's evaluation says it reads.

    `path` is the project file and `references` the project files it references,
    each repository-relative. `inputs` are the repository paths it reads itself."""

    path: str
    inputs: frozenset
    references: frozenset


def project_footprints(projects: Mapping[str, Project], universal: frozenset) -> dict:
    """Each project's footprint: `universal`, its inputs, and its references' footprints."""
    footprints: dict = {}
    visiting: set = set()

    def visit(path: str, referrer: Optional[str]) -> tuple:
        if path in footprints:
            return footprints[path]
        if path not in projects:
            raise FootprintError(f"{referrer} references {path}, which was not evaluated")
        if path in visiting:
            raise FootprintError(f"project references form a cycle through {path}")
        visiting.add(path)
        parts = set(universal) | set(projects[path].inputs)
        for reference in sorted(projects[path].references):
            parts.update(visit(reference, path))
        visiting.discard(path)
        footprints[path] = minimise(parts)
        return footprints[path]

    for path in sorted(projects):
        visit(path, None)
    return footprints


def universal_inputs(tracked_files: Iterable[str]) -> frozenset:
    """The root files in every footprint: every tracked one not known to be read only by some."""
    root_files = {f for f in tracked_files if "/" not in f}
    stale = ROOT_FILES_NOT_READ_BY_EVERY_BUILD - root_files
    if stale:
        raise FootprintError(
            f"ROOT_FILES_NOT_READ_BY_EVERY_BUILD names {sorted(stale)}, which are not tracked files at the root"
        )
    return frozenset(root_files - ROOT_FILES_NOT_READ_BY_EVERY_BUILD)


def path_filters(footprint: Iterable[str]) -> list:
    """The footprint as Nerdbank.GitVersioning's pathFilters, each relative to the root."""
    return [":/" + check_path(p) for p in footprint]


def parse_path_filters(filters: Sequence[str]) -> tuple:
    """The footprint that `path_filters` wrote, refusing any filter it would not write."""
    if not isinstance(filters, list) or not filters:
        raise FootprintError(f"expected a non-empty list of pathFilters, found {filters!r}")
    for f in filters:
        if not isinstance(f, str) or not f.startswith(":/"):
            raise FootprintError(f"pathFilter {f!r} is not one `generate` writes; regenerate it")
    return minimise(f[2:] for f in filters)


def render_version_json(existing: str, footprint: Sequence[str]) -> str:
    """`existing` with its pathFilters replaced by the footprint, and nothing else changed."""
    document = json.loads(existing)
    if not isinstance(document, dict):
        raise FootprintError("a version.json must hold a JSON object")
    document["pathFilters"] = path_filters(footprint)
    return json.dumps(document, indent=2) + "\n"


def render_test_footprint(test_projects: Sequence[str], footprint: Sequence[str]) -> str:
    """The contents of TEST_FOOTPRINT_FILE."""
    document = {
        "generatedBy": "scripts/footprint.py generate",
        "testProjects": [check_path(p) for p in test_projects],
        "paths": [check_path(p) for p in footprint],
    }
    return json.dumps(document, indent=2) + "\n"


def parse_test_footprint(text: str) -> tuple:
    """The test projects and the footprint that `render_test_footprint` wrote."""
    document = json.loads(text)
    if not isinstance(document, dict) or set(document) != {"generatedBy", "testProjects", "paths"}:
        raise FootprintError(f"{TEST_FOOTPRINT_FILE} is not in the shape `generate` writes; regenerate it")
    projects = tuple(check_path(p) for p in document["testProjects"])
    if not projects:
        raise FootprintError(f"{TEST_FOOTPRINT_FILE} names no test project")
    return projects, minimise(document["paths"])


def sparse_patterns(footprint: Iterable[str]) -> list:
    """The footprint as non-cone sparse-checkout patterns, each anchored at the root."""
    return ["/" + check_path(p) for p in footprint]


def imports_from_preprocessed(text: str) -> list:
    """The files a `dotnet msbuild -preprocess` listing imports, in order, each once.

    The preprocessor names each imported file on a line of its own, as an absolute
    path. A line in the project text proper that happens to be one too would be
    counted, which can only enlarge a footprint."""
    found: list = []
    for line in text.splitlines():
        candidate = line.strip()
        if candidate.startswith("/") and candidate not in found:
            found.append(candidate)
    return found


@dataclasses.dataclass(frozen=True)
class Evaluation:
    """What `dotnet msbuild -getItem -getProperty` printed: properties by name, and
    each item as its type and full path."""

    properties: dict
    items: list


def parse_evaluation(text: str) -> Evaluation:
    document = json.loads(text)
    items = [(t, item["FullPath"]) for t, members in document.get("Items", {}).items() for item in members]
    return Evaluation(dict(document.get("Properties", {})), items)


# ---------------------------------------------------------------------------------
# The shell: git, MSBuild and the filesystem.


def _run(args: Sequence[str], cwd: str, input: Optional[str] = None) -> str:
    result = subprocess.run(list(args), cwd=cwd, input=input, capture_output=True, text=True)
    if result.returncode != 0:
        raise FootprintError(
            f"`{' '.join(args)}` in {cwd} exited {result.returncode}:\n{result.stdout}{result.stderr}"
        )
    return result.stdout


def _git(cwd: str, *args: str, input: Optional[str] = None) -> str:
    return _run(["git", *args], cwd, input=input)


def repo_root(start: str = ".") -> str:
    return os.path.realpath(_git(start, "rev-parse", "--show-toplevel").strip())


def _commit(root: str, rev: str) -> str:
    return _git(root, "rev-parse", "--verify", "--quiet", rev + "^{commit}").strip()


def tracked_files(root: str, rev: Optional[str] = None) -> list:
    """The files git tracks: in the index, or at `rev`."""
    if rev is None:
        out = _git(root, "ls-files", "-z")
    else:
        out = _git(root, "ls-tree", "-r", "-z", "--name-only", _commit(root, rev))
    return [p for p in out.split("\0") if p]


def _file_at(root: str, rev: str, path: str) -> Optional[str]:
    commit = _commit(root, rev)
    if subprocess.run(["git", "cat-file", "-e", f"{commit}:{path}"], cwd=root, capture_output=True).returncode:
        return None
    return _git(root, "show", f"{commit}:{path}")


def sparse_clone(source: str, rev: str, footprint: Optional[Sequence[str]], dest: str) -> None:
    """Clones `source` at `dest`, detached at `rev`, with only the tracked files the
    footprint covers checked out, or every tracked file when it is None.

    The clone borrows `source`'s objects, so it has the whole history that
    Nerdbank.GitVersioning walks, and copies `source`'s `origin`, from which SourceLink
    computes the repository URL that each PDB records."""
    if os.path.lexists(dest):
        raise FootprintError(f"{dest} already exists")
    commit = _commit(source, rev)
    _git(source, "clone", "--quiet", "--shared", "--no-checkout", source, dest)
    origin = subprocess.run(
        ["git", "config", "--get", "remote.origin.url"], cwd=source, capture_output=True, text=True
    )
    if origin.returncode == 0:
        _git(dest, "remote", "set-url", "origin", origin.stdout.strip())
    else:
        _git(dest, "remote", "remove", "origin")
    if footprint is not None:
        _git(dest, "sparse-checkout", "set", "--no-cone", "--stdin", input="\n".join(sparse_patterns(footprint)) + "\n")
    _git(dest, "checkout", "--quiet", "--detach", commit)


def test_footprint_at(root: str, rev: str) -> Optional[tuple]:
    """The test projects and test footprint committed at `rev`, or None if it has none."""
    text = _file_at(root, rev, TEST_FOOTPRINT_FILE)
    return None if text is None else parse_test_footprint(text)


def affected_between(root: str, base: str, head: str) -> bool:
    """Whether a path in the test footprint at `base` or at `head` differs between them.

    Both footprints count: a change that adds a file to what the tests read has it only
    in the head's, and one that stops reading a file has it only in the base's."""
    head_footprint = test_footprint_at(root, head)
    if head_footprint is None:
        raise FootprintError(f"{head} has no {TEST_FOOTPRINT_FILE}")
    base_footprint = test_footprint_at(root, base)
    if base_footprint is None:
        return True
    union = base_footprint[1] + head_footprint[1]
    changed = _git(
        root, "diff", "--name-only", "--no-renames", "--no-ext-diff", "--no-color", "-z", _commit(root, base), _commit(root, head)
    )
    return any(covers(union, p) for p in changed.split("\0") if p)


def affected_by_merge(root: str, rev: str) -> bool:
    """`affected_between` a merge commit's first parent and the merge.

    GitHub runs a pull request's checks on its merge into the base branch, whose first
    parent is that branch: the merge differs from it by exactly what the pull request
    would change. Comparing with the merge base instead would count the base branch's
    own changes since the branch was cut."""
    parents = _git(root, "rev-list", "--parents", "-n", "1", _commit(root, rev)).split()[1:]
    if len(parents) != 2:
        raise FootprintError(
            f"{rev} has {len(parents)} parents, where GitHub's merge of a pull request into its base has two"
        )
    return affected_between(root, parents[0], rev)


def _repo_relative(root: str, path: str) -> Optional[str]:
    """`path` relative to the repository root, or None if it is not strictly inside it."""
    relative = os.path.relpath(os.path.realpath(path), root)
    if relative == "." or relative == ".." or relative.startswith(".." + os.sep):
        return None
    return relative.replace(os.sep, "/")


def _msbuild(root: str, project: str, *args: str) -> str:
    return _run(["dotnet", "msbuild", os.path.join(root, project), "-nologo", f"-p:Configuration={CONFIGURATION}", *args], root)


def evaluate(root: str, project: str, tracked: frozenset, tracked_directories: frozenset) -> tuple:
    """The Project that MSBuild's evaluation of `project` describes, and its properties."""
    directory = os.path.dirname(project)
    if not directory:
        raise FootprintError(f"{project} is at the repository root, so its directory would be the whole repository")
    with tempfile.TemporaryDirectory() as tmp:
        preprocessed = os.path.join(tmp, "preprocessed.xml")
        _msbuild(root, project, f"-preprocess:{preprocessed}")
        with open(preprocessed) as f:
            imports = imports_from_preprocessed(f.read())
    evaluation = parse_evaluation(
        _msbuild(
            root,
            project,
            "-getItem:" + ",".join(ITEM_TYPES + ("ProjectReference",)),
            "-getProperty:IsTestProject,IsPackable",
        )
    )
    inputs = {directory}
    references = set()
    for item_type, full_path in evaluation.items:
        relative = _repo_relative(root, full_path)
        if item_type == "ProjectReference":
            if relative not in tracked:
                raise FootprintError(f"{project} references {full_path}, which is not a tracked file in this repository")
            references.add(relative)
        elif item_type == "FootprintInput":
            if relative not in tracked and relative not in tracked_directories:
                raise FootprintError(
                    f"{project} declares the FootprintInput {full_path}, which is no tracked file or directory in this repository"
                )
            inputs.add(relative)
        elif relative in tracked:
            inputs.add(relative)
    inputs.update(r for r in map(lambda p: _repo_relative(root, p), imports) if r in tracked)
    return Project(project, frozenset(inputs), frozenset(references)), evaluation.properties


def solution_projects(root: str) -> list:
    paths = [p.get("Path").replace("\\", "/") for p in ElementTree.parse(os.path.join(root, SOLUTION)).iter("Project")]
    return sorted(paths)


def evaluate_all(root: str, tracked: Sequence[str]) -> tuple:
    """Every project in the solution and every project they reference, evaluated."""
    tracked_set = frozenset(tracked)
    directories = frozenset(f.rsplit("/", i)[0] for f in tracked for i in range(1, f.count("/") + 1))
    # Evaluation sees what NuGet packages import only once they are restored.
    _run(["dotnet", "restore", os.path.join(root, SOLUTION), "-nologo", "-v:q"], root)
    projects: dict = {}
    properties: dict = {}
    pending = solution_projects(root)
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        while pending:
            results = list(pool.map(lambda p: evaluate(root, p, tracked_set, directories), pending))
            for path, (project, props) in zip(pending, results):
                projects[path] = project
                properties[path] = props
            pending = sorted({r for project, _ in results for r in project.references} - set(projects))
    return projects, properties


def _is_true(properties: Mapping[str, str], name: str) -> bool:
    return properties.get(name, "").strip().lower() == "true"


def generated_files(root: str) -> dict:
    """Every file `generate` writes, by repository path, with the contents it writes."""
    tracked = tracked_files(root)
    stale_runner_inputs = [p for p in TEST_RUNNER_INPUTS if not any(covers([p], f) for f in tracked)]
    if stale_runner_inputs:
        raise FootprintError(f"TEST_RUNNER_INPUTS names {sorted(stale_runner_inputs)}, which are not tracked")
    projects, properties = evaluate_all(root, tracked)
    footprints = project_footprints(projects, universal_inputs(tracked))

    packages = sorted(p for p in projects if f"{os.path.dirname(p)}/version.json" in tracked)
    packable = sorted(p for p in projects if _is_true(properties[p], "IsPackable"))
    if packages != packable:
        raise FootprintError(
            f"the projects with a version.json beside them are {packages}, but the packable ones are {packable}: "
            "each package needs its own version.json, and nothing else should have one"
        )
    package_directories = {os.path.dirname(p) for p in packages}
    orphans = sorted(f for f in tracked if f.split("/")[-1] == "version.json" and os.path.dirname(f) not in package_directories)
    if orphans:
        raise FootprintError(f"{orphans} belong to no package in the solution")

    tests = sorted(p for p in projects if _is_true(properties[p], "IsTestProject"))
    if not tests:
        raise FootprintError("no project in the solution is a test project")
    test_footprint = minimise([*TEST_RUNNER_INPUTS, *(e for t in tests for e in footprints[t])])

    files = {}
    for package in packages:
        # CI checks the packages' footprints only when the tests are not skipped, which
        # is sound only while every package is built from paths the tests read.
        uncovered = [e for e in footprints[package] if not covers(test_footprint, e)]
        if uncovered:
            raise FootprintError(f"the test footprint does not cover {uncovered}, from {package}'s footprint")
        version_json = f"{os.path.dirname(package)}/version.json"
        with open(os.path.join(root, version_json)) as f:
            files[version_json] = render_version_json(f.read(), footprints[package])
    files[TEST_FOOTPRINT_FILE] = render_test_footprint(tests, test_footprint)
    return files


def _read(root: str, path: str) -> Optional[str]:
    try:
        with open(os.path.join(root, path)) as f:
            return f.read()
    except FileNotFoundError:
        return None


def cmd_generate(root: str) -> int:
    for path, contents in sorted(generated_files(root).items()):
        if _read(root, path) != contents:
            with open(os.path.join(root, path), "w") as f:
                f.write(contents)
            print(f"wrote {path}")
    return 0


def cmd_check(root: str) -> int:
    stale = []
    for path, contents in sorted(generated_files(root).items()):
        current = _read(root, path)
        if current != contents:
            stale.append(path)
            sys.stdout.writelines(
                difflib.unified_diff((current or "").splitlines(True), contents.splitlines(True), f"a/{path}", f"b/{path}")
            )
    if stale:
        print(
            f"\n{', '.join(stale)} no longer describe what the projects read. "
            "Run `nix develop -c python3 scripts/footprint.py generate` and commit the result.",
            file=sys.stderr,
        )
        return 1
    return 0


def package_footprints_at(root: str, rev: str) -> dict:
    """Each package's project file, and the footprint its version.json committed at `rev`."""
    tracked = tracked_files(root, rev)
    packages = {}
    for version_json in (f for f in tracked if f.endswith("/version.json")):
        directory = version_json.rsplit("/", 1)[0]
        project_files = [f for f in tracked if os.path.dirname(f) == directory and f.endswith((".fsproj", ".csproj"))]
        if len(project_files) != 1:
            raise FootprintError(f"expected one project file beside {version_json}, found {project_files}")
        document = json.loads(_file_at(root, rev, version_json))
        packages[project_files[0]] = parse_path_filters(document.get("pathFilters"))
    return packages


def _sha256(path: str) -> str:
    with open(path, "rb") as f:
        return hashlib.sha256(f.read()).hexdigest()


def describe_difference(whole: str, alone: str) -> Optional[str]:
    """None if the two packages are the same bytes, else which of their entries differ."""
    if _sha256(whole) == _sha256(alone):
        return None
    with zipfile.ZipFile(whole) as a, zipfile.ZipFile(alone) as b:
        names_a, names_b = set(a.namelist()), set(b.namelist())
        differing = [
            n for n in sorted(names_a | names_b) if n not in names_a or n not in names_b or a.read(n) != b.read(n)
        ]
    return ", ".join(differing) if differing else "the archives, though every entry is the same"


def _pack(tree: str, project: str, out: str) -> str:
    _run(
        [
            "dotnet",
            "pack",
            project,
            "-c",
            CONFIGURATION,
            "-p:ContinuousIntegrationBuild=true",
            "--disable-build-servers",
            "-nologo",
            "-v:q",
            "-o",
            out,
        ],
        tree,
    )
    packages = [f for f in os.listdir(out) if f.endswith(".nupkg")]
    if len(packages) != 1:
        raise FootprintError(f"expected packing {project} to produce one .nupkg in {out}, found {packages}")
    return os.path.join(out, packages[0])


def _progress(message: str) -> None:
    print(message, flush=True)


def cmd_verify(root: str, what: str) -> int:
    rev = _commit(root, "HEAD")
    work = tempfile.mkdtemp(prefix="footprint-verify-")
    # Every build happens at this one path. fsc lays out a DLL's debug directory by the
    # length of the path it builds at, mapped paths or not (measured: two clones of one
    # commit at paths one byte apart in length gave DLLs four bytes apart), so builds at
    # different paths would differ whatever their footprints held.
    tree = os.path.join(work, "tree")
    failures = []

    if what in ("packages", "all"):
        packages = package_footprints_at(root, rev)
        sparse_clone(root, rev, None, tree)
        whole = {}
        for project in sorted(packages):
            _progress(f"packing {project} from the whole tree")
            whole[project] = _pack(tree, project, os.path.join(work, "whole", os.path.dirname(project)))
        for project, footprint in sorted(packages.items()):
            _progress(f"packing {project} from its footprint alone")
            shutil.rmtree(tree)
            sparse_clone(root, rev, footprint, tree)
            try:
                alone = _pack(tree, project, os.path.join(work, "footprint", os.path.dirname(project)))
            except FootprintError as e:
                failures.append(f"{project} cannot be packed from its footprint alone, so it reads something its pathFilters do not name:\n{e}")
                continue
            difference = describe_difference(whole[project], alone)
            if difference is not None:
                failures.append(
                    f"{project} packed from its footprint alone differs from the whole tree's package in {difference}, "
                    "so something outside its pathFilters changes the build"
                )

    if what in ("tests", "all"):
        test_projects, footprint = parse_test_footprint(_file_at(root, rev, TEST_FOOTPRINT_FILE))
        shutil.rmtree(tree, ignore_errors=True)
        sparse_clone(root, rev, footprint, tree)
        for project in test_projects:
            _progress(f"building {project} from the test footprint alone")
            try:
                _run(["dotnet", "build", project, "-c", CONFIGURATION, "--disable-build-servers", "-nologo", "-v:q"], tree)
            except FootprintError as e:
                failures.append(f"{project} cannot be built from the test footprint alone, so it reads something the footprint does not name:\n{e}")

    if failures:
        for failure in failures:
            print(failure, file=sys.stderr)
        print(f"\nThe builds are in {work}. Name what is missing in a FootprintInput item, then run `generate`.", file=sys.stderr)
        return 1
    shutil.rmtree(work)
    _progress("every footprint builds alone, as the whole tree does")
    return 0


def cmd_clone(root: str, target: str, dest: str, rev: str) -> int:
    if target == "whole":
        footprint = None
    elif target == "tests":
        found = test_footprint_at(root, rev)
        if found is None:
            raise FootprintError(f"{rev} has no {TEST_FOOTPRINT_FILE}")
        footprint = found[1]
    else:
        matching = [fp for project, fp in package_footprints_at(root, rev).items() if os.path.dirname(project) == target]
        if not matching:
            raise FootprintError(f"{target} is neither `tests`, `whole`, nor a package's directory")
        footprint = matching[0]
    sparse_clone(root, rev, footprint, dest)
    return 0


def main(argv: Sequence[str]) -> int:
    parser = argparse.ArgumentParser(description="The paths each package's build, and the test suite, read.")
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("generate")
    commands.add_parser("check")
    affected = commands.add_parser("affected")
    affected.add_argument("--merge", metavar="REV")
    affected.add_argument("revs", nargs="*", metavar="REV")
    clone = commands.add_parser("clone")
    clone.add_argument("target")
    clone.add_argument("dest")
    clone.add_argument("--rev", default="HEAD")
    commands.add_parser("test-projects")
    verify = commands.add_parser("verify")
    verify.add_argument("what", choices=["packages", "tests", "all"])
    args = parser.parse_args(argv)

    try:
        root = repo_root()
        if args.command == "generate":
            return cmd_generate(root)
        if args.command == "check":
            return cmd_check(root)
        if args.command == "affected":
            if args.merge is not None and not args.revs:
                result = affected_by_merge(root, args.merge)
            elif args.merge is None and len(args.revs) == 2:
                result = affected_between(root, *args.revs)
            else:
                parser.error("affected takes either BASE HEAD or --merge REV")
            print("true" if result else "false")
            return 0
        if args.command == "clone":
            return cmd_clone(root, args.target, os.path.abspath(args.dest), args.rev)
        if args.command == "test-projects":
            text = _read(root, TEST_FOOTPRINT_FILE)
            if text is None:
                raise FootprintError(f"there is no {TEST_FOOTPRINT_FILE}")
            print("\n".join(parse_test_footprint(text)[0]))
            return 0
        if args.command == "verify":
            return cmd_verify(root, args.what)
        raise AssertionError(args.command)
    except FootprintError as e:
        print(f"footprint: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

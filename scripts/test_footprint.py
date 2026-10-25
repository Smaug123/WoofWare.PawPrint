#!/usr/bin/env python3
"""Executable contract for footprint.py.

Usage: scripts/test_footprint.py [path/to/footprint.py]

The pure core is held to properties over generated inputs: minimising a footprint
covers exactly what it covered before, a project's footprint covers exactly what
its references reach, and every rendered file parses back to what was rendered.
The two places a footprint meets git are held to git itself, in throwaway
repositories: a sparse clone of a footprint holds exactly the tracked files the
footprint covers, and `affected` reports a change exactly when a changed path is
covered. Together those say the model of "covers" that the generator, the skip and
the sparse checkout each rely on is the one git implements. (Nerdbank.GitVersioning,
the third consumer, matches its pathFilters at path-component boundaries too; that
was measured by hand, since it needs a .NET build.)

HYPOTHESIS_PROFILE=explore runs many more examples, randomly; the default is
derandomized so that the flake check gives the same answer every time.
"""

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest

from hypothesis import HealthCheck, given, settings
from hypothesis import strategies as st

settings.register_profile(
    "deterministic",
    derandomize=True,
    database=None,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
settings.register_profile(
    "explore",
    max_examples=2000,
    database=None,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
settings.load_profile(os.environ.get("HYPOTHESIS_PROFILE", "deterministic"))


def _load_module(path):
    spec = importlib.util.spec_from_file_location("footprint", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


FOOTPRINT_PY = (
    sys.argv.pop(1)
    if len(sys.argv) > 1
    else os.path.join(os.path.dirname(os.path.abspath(__file__)), "footprint.py")
)
fp = _load_module(FOOTPRINT_PY)

# Few, similar components, so that generated paths collide often and sit on the
# boundaries that matter: `a` is a string prefix of `a.b` and `ab` without being a
# directory above them, and `a.b` sorts between `a` and `a/b`.
COMPONENT = st.sampled_from(["a", "b", "a.b", "ab", ".a", "a-b"])
PATH = st.lists(COMPONENT, min_size=1, max_size=4).map("/".join)
PATHS = st.lists(PATH, min_size=1, max_size=8)


def covers_by_components(footprint, path):
    """An independent statement of `covers`: some entry's components begin the path's."""
    parts = path.split("/")
    return any(parts[: len(e.split("/"))] == e.split("/") for e in footprint)


def file_tree(paths):
    """The paths that can all be files at once: none is a directory above another."""
    distinct = sorted(set(paths))
    return [p for p in distinct if not any(q.startswith(p + "/") for q in distinct)]


class TestCovers(unittest.TestCase):
    @given(PATHS, PATH)
    def test_agrees_with_component_prefix(self, footprint, path):
        self.assertEqual(fp.covers(footprint, path), covers_by_components(footprint, path))

    def test_is_not_a_string_prefix(self):
        self.assertFalse(fp.covers(["a"], "a.b"))
        self.assertFalse(fp.covers(["a"], "ab/c"))
        self.assertTrue(fp.covers(["a"], "a/b"))
        self.assertTrue(fp.covers(["a"], "a"))


class TestMinimise(unittest.TestCase):
    @given(PATHS, PATH)
    def test_covers_what_the_input_covered(self, paths, probe):
        result = fp.minimise(paths)
        # Each input path, and a file beneath it, must stay covered; the random probe
        # catches covering more.
        for p in [probe, *paths, *(q + "/a" for q in paths)]:
            self.assertEqual(fp.covers(result, p), fp.covers(paths, p), p)

    @given(PATHS)
    def test_is_sorted_and_no_entry_covers_another(self, paths):
        result = fp.minimise(paths)
        self.assertEqual(list(result), sorted(set(result)))
        for e in result:
            self.assertFalse(fp.covers([o for o in result if o != e], e), (e, result))

    @given(PATHS)
    def test_is_idempotent(self, paths):
        once = fp.minimise(paths)
        self.assertEqual(fp.minimise(once), once)

    def test_refuses_nothing(self):
        with self.assertRaises(fp.FootprintError):
            fp.minimise([])


class TestCheckPath(unittest.TestCase):
    def test_accepts_repository_relative_paths(self):
        for p in ["a", ".github", "a/b.c", "WoofWare.PawPrint.Test/sourcesPure/X.cs", ".gitignore"]:
            self.assertEqual(fp.check_path(p), p)

    def test_refuses_what_a_consumer_would_read_differently(self):
        for p in ["", "/a", "a/", "a//b", "./a", "a/./b", "a/../b", "..", "a b", "a*", "a?b", "[a]", "a\\b", "!a", "#a", ":/a"]:
            with self.assertRaises(fp.FootprintError, msg=p):
                fp.check_path(p)


@st.composite
def project_graphs(draw):
    """Projects p0..pn, each referencing only lower-numbered ones, so the graph is acyclic."""
    n = draw(st.integers(min_value=1, max_value=6))
    projects = {}
    for i in range(n):
        refs = draw(st.sets(st.sampled_from([f"p{j}/p{j}.fsproj" for j in range(i)]))) if i > 0 else set()
        inputs = draw(st.sets(PATH, max_size=4))
        path = f"p{i}/p{i}.fsproj"
        projects[path] = fp.Project(path=path, inputs=frozenset(inputs | {f"p{i}"}), references=frozenset(refs))
    universal = frozenset(draw(st.sets(PATH, max_size=3)))
    return projects, universal


def reachable(projects, start):
    seen = set()
    stack = [start]
    while stack:
        p = stack.pop()
        if p not in seen:
            seen.add(p)
            stack.extend(projects[p].references)
    return seen


class TestProjectFootprints(unittest.TestCase):
    @given(project_graphs(), PATH)
    def test_covers_exactly_what_the_references_reach(self, graph, probe):
        projects, universal = graph
        footprints = fp.project_footprints(projects, universal)
        self.assertEqual(set(footprints), set(projects))
        # Every path the graph names, so that an input reachable only through a chain of
        # references is probed; the random probe catches covering more.
        probes = {probe, *universal, *(i for project in projects.values() for i in project.inputs)}
        for path, footprint in footprints.items():
            self.assertEqual(footprint, fp.minimise(footprint))
            for p in sorted(probes):
                expected = fp.covers(universal, p) or any(
                    fp.covers(projects[q].inputs, p) for q in reachable(projects, path)
                )
                self.assertEqual(fp.covers(footprint, p), expected, (path, p))

    def test_refuses_a_cycle(self):
        projects = {
            "a/a.fsproj": fp.Project("a/a.fsproj", frozenset({"a"}), frozenset({"b/b.fsproj"})),
            "b/b.fsproj": fp.Project("b/b.fsproj", frozenset({"b"}), frozenset({"a/a.fsproj"})),
        }
        with self.assertRaises(fp.FootprintError):
            fp.project_footprints(projects, frozenset())

    def test_refuses_a_reference_to_an_unevaluated_project(self):
        projects = {"a/a.fsproj": fp.Project("a/a.fsproj", frozenset({"a"}), frozenset({"b/b.fsproj"}))}
        with self.assertRaises(fp.FootprintError):
            fp.project_footprints(projects, frozenset())


class TestUniversalInputs(unittest.TestCase):
    def test_is_every_root_file_but_those_listed(self):
        listed = sorted(fp.ROOT_FILES_NOT_READ_BY_EVERY_BUILD)
        tracked = listed + ["Directory.Build.props", ".gitignore", "a/b", "a/Directory.Build.props"]
        self.assertEqual(fp.universal_inputs(tracked), frozenset({"Directory.Build.props", ".gitignore"}))

    def test_refuses_a_listed_file_that_is_not_tracked(self):
        listed = sorted(fp.ROOT_FILES_NOT_READ_BY_EVERY_BUILD)
        with self.assertRaises(fp.FootprintError):
            fp.universal_inputs(listed[1:] + ["Directory.Build.props"])


class TestRendering(unittest.TestCase):
    @given(PATHS)
    def test_path_filters_round_trip(self, paths):
        footprint = fp.minimise(paths)
        self.assertEqual(fp.parse_path_filters(fp.path_filters(footprint)), footprint)

    def test_path_filters_refuse_what_they_do_not_write(self):
        for filters in [["./"], ["a"], [":!a"], [":/a", "./b"], []]:
            with self.assertRaises(fp.FootprintError, msg=filters):
                fp.parse_path_filters(filters)

    @given(PATHS)
    def test_version_json_keeps_everything_but_the_filters(self, paths):
        footprint = fp.minimise(paths)
        existing = '{\n  "version": "0.4",\n  "publicReleaseRefSpec": [\n    "^refs/heads/main$"\n  ],\n  "pathFilters": [\n    "./"\n  ]\n}\n'
        rendered = fp.render_version_json(existing, footprint)
        parsed = json.loads(rendered)
        self.assertEqual(list(parsed), ["version", "publicReleaseRefSpec", "pathFilters"])
        self.assertEqual(parsed["version"], "0.4")
        self.assertEqual(parsed["publicReleaseRefSpec"], ["^refs/heads/main$"])
        self.assertEqual(fp.parse_path_filters(parsed["pathFilters"]), footprint)
        self.assertEqual(fp.render_version_json(rendered, footprint), rendered)

    @given(PATHS, st.lists(PATH, min_size=1, max_size=3, unique=True))
    def test_test_footprint_round_trips(self, paths, projects):
        footprint = fp.minimise(paths)
        rendered = fp.render_test_footprint(sorted(projects), footprint)
        self.assertEqual(fp.parse_test_footprint(rendered), (tuple(sorted(projects)), footprint))
        self.assertTrue(rendered.endswith("\n"))


PREPROCESSED = """<?xml version="1.0" encoding="utf-8"?>
<!--
============================================================================================================================================
/repo/A/A.fsproj
============================================================================================================================================
-->
<Project>
  <!--
============================================================================================================================================
  <Import Project="$(DirectoryBuildPropsPath)" Condition="exists('$(DirectoryBuildPropsPath)')">

/repo/Directory.Build.props
============================================================================================================================================
-->
  <PropertyGroup>
    <Thing>/repo/not/an/import</Thing>
  </PropertyGroup>
  <!--
============================================================================================================================================
  </Import>

/repo/A/A.fsproj
============================================================================================================================================
-->
</Project>
"""


class TestMSBuildOutputParsing(unittest.TestCase):
    def test_imports_are_the_paths_the_preprocessor_names(self):
        self.assertEqual(
            fp.imports_from_preprocessed(PREPROCESSED),
            ["/repo/A/A.fsproj", "/repo/Directory.Build.props"],
        )

    def test_items_are_the_full_paths_of_each_type(self):
        text = json.dumps(
            {
                "Properties": {"IsTestProject": "true"},
                "Items": {
                    "Compile": [{"Identity": "a.fs", "FullPath": "/repo/A/a.fs"}],
                    "FootprintInput": [{"Identity": "../x", "FullPath": "/repo/x"}],
                },
            }
        )
        evaluation = fp.parse_evaluation(text)
        self.assertEqual(evaluation.properties, {"IsTestProject": "true"})
        self.assertEqual(
            sorted(evaluation.items),
            [("Compile", "/repo/A/a.fs"), ("FootprintInput", "/repo/x")],
        )


def run(cwd, *args):
    return subprocess.run(args, cwd=cwd, check=True, capture_output=True, text=True).stdout


def make_repo(directory, files):
    run(directory, "git", "init", "-q", ".")
    run(directory, "git", "config", "user.email", "t@example.invalid")
    run(directory, "git", "config", "user.name", "t")
    run(directory, "git", "config", "commit.gpgsign", "false")
    run(directory, "git", "remote", "add", "origin", "https://example.invalid/repo.git")
    write_files(directory, files)
    run(directory, "git", "add", "-A")
    run(directory, "git", "commit", "-q", "--allow-empty", "-m", "base")
    return run(directory, "git", "rev-parse", "HEAD").strip()


def write_files(directory, files):
    for path, content in files.items():
        full = os.path.join(directory, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(content)


def working_tree_files(directory):
    found = []
    for dirpath, dirnames, filenames in os.walk(directory):
        if ".git" in dirnames:
            dirnames.remove(".git")
        for name in filenames:
            found.append(os.path.relpath(os.path.join(dirpath, name), directory))
    return sorted(found)


GIT_SETTINGS = settings(max_examples=40)


class TestSparseClone(unittest.TestCase):
    @GIT_SETTINGS
    @given(PATHS, PATHS)
    def test_holds_exactly_the_covered_tracked_files(self, files, paths):
        footprint = fp.minimise(paths)
        tracked = file_tree(files)
        with tempfile.TemporaryDirectory() as tmp:
            source = os.path.join(tmp, "source")
            os.mkdir(source)
            rev = make_repo(source, {p: p for p in tracked})
            dest = os.path.join(tmp, "dest")
            fp.sparse_clone(source, rev, footprint, dest)
            self.assertEqual(working_tree_files(dest), sorted(p for p in tracked if fp.covers(footprint, p)))
            self.assertEqual(run(dest, "git", "rev-parse", "HEAD").strip(), rev)
            self.assertEqual(run(dest, "git", "remote", "get-url", "origin").strip(), "https://example.invalid/repo.git")

    def test_without_a_footprint_holds_every_tracked_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = os.path.join(tmp, "source")
            os.mkdir(source)
            rev = make_repo(source, {"a/b": "1", ".gitignore": "x\n", "c": "2"})
            write_files(source, {"untracked": "3"})
            dest = os.path.join(tmp, "dest")
            fp.sparse_clone(source, rev, None, dest)
            self.assertEqual(working_tree_files(dest), [".gitignore", "a/b", "c"])

    def test_refuses_an_existing_destination(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = os.path.join(tmp, "source")
            os.mkdir(source)
            rev = make_repo(source, {"a": "1"})
            with self.assertRaises(fp.FootprintError):
                fp.sparse_clone(source, rev, ["a"], tmp)


@st.composite
def changes(draw):
    """Two trees and the footprints committed with each; a footprint may be absent from the base."""
    before = file_tree(draw(PATHS))
    after = file_tree(draw(PATHS))
    base_footprint = draw(st.none() | PATHS.map(fp.minimise))
    head_footprint = draw(PATHS.map(fp.minimise))
    return before, after, base_footprint, head_footprint


def tree_contents(files, footprint):
    """What `commit_tree` commits: each file holding its own name, and the footprint file."""
    contents = {p: p + "\n" for p in files}
    if footprint is not None:
        contents[fp.TEST_FOOTPRINT_FILE] = fp.render_test_footprint(["t/t.fsproj"], footprint)
    return contents


def commit_tree(directory, files, footprint, message):
    # `git rm` also removes the directories it empties, any of which the new tree may
    # need as a file.
    run(directory, "git", "rm", "-rq", "--ignore-unmatch", ".")
    write_files(directory, tree_contents(files, footprint))
    run(directory, "git", "add", "-A")
    run(directory, "git", "commit", "-q", "--allow-empty", "-m", message)
    return run(directory, "git", "rev-parse", "HEAD").strip()


class TestAffected(unittest.TestCase):
    @GIT_SETTINGS
    @given(changes())
    def test_reports_a_change_exactly_when_a_changed_path_is_covered(self, change):
        before, after, base_footprint, head_footprint = change
        with tempfile.TemporaryDirectory() as tmp:
            make_repo(tmp, {})
            base = commit_tree(tmp, before, base_footprint, "base")
            head = commit_tree(tmp, after, head_footprint, "head")
            old = tree_contents(before, base_footprint)
            new = tree_contents(after, head_footprint)
            changed = {p for p in set(old) | set(new) if old.get(p) != new.get(p)}
            if base_footprint is None:
                # Nothing says what the base's tests read, so nothing says they read
                # none of what changed.
                expected = True
            else:
                union = list(base_footprint) + list(head_footprint)
                expected = any(fp.covers(union, p) for p in changed)
            self.assertEqual(fp.affected_between(tmp, base, head), expected)

    def test_refuses_a_head_without_a_footprint(self):
        with tempfile.TemporaryDirectory() as tmp:
            make_repo(tmp, {})
            base = commit_tree(tmp, ["a/x"], ["a"], "base")
            head = commit_tree(tmp, ["a/x"], None, "head")
            with self.assertRaises(fp.FootprintError):
                fp.affected_between(tmp, base, head)

    def test_a_change_to_what_only_the_base_read_is_a_change(self):
        with tempfile.TemporaryDirectory() as tmp:
            make_repo(tmp, {})
            base = commit_tree(tmp, ["a/x", "b/x"], ["a", "b"], "base")
            head = commit_tree(tmp, ["b/x"], ["b"], "stop reading a")
            self.assertTrue(fp.affected_between(tmp, base, head))

    def test_a_mode_change_is_a_change(self):
        with tempfile.TemporaryDirectory() as tmp:
            make_repo(tmp, {})
            base = commit_tree(tmp, ["a/x"], ["a"], "base")
            run(tmp, "git", "update-index", "--chmod=+x", "a/x")
            run(tmp, "git", "commit", "-q", "-m", "chmod")
            head = run(tmp, "git", "rev-parse", "HEAD").strip()
            self.assertTrue(fp.affected_between(tmp, base, head))

    def test_a_rename_out_of_the_footprint_is_a_change(self):
        with tempfile.TemporaryDirectory() as tmp:
            make_repo(tmp, {})
            base = commit_tree(tmp, ["a/x"], ["a"], "base")
            run(tmp, "git", "mv", "a/x", "b")
            run(tmp, "git", "commit", "-q", "-m", "rename")
            head = run(tmp, "git", "rev-parse", "HEAD").strip()
            self.assertTrue(fp.affected_between(tmp, base, head))

    def test_merge_compares_against_the_first_parent(self):
        with tempfile.TemporaryDirectory() as tmp:
            make_repo(tmp, {})
            commit_tree(tmp, ["a/x", "b/x"], ["a"], "base")
            run(tmp, "git", "checkout", "-q", "-b", "side")
            write_files(tmp, {"b/x": "changed\n"})
            run(tmp, "git", "commit", "-qam", "outside the footprint")
            run(tmp, "git", "checkout", "-q", "-")
            write_files(tmp, {"a/x": "changed on main\n"})
            run(tmp, "git", "commit", "-qam", "inside the footprint, on main")
            run(tmp, "git", "merge", "-q", "--no-ff", "--no-edit", "side")
            self.assertFalse(fp.affected_by_merge(tmp, "HEAD"))
            with self.assertRaises(fp.FootprintError):
                fp.affected_by_merge(tmp, "HEAD^1")


if __name__ == "__main__":
    unittest.main()

# The current directory that has no path

Issue #956's filesystem line left one thing deliberately unfinished, recorded as
"out of scope" in `docs/plans/2026-08-29-systemnative-chdir.md`:
`UnixProcessState.CurrentDirectory` keeps a path that no longer reaches
`CurrentDirectoryInode` once the process's directory has been removed out from
under it. This plan is that slice.

## First, the premise in the chdir plan is wrong

That plan says:

> PawPrint answers the stale cached path where a real `getcwd` fails ENOENT —
> measured above.

**It does not.** `UnixSystem.getcwd` has guarded the case since #1196:

```fsharp
elif VirtualFileSystem.isOrphanedDirectory system.Process.CurrentDirectoryInode system.Machine.FileSystem then
    match SimulatedUnixPlatform.getCwdOrphanAnswer system.Machine.UnixPlatform with
    | GetCwdOrphanAnswer.AlwaysDetached -> Ok (GetCwdAnswer.Failed UnixError.ENOENT)
    ...
```

and `sourcesImpure/RmDirOrphanLinuxSeeded.cs` asserts it end-to-end
(`GetCwdError(256) != PAL_ENOENT` fails the guest), registered with
`ExpectedReturnCode = 0`. That guest is active and green, so the guest-visible
behaviour is already right, on both flavours, including the buffer-size rows.

The same false claim is repeated in the comment on
`chdir into a removed current directory succeeds and moves no path`
(`TestUnixSystemStep.fs`). Both are wrong about the code as it stands and should
be corrected on #1253 rather than carried into this slice.

**Consequence for scoping: this is not a bug fix, it is the removal of a
representational lie.** There is no input for which PawPrint currently gives a
guest a wrong answer — see "Why nothing observes it today" below. That lowers
the urgency and it changes what the tests can be: there is no failing guest to
write first, so the work has to be justified and tested at the invariant tier.

## What is actually wrong

**A. The field's contract is false while the directory is detached.**
`UnixProcessState.CurrentDirectory` is documented as the physical path that
reaches `CurrentDirectoryInode`, "so the two cannot describe a process no Unix
could produce". Once the directory is orphaned there is no such path, and all
three sites that maintain the field keep the previous value instead:

| site | what it does when there is no path |
| --- | --- |
| `EmulatedKernel.withFileSystemAndCurrentDirectory` | cannot arise — the host names a live directory |
| `UnixSystem.rename` (`UnixSystem.fs:6642`) | `\| None -> system.Process.CurrentDirectory` |
| `UnixSystem.chdir` (`UnixSystem.fs:6351`) | `\| None -> system.Process.CurrentDirectory` |

**B. Nothing checks the field during exactly the window in which it is wrong.**
`checkInvariants` raises `CurrentDirectoryPathDisagrees` only while
`pathOfDirectory` returns `Some`; the `None` arm is grouped with the success arm
and returns no defect. So the one period when the field is knowingly untrue is
the one period when the checker is silent.

**C. `getcwd`'s guard and the field's staleness are two different predicates,
and their agreement is unchecked.** `getcwd` asks `isOrphanedDirectory cwd`
(is *this inode's* name count zero); the field goes stale exactly when
`pathOfDirectory cwd` is `None` (does *the whole parent chain* reach the root).
Those are not the same question. They coincide only because no operation can
orphan a non-empty directory — `rmdir` refuses one, and `rename` refuses a
populated destination — so an orphan is always childless and therefore never an
ancestor of anything. That argument spans `VirtualFileSystem`,
`SimulatedUnixPlatform` and `UnixSystem`, and nothing asserts it.

### Why nothing observes it today

The field can only go stale at a moment when `pathOfDirectory cwd = None`, which
(by C) means the cwd inode is orphaned. An orphaned directory can never be
re-linked: `link(2)` refuses directories, and `rename` needs a source *path*,
which by definition it has not got. So the inode stays orphaned for as long as
the process stands in it, and `getcwd`'s guard keeps firing. The only way out is
a `chdir`, which recomputes the field. Hence: stale ⇒ masked, for every reachable
state.

Measured on Darwin (`scratchpad`, reproduced below as a probe row to add), the
escape really is the recovery point:

```
in /d/sub                     getcwd -> .../d/sub
after rmdir of cwd            getcwd -> errno 2 (ENOENT)
after chdir("..") to live /d  getcwd -> .../d
```

PawPrint already answers all three correctly. Nothing tests the third.

## Options

### Option 1 — make the field optional

`CurrentDirectory : AbsoluteUnixPath option`, `None` meaning detached.

* `getcwd` matches on the field instead of calling `isOrphanedDirectory`, which
  collapses defect C: the predicate that decides the answer becomes the same
  value that holds the answer.
* `checkInvariants` gains a real rule for the `None` case (`None` iff
  `pathOfDirectory` is `None`), closing B.
* The denormalisation survives: two fields still describe one fact, three sites
  still have to maintain it, and `CurrentDirectoryPathDisagrees` still has to
  exist to catch them drifting.
* `KernelConfig.CurrentDirectory` stays non-optional — a host always names a
  live directory — so no host is affected.

### Option 2 — delete the field and derive the path (recommended)

Remove `CurrentDirectory` from `UnixProcessState` entirely. The path becomes a
function of the state that is already there:

```fsharp
let currentDirectoryPath (system : UnixSystem<_,_>) : AbsoluteUnixPath option =
    VirtualFileSystem.pathOfDirectory system.Process.CurrentDirectoryInode system.Machine.FileSystem
```

* **A, B and C all stop being representable rather than being fixed.** There is
  no second copy to go stale, so `CurrentDirectoryPathDisagrees` is deleted, the
  unchecked window vanishes, and `getcwd`'s guard *becomes* the derivation
  (`None` is the detached answer) rather than a separate predicate that has to
  agree with it.
* `rename`'s recompute and `chdir`'s `recorded` both disappear — about 25 lines
  of comment-heavy maintenance code, each of which is currently a rule someone
  must remember to obey.
* The "report the physical path, not the guest's spelling" fact (`chdir("ld")`
  with `ld -> d` reports `/d`) stops being something `chdir` must remember and
  becomes a consequence of deriving from the inode the walk landed on. It is
  still tested; it just cannot be got wrong.
* This is the identity/projection split AGENTS.md asks for: the inode is
  identity, the path is a view, and a view computed on demand cannot disagree
  with its source.
* **Cost — a breaking change to a published package.** `UnixProcessState` is
  public in `WoofWare.PosixKernel`; PawPrint is its only consumer today.
  `EmulatedKernel.CurrentDirectory` (a member with no callers in the library —
  only tests) becomes a computed member returning an option.
* **Cost — a climb per `getcwd`.** `pathOfDirectory` walks parents to the root
  with a `Set` for cycle-bounding. `Path.GetFullPath` on a relative path reaches
  `getcwd`, so this is not a once-per-process call. I have not measured it and
  will not guess: if we take this option, benchmark `getcwd` before and after on
  a deep tree, and only if it registers against the cost of interpreting the PAL
  shim's own IL does a cache become worth reintroducing.
* Reversible: re-adding a cached field later is mechanical, and would come back
  with the invariant rule Option 1 describes.

### Considered and rejected

* **A DU keeping the last known path** (`Reachable of path | Detached of
  lastKnown`). Makes the state explicit but keeps the denormalisation *and*
  hands every reader a plausible-looking wrong answer to reach for. Strictly
  worse than Option 1.
* **Change nothing; just check and document.** Add the equivalence rule to
  `checkInvariants`, fix the two false comments, stop. This is the honest
  minimum, and it is worth doing under either option above — but it leaves a
  field whose docstring has to explain when it is allowed to be untrue.

## Recommendation

Option 2. The bug is that one fact is stored twice; deriving it removes the
class, not the instance. The two costs are a package-API break with one known
consumer, and a performance question that is measurable before committing.

## Measured: the derivation costs nothing that matters

Option 2's one real risk was a parent-climb on every `getcwd`, which
`Path.GetFullPath` reaches, so it is not a once-per-process call.

`VirtualFileSystem.pathOfDirectory`, Release build, arm64:

| cwd depth | path length | per call |
| --- | --- | --- |
| 1 | 3 | 0.78 µs |
| 4 | 12 | 3.4 µs |
| 16 | 55 | 10 µs |
| 64 | 247 | 26 µs |
| 256 | 1172 | 115 µs |

Against that, one interpreted `Environment.CurrentDirectory` under PawPrint,
measured by sweeping a guest's loop count (n = 1000 and n = 5000, twice each;
means 5.13 s and 10.14 s, so a slope of **1.25 ms per call**).

So at the depth guests actually run at, the derivation is **0.06%** of the call
it serves; even a 256-deep current directory would be under a tenth of it. No
cache, and the field stays gone.

### The sweep that mattered was width, not depth

Depth was the parameter I thought to sweep, and it was the wrong one. Codex's
review found the other: `pathOfDirectory` recovers each component's name by
searching its *parent's* entries, so the cost scaled with how many **siblings**
a directory has. That copied the parent's whole entry map per level.

Measured over one directory holding 100,000 entries, three ways of doing the
same reverse lookup:

| lookup | per call | allocated |
| --- | --- | --- |
| `Map.toList \|> List.tryPick` (what was there) | 2.3 ms | 6 400 024 B |
| `Map.tryFindKey` | 4.2 ms | 6 800 175 B |
| `Map.tryPick` | 0.39 ms | 24 B |

`Map.tryFindKey` reads like the obvious answer and is *worse than the original
on both counts* — which is why this is a measurement and not an argument.
`Map.tryPick` walks the tree instead of copying it, and is what the code now
does. End to end on `pathOfDirectory`:

| siblings | before | after |
| --- | --- | --- |
| 0 | 0.9 µs / 968 B | 0.8 µs / 904 B |
| 100 | 4.7 µs / 8 088 B | 1.9 µs / 1 624 B |
| 10 000 | 147 µs / 642 529 B | 35 µs / 2 488 B |
| 100 000 | 1 179 µs / 6 403 128 B | 556 µs / 3 064 B |

Allocation is now flat in width. The remaining time still is not: a reverse
lookup in a forward map is inherently O(width), and removing that would mean
storing each directory's own name beside its parent link — a second copy of a
fact the graph already holds, which is the thing this slice exists to delete.
The measurement above says that trade is not worth making: it buys nothing at
the widths a guest has, and `getcwd` costs 1.25 ms of interpretation regardless.

This is a strict improvement for `pathOfDirectory`'s other callers too — the
test oracle and kernel construction were paying the same copy.

## What was done

Option 2, in the order the plan set out.

1. Corrected the two false comments (the chdir plan's out-of-scope note and the
   test's).
2. Added `a directory has no path exactly when it is orphaned` to
   `TestVirtualFileSystem.fs` — the equivalence Option 2 rests on for being
   behaviour-preserving, over a corpus with orphans and a rename that moves
   directories between parents. Coverage guard measured over eight runs (64, 64,
   65, 65, 67, 67, 71, 75 orphans seen); mutation-checked by removing
   `isOrphanedDirectory`'s root special case.
3. Extended `docs/probes/chdir/chdir.py` with the recovery rows and re-measured
   both flavours. They agree: `chdir("..")` out of a removed directory recovers
   `getcwd`, and `chdir(".")` in one leaves it failing ENOENT both before and
   after.
4. Removed `UnixProcessState.CurrentDirectory`. `getcwd` derives via the new
   `UnixSystem.currentDirectoryPath`; `rename`'s recompute and `chdir`'s
   `recorded` are gone; `UnixSystemDefect.CurrentDirectoryPathDisagrees` is
   deleted along with the test that constructed the now-unrepresentable state
   (`CurrentDirectoryIsNotADirectory`, which survives, keeps its two tests).
   `EmulatedKernel.currentDirectoryPath` replaces the forwarding member.
5. Benchmarked — see above. No cache.
6. Mutation battery, five mutations, all killed, each by the test that names its
   subject:

| mutation | killed by |
| --- | --- |
| detached falls back to the root | `a removed current directory is ENOENT at every capacity that reaches it` (and, checked separately, both `RmDirOrphan*Seeded` guests) |
| the derivation reads the root, not where the process stands | `a kernel-copying getcwd answers ERANGE before it looks at the destination` |
| the zero-capacity guard never fires | `getcwd answers a zero capacity EINVAL, and that beats a removed directory` |
| chdir does not move the process | `chdir in and out of a removed current directory` |
| the shortest-path flavour needs no room for the root | `below capacity 2 even a user-space getcwd answers an unwritable destination` |

## Out of scope

* `fchdir(2)`: still no CoreLib caller.
* Caching the derived path. The benchmark says no.

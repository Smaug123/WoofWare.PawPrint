# `rename(2)`, measured

`rename.py` is the probe behind `RenameRules` in
`WoofWare.PawPrint/EmulatedKernel.fs` and the rows in
`WoofWare.PawPrint.Test/TestRenameRules.fs`. Re-run it rather than re-deriving
those tables; a row that disagrees with them is a measurement, not a bug report
about the probe.

```
python3 rename.py                                             # this machine
container run --rm -v "$PWD:/probe" --user 1000:1000 \
    python:3-slim python3 /probe/rename.py                    # Linux, uid 1000
container run --rm -v "$PWD:/probe" python:3-slim python3 /probe/rename.py
```

The captured columns beside it are:

| file | envelope |
| --- | --- |
| `measured-darwin-uid501.txt` | macOS 26.6 / APFS, uid 501 |
| `measured-linux-uid1000.txt` | Linux 6.18 arm64 / ext4, uid 1000 |
| `measured-linux-uid0.txt` | the same kernel at uid 0 |
| `measured-darwin-uid0.txt` | macOS 26.6 / APFS at uid 0 |

Fifty-four of the 212 rows diverge between the two flavours, which is why
`rename` has two whole verdict functions rather than a record of flags.

## Reading the columns

Run both privileged and unprivileged. Privilege does exactly one thing — it
stops the permission arms firing — and nothing reorders, so neither column is
derivable from the other.

Be careful how far that is taken. At uid 0 the two flavours agree on every row
that diverges *because of a permission check* — but they still disagree on every
structural one, which privilege never touches: `rename("d/.", x)` is EINVAL on
Darwin and EBUSY on Linux at uid 0 exactly as at uid 1000. An earlier note in
this workstream claimed the flavours agree on *every* row at uid 0; the
`measured-darwin-uid0.txt` column falsifies it.

Build the trees on a real filesystem. Inside a container, `/tmp` on the
container's own block device is ext4, while a bind-mounted host directory is
virtiofs and answers differently — `stat -f -c %T /tmp` before believing a
Linux column.

## The mount root: three readings, two of them wrong

The question was whether Darwin gives the filesystem root its own arm for
`rename`, as its `unlink` and `rmdir` both do. The answer is **no** — but
getting there took three attempts, and each wrong answer looked convincing
enough to be written into an implementation.

The obstacle is that a filesystem root which is not `/` is a *mount* root, and
renaming one is liable to EXDEV, which masks the rule. Two earlier readings of
this section were wrong in opposite directions: the first probed `base/..`,
which resolves to the directory *containing* the mount — a different inode on a
different filesystem — and the second moved the operands into a private
subdirectory, which flipped every answer.

The `mountroot` section is now a 2×2 because that is what the discriminator
turned out to be, measured 40 trials per row and stable: not `.` against `..`,
but whether the source's parent directory and the destination's parent
directory are the same object.

| source | parents | result |
| --- | --- | --- |
| `base/.` | differ | **EINVAL** |
| `base/.` | same | EXDEV |
| `base/<private>/..` | same | EXDEV |
| `base/<private>/..` | differ | **EINVAL** |

So EXDEV is the mount boundary talking, and wherever it stays quiet the root
answers **EINVAL for both navigations** — exactly what an ordinary directory
answers, which the control row confirms. PawPrint has one filesystem and no
mounts, so nothing can make EXDEV speak and the EINVAL readings are the
applicable ones. The root is not a special case, and `darwinVerdict` has no arm
for it.

The mount-root section is the one part of this probe that writes outside a
private temp directory, because its rows have to name the mount root itself and
that cannot be relocated. Everything it creates lives in one `mkdtemp`
directory made directly under the mount — created atomically and uniquely, so
there is no collision to lose a race against and nothing to clean up but that
directory. Two rows need a destination whose *parent* is the mount root, so they
name one sibling of it; that name derives from `mkdtemp`'s, is collision-checked
before use, and at cleanup is removed only if the object there is one this run
created, verified by `(st_dev, st_ino)` and removed with `rmdir` rather than
recursively. Anything else at that name is reported and left alone.

## Rows the probe declines to run

**Anything resolving to `/` while running as root.** Those rows are refusals on
both kernels and create nothing, but a probe run under `sudo` must not be the
thing that discovers otherwise on somebody's real root filesystem, so it skips
them and says so. The unprivileged runs pin them, and privilege does not
participate in either kernel's structural checks.

The guard resolves each operand rather than string-matching `/`: `.`, `..` and a
symlink whose target is `/` all reach it, and an earlier version caught only the
literal spelling — so eleven rows, including three through a `lroot -> "/"`
symlink, really did name the root under `sudo`.

## One row is nondeterministic on macOS

`rename("l/", "g")` where `l` is a symlink to a **regular file** with an
**absolute** target is a race on macOS 26.6/APFS: usually ENOTDIR, but roughly
1–20% of the time it succeeds and **moves the link's target**, leaving `l`
dangling. The rate scales with how many components the absolute target has to
traverse — ~1% under `/private/tmp`, ~10–20% under a `/private/var/folders`
temp directory. With a **relative** target it is ENOTDIR every time, 600/600
across both locations, and on Linux it is stable either way.

It is also **uid-dependent**: at uid 0 the same row is ENOTDIR 200/200, where at
uid 501 it is ~178/22. At an ~11% success rate, 0 successes in 200 has
probability ~5e-11, so privilege genuinely suppresses the race rather than the
root column having been lucky. Not modelled — PawPrint answers ENOTDIR whatever
the uid, which is what root sees anyway — but recorded, because a reader
comparing the two columns will notice the discrepancy and should not have to
re-derive it.

The `unstable` section samples it 200 times per style on every run, so the fact
stays measured rather than decaying into a comment. The corpus rows use relative
targets deliberately, which is what makes the `trail` section reproducible.

PawPrint answers ENOTDIR: the stable result, the overwhelming majority of the
unstable one, and the only one of the two that destroys nothing. **That is a
choice**, recorded so nobody later reads a single ENOTDIR sample as evidence
there was nothing to choose. This row must never enter the host-equality tier.

## What each section is for

| section | the question |
| --- | --- |
| `type` | every source kind against every destination kind. Unanimous, and it collapses to: the only thing that matters about either object is whether it is a directory |
| `struct` | subtrees, prefixes, self-renames, hard links, absent names, over-long names |
| `nav` | `.`, `..` and `/` in each position. Linux spends one errno on all of them; Darwin spends three and splits the `/` destination by the *source*'s kind |
| `perm` | which directory's write bit each kernel demands, one bit removed at a time |
| `order` | pairs where two checks both refuse, so the errno says which was asked first |
| `displaced` | the strangest fact here: a directory replacing a directory consults the **displaced directory's** own write bit on Darwin and its **holder's** on Linux |
| `trail` | the trailing-separator walk, which is where the two kernels destroy different objects |
| `orphan` | a destination whose parent has lost its last name, reachable only as an `rmdir`'d current directory |
| `unstable` | the one row macOS does not answer the same way twice; see above |
| `walk` | which of the two paths is resolved first, and how far. Not yet implemented — this is `RenameRules.WalkOrder`'s slice |

`walk`'s Linux answers are only consistent with four phases — resolve the
source's parent, the destination's parent, then look the source up, then the
destination — which is `do_renameat2`'s shape. Darwin's are consistent with
resolving the source completely and then the destination. That divergence is
deliberately not modelled yet: it needs `VirtualFileSystem.resolveFull` split
into a parent walk and a final lookup, which every other syscall shares.

## `walk-order.py` — the order the two paths are settled in

`rename.py` above measures what a rename owes once both paths have resolved.
`walk-order.py` measures the order the resolving happens in, which is a
separate divergence: it is invisible to any row where the two paths earn the
same errno, so every row it asks is built from a pair that *disagrees*.

| file | envelope |
| --- | --- |
| `measured-walk-order-darwin.txt` | macOS 25.6 / APFS, uid 501 |
| `measured-walk-order-linux.txt` | Linux 6.18 arm64 / ext4, uid 1000 |

What the columns say, and what each is load-bearing for:

* **Both pathnames are copied in before either is walked, on Linux only.** With
  a source that does not exist, an over-`PATH_MAX` destination is ENAMETOOLONG
  on Linux and ENOENT on Darwin. The *mid-length* row is what makes this
  conclusive rather than suggestive: 2000 bytes is over Darwin's `PATH_MAX` of
  1024 and under Linux's 4096, and Darwin answers ENOENT for it too — so Darwin
  is not copying the destination in early and finding it short enough, it is
  not copying it in at all yet. Each platform probed at its own scale.
* **EFAULT and ENAMETOOLONG surface together**, so one "the argument was copied
  in" step models both. That is `getname()`, and it is why `PathArgument.Failed`
  carries either.
* **Everything about the source's final component loses to the destination's
  parent, on Linux.** A free name, "/", ".", "..", a trailing separator and a
  300-byte name all answer the destination's ENOTDIR. The 300-byte row is the
  one that pins where `resolveParent` stops: the final component's length is
  measured in the *finals* phase, so a walk that checked it while resolving the
  parent would answer ENAMETOOLONG here.
* **The source's parent walk does beat the destination**, on both:
  `rename("nodir/kid", "f/x")` is ENOENT and `rename("nosearch/kid", "f/x")` is
  EACCES, where the destination alone would answer ENOTDIR.
* **Darwin settles exactly two source-side refusals early**: a free final name
  (ENOENT) and the filesystem root named as the whole path (EISDIR). Reaching
  the root *by navigation* is not the same thing — "/.", "/..", "/dev/..", ".",
  ".." and "dir/.." all answer the destination's ENOTDIR — so what is early is
  `FinalNavigation.Root`, the one case that consumed no component at all. This
  is `RenameRules.sourceScreen`.

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

## Two rows that are unmeasurable, not merely unmeasured

**A rename whose *source* is the filesystem root reached by a final `.`, on
Darwin — and only that.** An earlier version of this note said "`.` or `..`",
and it was wrong for a measurable reason: it had been probing `base/..`, which
resolves to the directory *containing* the mount, on the other filesystem. The
root reached by a final `..` is `base/<child>/..`, and on a fresh APFS image
where both operands share a device it answers **EINVAL**, like any other
directory. So `..` needs no special case at all.

A final `.` really is out of reach. For such a path the source's parent
directory is the mount's parent, on the containing filesystem, so EXDEV
pre-empts the rule — measured on `base/.`, `base/./.` and `base/kid/../.`
alike, and at uid 0 as well, so privilege is no way round it. The same
navigation as a *destination* is fine (EINVAL), because only the source side
crosses. Pass a mount point as `argv[1]` to run that section.

The mount-root section is the one part of this probe that writes outside a
private temp directory, because those rows have to name the mount root itself
and it cannot be relocated. It therefore creates only uniquely-named entries,
**refuses to run** if any of them already exists rather than clearing the way,
and removes only what it made. Set `RENAME_PROBE_TAG` to a fixed string to
exercise that refusal.

PawPrint models one filesystem and therefore never answers EXDEV, so nothing
can stand in for the row — and guessing EINVAL would be a guess against
evidence, since Darwin's `unlink` and `rmdir` both give the root its own EBUSY
arm where an ordinary directory gets EPERM or EINVAL. `RenameRules.darwinVerdict`
crashes there, naming the condition.

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

# `chdir(2)`, measured

`chdir.py` is the probe behind `UnixSystem.chdir` and its rows in
`TestUnixSystemStep`. Re-run it rather than re-deriving those; a row that
disagrees is a measurement, not a bug report about the probe.

```
python3 chdir.py                                              # this machine
container run --rm -v "$PWD:/probe" --user 1000:1000 \
    python:3-slim python3 /probe/chdir.py                     # Linux, uid 1000
```

| file | envelope |
| --- | --- |
| `measured-darwin.txt` | macOS 25.6 / APFS, uid 501 |
| `measured-linux.txt` | Linux 6.18 arm64 / ext4, uid 1000 |

**The two columns are identical on every row.** That is the headline: `chdir`
is the first filesystem syscall in this workstream with no flavour divergence
at all, so it needs no `ChDirRules` and no per-flavour verdict. The rows cover
the whole arm space — object type, final-symlink following, trailing
separator, which permission bit, name length, navigation, and the current
directory removed underneath the process — so this is a measured absence
rather than an unexamined one.

What the columns say:

* **`chdir` follows a final symlink, and `getcwd` afterwards reports the
  *physical* path.** `chdir("ld")` with `ld -> d` succeeds and `getcwd` says
  `<root>/d`, not `<root>/ld`. So the cached current-directory path is
  `VirtualFileSystem.pathOfDirectory` of the inode landed on, never the path
  the guest passed.
* **It wants the *search* bit, not the read bit.** A 0o100 directory is fine
  and a 0o400 one is EACCES. That is the opposite way round from `opendir`,
  which demands read — the two came apart there first, and this is the second
  place.
* **A trailing separator costs nothing.** `d/` behaves as `d`, and `f/` earns
  the same ENOTDIR that `f` does. No `TrailingSeparatorPolicy` question.
* A dangling symlink is ENOENT and a symlink to a file is ENOTDIR, which is
  what following the final link and then demanding a directory produces.
* The empty path is ENOENT; a 300-byte component is ENAMETOOLONG.
* **`chdir(".")` in an `rmdir`'d current directory succeeds**, while `getcwd`
  in that state fails ENOENT. The two facts together are why the cached path
  cannot simply be recomputed on every `chdir`.

The `..` row escapes the tree on the host, where the model's root is its own
parent. It is recorded for completeness and is not comparable between the two
worlds; `VirtualFileSystem`'s own tests cover `..` at the root.

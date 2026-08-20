# Testing a kernel fact: choosing a tier

Three tiers can observe a kernel fact, and each one is blind to something the
others see. Picking wrong wastes a cycle.

## Host-equality tests

`TestVirtualFileSystemAgainstHost`, `TestPlatformSocketSupport`. For any fact
measurable on the machine running the test, this is the strongest form — and the
assertion should be **equality against the host**, not a bracketing check:

```fsharp
let modelled = SimulatedUnixPlatform.pathLimits (hostPlatform ())
if measured <> modelled then failwith $"...; %d{measured} is the measured answer."
```

The dev machine is macOS and CI is entirely `ubuntu-latest`, so each flavour's
column is checked on the machine that can actually falsify it, and neither rests
on anybody's say-so. **Make the failure message report the measured value**, so
one CI run corrects a wrong constant rather than merely rejecting it. This is
also the right home for *divergent* rows, not only unanimous ones.

Three details that are easy to get wrong:

- Derive `hostPlatform` as a **function**, not a module-level value: F# module
  values initialise on first touch of the file, so a `failwith` for "not a Unix"
  fires before the test's own `Assert.Ignore`.
- **The test must not construct the expected value itself.** Obtain it from the
  platform, or the test pins behaviour under a constant no real kernel has.
- **Oracle the PAL function, not a transcribed struct.** A `DllImport` of
  `SystemNative_GetFileSystemType` oracles the exact function being emulated —
  including Darwin's name mapping and its failure-to-0 folding — with no
  platform-forked layout to get wrong.

The oracle covers only the host's column. Swapping pipefs and sockfs survives it
on macOS, because Darwin answers `EINVAL` for both; the Linux guest is what kills
that mutant. Per-flavour guests and the host oracle are complementary across
environments, not redundant.

`TestVirtualFileSystemAgainstHost` cannot express `FinalNavigation.Root`: it
prefixes every path with a temp directory, so `/` arrives as a named component.

## `sourcesPure`

Differential against real .NET running on the *host*, so a row may only assert
what holds on **both** flavours — and specifically, what holds in the *observable
asserted*. `ELOOP` agrees as an errno but not as a number (40 against 62).

Two walls worth knowing before writing a row:

- **No `EEXIST` row is reachable.** The BCL's `EEXIST` arm goes through
  `GetIOException`, which needs `SystemNative_ConvertErrorPalToPlatform` and
  `StrErrorR`, so the guest aborts while *constructing* the exception. The same
  wall stops `OpenMissingFile.cs`'s `EACCES` row.
- **A seeded guest sees two different filesystems.** PawPrint puts the seed at
  `/` with cwd `/`; `RealRuntime` materialises it into a scratch directory that
  is the oracle's cwd. Relative names agree, absolute ones do not — and an
  absolute path is not merely different but *meaningful on both sides*. The trap
  is not a literal `/foo` in the guest; it is a BCL API that manufactures an
  absolute path from a relative one. `File.ResolveLinkTarget(path, …)` does not
  `GetFullPath` its argument where `FileSystemInfo.ResolveLinkTarget` passes
  `FullPath`, so `File.ResolveLinkTarget("lf", false).FullName` is `"/f"`.
  Prefer the instance overloads, compare against `Path.GetFullPath(relative)`
  rather than a literal, and before asserting on a `FullName` ask whether the BCL
  could have rooted it at `/`.

## `sourcesImpure`

PawPrint only, so its expected values are a claim rather than an oracle's answer
— obtain them by running the guest on the real platform where that is possible.

It is the only tier that sees the **wiring**. Unit tests pass rules in by hand
(`CreatingOpenRules` and friends), so a handler that hardcodes `Kernel.Umask` or
`Kernel.UnixPlatform` survives every one of them. Set both away from their
defaults in the registration.

It is also the only home for a row that is PawPrint-specific by construction —
an interpreter limit that is arithmetic in a modelled constant, for instance,
since a real 64-bit libc succeeds at every count by overcommit.

## The set-ID caveat that looks like a flake

CI runs the suite twice: the `build` job is `nix develop --command dotnet test`
(unsandboxed, real Linux semantics, set-ID bits work) and `build-nix` is
`nix build` (sandboxed). Nix's Linux sandbox installs a seccomp filter returning
`EPERM` for `chmod`/`fchmod`/`fchmodat`/`fchmodat2` whenever the mode sets
`S_ISUID` or `S_ISGID` — unconditional, not row-dependent, and
`File.SetUnixFileMode` therefore *throws* `UnauthorizedAccessException`, which a
"did the chmod stick?" read-back guard does not catch. `listxattr` and friends
return `ENOTSUP` there too.

So a host-touching test needing set-ID bits must catch the refusal and skip, not
merely read the mode back. Skipping costs nothing: `build` still measures those
rows on every push. `RealRuntime.validateSeedForOracle` already refuses
set-user-ID, set-group-ID and sticky modes in a `sourcesPure` seed, so the
differential harness is safe by construction.

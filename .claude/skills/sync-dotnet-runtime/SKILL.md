---
name: sync-dotnet-runtime
description: How to bump the pinned dotnet/runtime source in flake.nix, and how to add a tree to its sparse checkout. Use when $DOTNET_RUNTIME_SRC_NET10 (or _NET11) lacks a tree you need to read, or when a runtime-version-pin check or TestEmulatedRuntime reports drift.
---

# Sync the pinned dotnet/runtime source to our Nix devshell

`flake.nix` holds one entry of `runtimes` per runtime PawPrint emulates (`net10`; `net11`, which the devshell carries as a framework for guests only). Each pins nixpkgs's runtime package, the version we emulate (`expectedRuntimeVersion`), the dotnet/runtime source at that version (`sourceRev`, `sourceHash`; a sparse `fetchgit` made by `runtimeSource`), and its linux-x64 runtime pack (`linuxFrameworkTfm`, `linuxFrameworkHash`). The source is exposed in the devshell as `$DOTNET_RUNTIME_SRC_NET10` and `$DOTNET_RUNTIME_SRC_NET11`. We use it as a read-only reference for upstream behaviour (BCL source, QCall stubs, native runtime helpers). Each source pin must match the runtime the devshell runs for it, so the source you read matches the assemblies the interpreter actually executes. The steps below bump one runtime; the other's pins are untouched.

Bumping is an edit to `flake.nix` and `WoofWare.PawPrint/EmulatedRuntime.fs` — there is no sibling checkout to manage any more. Two guards keep the pin honest, and both run in CI:

- the `runtime-version-pin-<runtime>` flake checks fail when a runtime's `expectedRuntimeVersion` in `flake.nix` drifts from the version nixpkgs provides;
- the `TestEmulatedRuntime` drift tests fail when the CoreLib the test suite loads (the runtime under test's, and its pinned linux-x64 pack's inside the devshell) is not the build `EmulatedRuntime.pin` records for its major. The pinned build is the CoreLib's `AssemblyInformationalVersion` up to its `+`, e.g. `10.0.7-servicing.26217.108`, compared as an exact string.

## Steps

1. Find the runtime version the devshell pins:
   ```bash
   nix develop -c dotnet --info
   ```
   The `Host:` block reports the runtime `Version:` (e.g. `10.0.7`). It also reports a `Commit:` — **do not pin to that commit.** It is the binary's *internal build commit* and is frequently never pushed to the public `dotnet/runtime` repo (`gh api repos/dotnet/runtime/commits/<sha>` returns 422), so `fetchgit` cannot fetch it.

2. Resolve the **public** source commit for that version — the commit the `vX.Y.Z` tag points to:
   ```bash
   git ls-remote --tags https://github.com/dotnet/runtime 'v10.0.7'
   ```
   If the exact `vX.Y.Z` tag isn't published yet (Microsoft cuts servicing releases internally before the public push), fall back to the closest published tag `≤` the runtime version: list with `git ls-remote --tags https://github.com/dotnet/runtime 'v10.0.*'` and pick the highest `≤` ours. Note: locally-cached tags can be stale; trust `git ls-remote`, not a local clone.

   Don't read the tagged tree's `eng/Versions.props` `<PatchVersion>` as a check on the tag: some release commits are tagged *before* the in-tree version is bumped and self-report `Z-1` (`v10.0.7` -> `PatchVersion 6`, `v10.0.8` -> `7`), while others agree with the tag (`v10.0.12` -> `12`). The tag name is the release identity — pin by it, not by the in-tree version string.

3. Update the pin in `flake.nix`:
   - In that runtime's entry of `runtimes`, set `expectedRuntimeVersion` to the new version (e.g. `"10.0.8"`).
   - Set its `sourceRev` to the public tag commit from step 2, set `sourceHash = pkgs.lib.fakeHash;`, then run `nix develop -c true` and copy the real `got: sha256-…` value back into `sourceHash`. Do the same for `linuxFrameworkHash` and `osxFrameworkHash`, since the packs are fetched at `expectedRuntimeVersion`.
   - If you need a runtime tree outside the current sparse set, add it to `sparseCheckout`, which every runtime's source shares, then re-run the fakeHash → real-hash dance for every runtime's `sourceHash`, since each hash covers its checked-out tree.

4. Update `EmulatedRuntime.pin` in `WoofWare.PawPrint/EmulatedRuntime.fs` for the runtime's major:
   - `Build` must become the new CoreLib's informational-version build identity. It carries a label and build number that `dotnet --info` does not show (a servicing build's is like `10.0.8-servicing.<build>`), so do not guess it: run `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --filter "FullyQualifiedName~TestEmulatedRuntime"`, and the failing drift test prints the exact string to paste.
   - `SourceRef` and `SourceCommit` must name the public tag and its commit, per step 2.

   That runtime's `runtime-version-pin-<runtime>` check and `TestEmulatedRuntime` both stay red until `flake.nix` and `EmulatedRuntime.fs` agree with the devshell runtime. (A runtime without an `EmulatedRuntime` case yet has no `EmulatedRuntime.pin` to update.)

   Then run `TestHardwareInstruction`. Its first test regenerates `WoofWare.PawPrint.Semantics/HardwareIntrinsicTable.tsv` from the JIT's `hwintrinsiclist*.h` at the new pin, and if the JIT's tables changed it fails and prints the `cp` command that installs the regenerated table. Read the diff before copying: a row that changes category or flags changes what the escape analysis says an instruction can raise.

5. Verify the source you care about looks right: e.g. for QCall work against RuntimeHandles, glance at `$DOTNET_RUNTIME_SRC_NET10/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs` and `$DOTNET_RUNTIME_SRC_NET10/src/coreclr/vm/runtimehandles.cpp` to confirm they look like the .NET 10 shape you expect.

6. Re-audit the JIT's `CORINFO_FIELD_INTRINSIC_*` enum at `$DOTNET_RUNTIME_SRC_NET10/src/coreclr/inc/corinfo.h`. As of the audit it has exactly three entries (`ZERO`, `EMPTY_STRING`, `ISLITTLEENDIAN`) and `TestBclIntrinsicStaticFields` in `WoofWare.PawPrint.Test` pins the corresponding BCL field set. If a new entry appears, the JIT is folding `ldsfld` for a slot that PawPrint reads literally: audit the BCL declaration (`grep -nR "\[Intrinsic\]" $DOTNET_RUNTIME_SRC_NET10/src/libraries/System.Private.CoreLib/src/`) for any new static field that lacks an initialiser. Don't assume that "the zero-initialised slot is the right value" makes the case safe — `IntPtr::Zero` and `UIntPtr::Zero` look that way but `cliTypeZeroOf` populates them with `NativeIntSource.ManagedPointer Null`, an isomorphic-but-distinct representation of nint zero that relates to `Verbatim 0L` only because `cgtUn`/`cltUn`/`equalsForCli` carry explicit arms bridging the two (it did not come for free from the zero-initialised slot). Always add a focused guest test (see `IntPtrZero.cs` / `UIntPtrZero.cs` / `BitConverterIsLittleEndian.cs`) before relying on the generic path; lazy population at first `ldsfld`/`ldsflda` (the `System.String::Empty` approach in `UnaryMetadataFieldOps.executeLdsfld`) is the safer default for non-Boolean fields. Then update `expectedIntrinsicStaticFields` in `TestBclIntrinsicStaticFields.fs` to acknowledge the new field.

## Notes

- `$DOTNET_RUNTIME_SRC_NET10` and `_NET11` are sparse: only the trees `sparseCheckout` lists are present. Need something else? Extend `sparseCheckout` in `flake.nix`.
- Don't use `../dotnet` — that's the .NET SDK source, not the runtime. AGENTS.md spells this out.
- If you're iterating against a specific upstream PR that isn't merged, you can temporarily point that runtime's `sourceRev` at that PR's commit (and re-hash). Ask the user before doing so.

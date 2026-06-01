# Sync the pinned dotnet/runtime source to our Nix devshell

The dotnet/runtime source is pinned in `flake.nix` as `dotnet-runtime-src` (a sparse `fetchgit`) and exposed in the devshell as `$DOTNET_RUNTIME_SRC`. We use it as a read-only reference for upstream behaviour (BCL source, QCall stubs, native runtime helpers). The pin must match the .NET 10 runtime the Nix devshell pins, so the source you read matches the assemblies the interpreter actually executes.

Bumping is an edit to `flake.nix` and `WoofWare.PawPrint/EmulatedRuntime.fs` — there is no sibling checkout to manage any more. Two guards keep the pin honest, and both run in CI:

- the `runtime-version-pin` flake check fails when `expectedRuntimeVersion` in `flake.nix` drifts from the version nixpkgs provides;
- the `TestEmulatedRuntime` test fails when `EmulatedRuntime.current.Version` drifts from the runtime the test suite runs on.

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

3. Update the pin in `flake.nix`:
   - Set `expectedRuntimeVersion` to the new version (e.g. `"10.0.8"`).
   - Set `dotnet-runtime-src.rev` to the public tag commit from step 2, set `hash = pkgs.lib.fakeHash;`, then run `nix develop -c true` and copy the real `got: sha256-…` value back into `hash`.
   - If you need a runtime tree outside the current sparse set, add it to `sparseCheckout` (then re-run the fakeHash → real-hash dance, since the hash covers the checked-out tree).

4. Update `WoofWare.PawPrint/EmulatedRuntime.fs` so `Version`, `SourceRef`, and `SourceCommit` match (use the public tag and its commit, per step 2). The `runtime-version-pin` check and `TestEmulatedRuntime` both stay red until `flake.nix` and `EmulatedRuntime.fs` agree with the devshell runtime.

5. Verify the source you care about looks right: e.g. for QCall work against RuntimeHandles, glance at `$DOTNET_RUNTIME_SRC/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs` and `$DOTNET_RUNTIME_SRC/src/coreclr/vm/runtimehandles.cpp` to confirm they look like the .NET 10 shape you expect.

6. Re-audit the JIT's `CORINFO_FIELD_INTRINSIC_*` enum at `$DOTNET_RUNTIME_SRC/src/coreclr/inc/corinfo.h`. As of the audit it has exactly three entries (`ZERO`, `EMPTY_STRING`, `ISLITTLEENDIAN`) and `TestBclIntrinsicStaticFields` in `WoofWare.PawPrint.Test` pins the corresponding BCL field set. If a new entry appears, the JIT is folding `ldsfld` for a slot that PawPrint reads literally: audit the BCL declaration (`grep -nR "\[Intrinsic\]" $DOTNET_RUNTIME_SRC/src/libraries/System.Private.CoreLib/src/`) for any new static field that lacks an initialiser. Don't assume that "the zero-initialised slot is the right value" makes the case safe — `IntPtr::Zero` and `UIntPtr::Zero` look that way but `cliTypeZeroOf` populates them with `NativeIntSource.ManagedPointer Null`, which compares unequal to `Verbatim 0L` in `cgt.un`. Always add a focused guest test (see `IntPtrZero.cs` / `UIntPtrZero.cs` / `BitConverterIsLittleEndian.cs`) before relying on the generic path; lazy population at first `ldsfld`/`ldsflda` (the `System.String::Empty` approach in `UnaryMetadataFieldOps.executeLdsfld`) is the safer default for non-Boolean fields. Then update `expectedIntrinsicStaticFields` in `TestBclIntrinsicStaticFields.fs` to acknowledge the new field.

## Notes

- `$DOTNET_RUNTIME_SRC` is sparse: only `src/coreclr`, `src/libraries/System.Private.CoreLib`, and `eng` are present. Need something else? Extend `sparseCheckout` in `flake.nix`.
- Don't use `../dotnet` — that's the .NET SDK source, not the runtime. AGENTS.md spells this out.
- If you're iterating against a specific upstream PR that isn't merged, you can temporarily point `dotnet-runtime-src.rev` at that PR's commit (and re-hash). Ask the user before doing so.

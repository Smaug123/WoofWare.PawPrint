# Sync the dotnet-runtime checkout to our Nix devshell

The `../dotnet-runtime` checkout is a clone of `dotnet/runtime`. We use it as a read-only reference for upstream behaviour (BCL source, QCall stubs, native runtime helpers). Its working tree should match the .NET 10 runtime that the Nix devshell pins, so source you read matches the assemblies the interpreter actually executes.

## Steps

1. Find the runtime version pinned by the devshell:
   ```bash
   nix develop -c dotnet --info
   ```
   The `Host:` block reports the runtime `Version:` (e.g. `10.0.7`) and its `Commit:` (e.g. `b16286c228`). The full commit hash also lives in `<sdk-store-path>/share/dotnet/shared/Microsoft.NETCore.App/<version>/.version` — read it for the unabbreviated SHA.

2. Fetch the latest tags and the 10.0 servicing branch:
   ```bash
   cd ../dotnet-runtime
   git fetch origin --tags
   git fetch origin 'refs/heads/release/10.0:refs/remotes/origin/release/10.0'
   ```
   `git fetch --tags` may report `! [rejected] v9.0.0-preview.6.24327.7 (would clobber existing tag)` — that's harmless and does not stop the new tags being fetched.

3. Check the commit out:
   - **If `git cat-file -e <full-sha>^{commit}` succeeds**, check out that exact commit:
     ```bash
     git checkout <full-sha>
     ```
   - **Otherwise** the version is an in-flight servicing release whose commit hasn't been pushed to `dotnet/runtime` yet (Microsoft cuts these from internal branches before the public push). Fall back to the closest published tag less than or equal to the runtime version — list candidates with `git tag --list 'v10.0.*' | sort -V` and pick the highest one that's `≤` our runtime version. If even that's missing what you need, use `origin/release/10.0`.

4. Verify the source you care about compiles in your head: e.g. for QCall work, glance at `src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs` and `src/coreclr/vm/runtimehandles.cpp` to confirm they look like the .NET 10 shape you expect.

## Notes

- The repo's working tree often hosts the user's in-flight PR work; before checking out, confirm `git status` is clean and remember the previous branch (the user can `git switch -` to return).
- Don't use `../dotnet` — that's the .NET SDK source, not the runtime. AGENTS.md spells this out.
- If the user is iterating against a specific upstream PR that's not yet merged, they may want a different commit (e.g. a `pr/<num>` ref). Ask before reverting their checkout.

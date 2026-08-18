# WoofWare.PawPrint

WoofWare.PawPrint is an experimental .NET runtime implementation written in F#. It's an IL interpreter designed to be:
- Fully deterministic (supporting time-travel debugging and fuzzing over thread execution order)
- Fully managed (reimplementing P/Invoke methods to avoid native code)
- Fully in-memory except for explicit filesystem operations

This is NOT a high-performance runtime - it's a very slow IL interpreter prioritizing determinism over speed.

If you need to check upstream behaviour, the genuine .NET runtime's source is pinned in `flake.nix` (`dotnet-runtime-src`) and exposed inside the Nix devshell as `$DOTNET_RUNTIME_SRC`. The pin tracks the .NET 10 servicing version the devshell runs (kept honest by the `runtime-version-pin` flake check and the `TestEmulatedRuntime` drift test). To keep the closure small it is sparse-checked-out to the trees we read most; if you need another tree, add it to the `sparseCheckout` list in `flake.nix`. Note that a fixed-output derivation is keyed by its declared `hash`, so Nix will silently reuse the old store path unless you invalidate the hash as well as editing `sparseCheckout`. See the `.claude/commands/sync-dotnet-runtime.md` Claude command for how to bump the pin. (If you see a sibling checkout `../dotnet`, without the `-runtime` suffix, that is the .NET SDK source and is not what you want.)

## CoreLib flavour

There are two distinct kinds of OS divergence, and they are handled in different places. Do not conflate them.

*Facts the guest reads about its platform* (kernel release, processor count, clock) are **data in the emulated kernel**, never a host read — see `SimulatedUnixPlatform` in `EmulatedKernel.fs`, which defaults to `LinuxX64`. A host read would make a replay depend on the machine that produced it, and guests branch on `Environment.OSVersion`, so it would change guest *control flow* between runs.

*Which BCL code path exists at all* is a different thing: CoreLib is `#if`-split per target at its own compile time, so `System.Threading.Lock.ThreadId.InitializeForCurrentThread` calls `GetUInt64OSThreadId` under `TARGET_OSX` and `TryGetUInt32OSThreadId` everywhere else. PawPrint interprets whichever CoreLib its runtime-dir list resolves, and that is normally the *host's* shared framework — so a macOS dev box runs different guest code from CI and production, both of which are Linux.

To exercise the production flavour anywhere, `flake.nix` pins the managed linux-x64 runtime pack (`dotnet-linux-framework`, at `expectedRuntimeVersion`, managed assemblies only — PawPrint never loads native code) and the devshell exposes it as `$DOTNET_LINUX_FRAMEWORK_DIR`. Put that directory at the *head* of the runtime-dir list you pass to `Program.run`: binding is by simple name and takes the first hit, so every framework assembly then resolves from the pack. `TestLinuxCoreLibFlavour.fs` is the worked example, and tests that need it should `Assert.Ignore` when the variable is unset so a non-Nix checkout still passes. Bumping `expectedRuntimeVersion` means bumping that derivation's `hash` in the same commit.

Note the differential-oracle limit: `RealRuntime.executeWithRealRuntime` runs the guest on the *host's* shared framework, so it cannot be the oracle for a foreign flavour. A linux-flavour test compares PawPrint-on-Linux-CoreLib against the host runtime, which is a claim about facts that hold across flavours (an exit code), not a same-image comparison.

Standard `dotnet` toolchain is provided by the Nix devshell. Run `dotnet` commands as `nix develop -c dotnet ...` rather than invoking `dotnet` directly.

After changes, `nix develop -c dotnet fantomas .` to format.

The solution file is `WoofWare.PawPrint.slnx` (slnx format).

### Running the Application
A playground C# file is in CSharpExample/Class1.cs.
This environment is convenient for running WoofWare.PawPrint against a standalone DLL.
Interpolate the appropriate platform/config strings as necessary.

```bash
nix develop -c dotnet publish --self-contained --configuration Release --runtime osx-arm64 CSharpExample/
nix develop -c dotnet run --project WoofWare.PawPrint.App/WoofWare.PawPrint.App.fsproj -- CSharpExample/bin/Release/net9.0/osx-arm64/publish/CSharpExample.dll
```

## Architecture

### Core Components

**WoofWare.PawPrint** (Main Library)
- `AbstractMachine.fs`: Core IL interpreter execution engine, knitting together `UnaryConstIlOp.fs`, `UnaryMetadataIlOp.fs`, `UnaryStringTokenIlOp.fs`, and `NullaryIlOp.fs`
- `IlMachineState.fs`: Manages the complete state of the abstract machine
- `MethodState.fs`: Tracks execution state of individual methods
- `ManagedHeap.fs`: Implements the managed memory model
- `Assembly.fs`: Handles reading and parsing .NET assemblies
- `TypeInfo.fs`, `TypeDefn.fs`, `TypeRef.fs`: Type system implementation
- `IlOp.fs`: IL instruction definitions and munging
- `EvalStack.fs`: Evaluation stack implementation
- `Corelib.fs`: Core library type definitions (String, Array, etc.)
- `Native/` (dispatched by `Native/NativeDispatch.fs`) and `ExternImplementations/`: the boundary for runtime-provided or host-provided behavior; prefer extending this seam over special-casing host effects elsewhere in the interpreter
- `EmulatedKernel.fs`: the simulated process's kernel-visible state (virtual clock, seeded PRNG, fd table, env vars, processor count). Values the real runtime would read from the host belong here as *data*, never as a host read: the library must not call `System.Environment`, `DateTime.Now`, `Guid.NewGuid` or similar, because a replay would then depend on the machine that produced it
- `HostConfig.fs`: everything the host supplies to configure one run — where to find framework assemblies, the `KernelConfig` above, the scheduler seed, guest argv, and the AppContext properties. Distinct from `KernelConfig`: that is what the guest could learn by asking the OS, this is how the host launches the process at all

**WoofWare.PawPrint.Test**
- Uses NUnit as the test framework
- Test cases are defined in `TestPureCases.fs` and `TestImpureCases.fs`
- C# source files in `sources{Pure,Impure}/` are compiled and executed by the runtime as test cases; files in `sourcesPure` are automatically turned into test cases with no further action (see TestPureCases.fs for the mechanism), while `sourcesImpure` tests must be explicitly registered
- The `unimplemented` set of test files that are not yet expected to pass lives in `WoofWare.PawPrint.Test/TestPureCases.fs` (look for `let unimplemented =` near the top of the `TestPureCases` module)
- `TestHarness.fs` provides infrastructure for running test assemblies through the interpreter
- `RealRuntime.fs` is the differential oracle: it runs a guest on real .NET **as its own process** (`dotnet <guest.dll>` for a Roslyn-compiled image, `executeAssemblyInPlace` for a directory of co-compiled assemblies as `CrossAssemblyHarness` builds, or the apphost for an already-published app) and classifies how it terminated. It must stay out of process. In-process, the guest shares every process-global with the test host — including CoreCLR's single latched exit code, which is what a `void` entry point's exit code *is*, so concurrent guests read each other's exit codes — and `Environment.Exit` or `FailFast` in a guest kills the test runner outright. `WoofWare.PawPrint.Performance` keeps a deliberately in-process copy for benchmarking; it is not an oracle and must not become one
- Run all tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`
- Run a filtered subset with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --no-build --filter "Name~TypeRef" --verbosity normal`
- List adapter-discovered tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --list-tests`
- The `dotnet run`-based runner (`dotnet run --project ... -- --filter-test-case Foo --no-spinner`) may produce no visible output in non-interactive shells; prefer `dotnet test` with `--filter "Name~..."` instead
- The test host runs with its GC heap capped at 50% of physical memory (`System.GC.HeapHardLimitPercent` in the test `.fsproj`, asserted by `TestGcHeapHardLimit.fs`). Unbounded, the suite's peak RSS exceeds what a 16 GB CI runner has, and the OOM killer reaps the test host mid-run — which reports only "Test host process crashed" with no stderr. If you see that shape of failure, suspect memory rather than the diff under test

**WoofWare.PawPrint.App**
- Entry point application for running the interpreter

**WoofWare.PawPrint.IlDump**
- Small CLI tool for disassembling IL from .NET assemblies, using the same assembly-reading infrastructure as the interpreter
- Usage: `nix develop -c dotnet run --project WoofWare.PawPrint.IlDump -- <dll-path> [TypeName] [MemberName]`
- Filters are case-insensitive substring matches; an empty filter argument means "no narrowing", so `-- <dll> "" Foo` searches every type for a member named `Foo`
- Default mode dumps each matching type as a `// type` header, one line per field/property/event, then the full IL of each matching method. A type filter that matches a type but no member still prints the type header, so "no such member" is distinguishable from "no such type"
- `--attrs-only` instead dumps custom attribute applications, and emits only those members which carry attributes

### Key Design Patterns

1. **Immutable State**: The interpreter uses immutable F# records for all state, with state transitions returning new state objects
2. **Assembly Loading**: Assemblies are loaded on-demand as types are referenced
3. **Thread Management**: Each thread has its own execution state, managed through the `IlMachineState`
4. **Type Initialization**: Classes are initialized lazily when first accessed, following .NET semantics

### Target Frameworks

- `WoofWare.PawPrint` and `WoofWare.PawPrint.Domain` intentionally target `net8.0` for compatibility with future consumers
- `WoofWare.PawPrint.App`, `WoofWare.PawPrint.Test`, and playground/example executables target `net10.0`
- When diagnosing build/runtime issues, keep the cross-target split in mind; it is deliberate, not drift

### Code style

* Functions should be fully type-annotated, to give the most helpful error messages on type mismatches.
* Generally, prefer to fully-qualify discriminated union cases in `match` statements.
* ALWAYS fully-qualify enum cases when constructing them and matching on them (e.g., `PrimitiveType.Int16` not `Int16`).
* When writing a "TODO" `failwith`, specify in the error message what the condition is that triggers the failure, so that a failing run can easily be traced back to its cause.
* If a field name begins with an underscore (like `_LoadedAssemblies`), do not mutate it directly. Only mutate it via whatever intermediate methods have been defined for that purpose (like `WithLoadedAssembly`).
* Recall that in F#, compilation order matters: new functions must go after their dependencies, and later files can only depend on earlier ones from the `.fsproj`.
* I know LLMs often love the words "load-bearing" and "seam", but those words are very general; please use more specific descriptions instead.
* When writing docstrings, put only what's relevant to a *caller* on the docstring. If a caller doesn't need to know,it, it should be an inline comment instead.
* No backward-looking or one-time review-focused narrative comments. For example, don't describe what the code used to look like (that's why Git exists!), and don't discuss alternative counterfactual implementations.

### Architecture guidelines

* When a lookup fails because a value is not represented in that index, do not broaden the lookup to return a related value. Instead, keep lookup helpers honest: they should return exactly what the index contains, or `None`/an error. Hew tightly to the domain: don't mix concerns, but instead transform canonical data into the right form.
  * For example, preserve the distinction between identity and view/projection. Prefer making walks total rather than adding projection helpers. If a traversal over runtime types fails at a structural/synthetic handle, teach the traversal how to step through the appropriate relationship; do not coerce the handle into a different identity just to reuse metadata code.
* If callers use a classifier, guard, predicate, or DU case to justify a later operation, keep that classifier's contract truthful and load-bearing. Fixes should ensure the classifier/representation is reliable for its callers.

When you find yourself making an architectural decision, please come up with at least two genuinely different options and choose explicitly between them.
"Genuinely different" means structurally different approaches, not adjacent variants of the same idea.
Consider not just correctness on the immediate use case but also blast radius if the choice turns out wrong, reversibility, and how much information each option preserves for downstream consumers.

For non-trivial choices, write the option set down (in a plan doc or in chat) and confirm with the user before touching code.
If you are unsure, stop and ask rather than guess.

Example: bit-twiddling on provenance-tracked pointers in unsafe C# is a recurring instance of this kind of decision.
Options range from synthesising a bit pattern eagerly (smallest implementation cost, largest loss of information), to maintaining an AST of the transformations performed on a logical set of bits (largest cost, most information preserved), to a middle ground that waits until the last moment before materialising bits.
The right call depends on what downstream code does with the result.

### Development Workflow

Use the `/implement-il-instruction` skill when adding support for a new IL opcode.
Use the `/mutation-testing` skill when mutation-testing a fix or a new test, and the `/probe-methodology` skill before writing a claim about runtime behaviour based on a throwaway measurement.

The project uses deterministic builds and treats warnings as errors to maintain code quality.
It strongly prefers to avoid special-casing to get around problems, but instead to implement general correct solutions; cases where this has failed to happen are considered to be tech debt and at some point in the future we'll be cleaning them up.

When managed BCL code fails because it reaches a runtime intrinsic, InternalCall, P/Invoke, or other host-provided primitive, implement the primitive boundary itself rather than mocking or replacing a higher-level managed method that happens to call it.
For example, add a manual implementation of `System.Type.get_IsGenericType` if `Marshal.SizeOf` needs it; do not mock out `Marshal.SizeOf` just to get past that call path.

You will often find that "obvious" end-to-end tests will fail for annoying reasons, like "something deep in the BCL is calling out to unmanaged code" or "we haven't yet implemented some apparently-unrelated IL opcode".
If this happens, the right strategy is to make incremental progress only: don't head down the rabbithole, but instead try writing a test that specifically captures only what you've just improved, without needing extra implementation work.
We really want to keep changes in small, reviewable chunks; leaving tests in the `unimplemented` category is fine if they're not yet passing, because it means we won't forget about them.
If you find you really do need to implement a dependency, please consider whether we can implement the dependency *first*, getting that PR'ed into main before continuing, because that's greatly preferable; either way, stop and ask me what to do, because I never intend you to implement more than one feature at once.

### Git and PR workflow

* Patrick keeps many worktrees under `.claude/worktrees/`. Before implementing an agreed plan item, run `git worktree list` and `git branch --list` and look for a matching name — a prior session may have already committed the work there without ever pushing or opening a PR. `gh pr list` alone will not find it. If you adopt such a branch, `git rebase --onto origin/main <merge-base> <branch>` and work in its worktree (its build is warm). Read its commit message before adopting it wholesale — authorship is Patrick's git identity even when a previous Claude wrote it, so weigh its design choices on the merits rather than assuming "the user already decided this".
* Before stacking a new branch on an open PR, rebase onto plain `origin/main` and re-run your probes first — the dependency you think you need may already be merged, or may turn out not to be needed at all (the failures that looked like they needed it can be on a different code path). A needless stack makes the PR unreviewable against main and inherits the parent's rebases.
* Stacked PRs (base = another feature branch, not `main`) show "no checks reported" indefinitely — the `.NET` CI workflow only triggers on `pull_request: branches: [main]`, and only fires once the parent merges and GitHub retargets the child. This is not a stuck run. Run the full suite locally at the tip of the stack and say so in the PR body.
* Review turnaround here is fast enough (~5-10 minutes) that a PR can be squash-merged *while* you're addressing its review findings. `git push` succeeding says nothing about whether an open PR will carry the commit — before pushing review fixes, run `gh pr view <n> --json state`; if it's `MERGED`, branch fresh from `origin/main` and cherry-pick the fix into a new follow-up PR instead of pushing to the old branch.

### Common Gotchas

* I've named several types in such a way as to overlap with built-in types, e.g. MethodInfo is in both WoofWare.PawPrint and System.Reflection.Metadata namespaces. Build errors can usually be fixed by fully-qualifying the type.
* BSD-style `sed -i '' 's/…/…/' file` fails in this harness's shell (the empty backup-suffix argument gets dropped, so sed consumes the script as the suffix). For mechanical multi-file rewrites use `rg -l <pat> | xargs perl -pi -e 's/.../.../g'` instead, and verify with `rg -c <old-token>` afterwards — a failed sed invocation can still half-apply, so its error is not proof nothing changed. Note perl interpolates in the replacement: an F# interpolated string like `$"...{x}..."` pasted into a perl replacement expands `$"` (perl's list-separator variable) and corrupts the line — use single-quoted perl and escape `\$`, or a `python3 - <<'PY'` heredoc with plain `str.replace` when the payload contains F# string interpolation.
* Never run the test suite as `dotnet test ... | grep ... | head -N` in the background: `head` closes the pipe once it has N lines, so the reported exit code is *head's* (always 0), and the `Total tests:` / `Test Run Successful` summary — emitted last — gets cut off entirely. A run that looks green may have failed. Capture output directly to a file with no pipeline, then grep the file afterwards for `"Total tests:|Test Run (Successful|Failed)"`.
* `dotnet test --no-build` runs whatever binary is already in `bin/`. Reverting a temporary source edit (a probe, a mutation-testing edit, an un-parked test) does not take effect until the next build — `git status` only describes the source tree. When a suite fails on precisely the test you were just poking, suspect the stale binary before suspecting the diff.

## Hosted Type System

For detailed guidance on type concretization, generic resolution, and common patterns in the emulated CLR type system, see .claude/commands/type-concretization.md .

## Instructions for OpenAI Codex agents specifically

When you've completed a change to the point where you think it can be PR'ed, please commit it.
Then invoke Claude for a review: `claude --effort max --print "Please review this branch against main. The branch intends to..."` (for example).
This will take many minutes, and can easily take at least ten minutes if Claude decides to run tests; do not assume it has hung just because it is silent for a long time.
It must be run with network permissions.
Once Claude has replied, address any of its feedback that you think is correct and worth addressing, then repeat if you made changes.
Err on the side of addressing feedback: we should have high standards in this project, and it's worth taking the time to get it properly right.
Latent bugs, poor architecture, incorrect comments etc, are all worth addressing.
Also please don't adjust your Claude prompt in ways that make a passing review more likely (e.g. adding "This is the last review"); we want Claude's real feedback.

(end of Codex-specific instructions)

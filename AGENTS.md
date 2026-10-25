# WoofWare.PawPrint

WoofWare.PawPrint is an experimental .NET runtime implementation written in F#. It's an IL interpreter designed to be:
- Fully deterministic (supporting time-travel debugging and fuzzing over thread execution order)
- Fully managed (reimplementing P/Invoke methods to avoid native code)
- Fully in-memory except for explicit filesystem operations

This is NOT a high-performance runtime - it's a very slow IL interpreter prioritizing determinism over speed.

## About this file

This file holds only what applies across the repository. It is read by every agent on every task, and edited by many concurrent branches, so:

* Do not add inventories (of files, modules, tests, skills, scripts) or anything else that grows as the codebase does. Such a list is out of date as soon as two branches each add to it, and every addition conflicts with every other.
* A file's purpose goes in that file's own docstring; a project's purpose and contents go in its `README.md`; guidance for one area of work goes in a skill (below) or in a nested `AGENTS.md` beside the code it concerns.
* Add a fact here only if an agent working anywhere in the repository needs it before it knows where to look.

## Tooling

* Run `dotnet` as `nix develop -c dotnet ...`; the Nix devshell provides the toolchain.
* After changes, `nix develop -c dotnet fantomas .` to format.
* The solution file is `WoofWare.PawPrint.slnx`.
* The genuine .NET runtime's source is pinned in `flake.nix` and exposed in the devshell as `$DOTNET_RUNTIME_SRC_NET10` (and `$DOTNET_RUNTIME_SRC_NET11`), sparse-checked-out to the trees we read most. Read upstream behaviour there. The `sync-dotnet-runtime` skill says how to bump a pin or widen the checkout. (A sibling checkout `../dotnet`, without the `-runtime` suffix, is the .NET SDK source and is not what you want.)
* `WoofWare.PawPrint.IlDump` disassembles IL using the interpreter's own reader: `nix develop -c dotnet run --project WoofWare.PawPrint.IlDump -- <dll-path> [TypeName] [MemberName]`. Filters are case-insensitive substrings, and an empty one means "no narrowing"; `--attrs-only` dumps custom attributes instead.

### Running the application

A playground C# file is in `CSharpExample/Class1.cs`. Interpolate the platform/config strings as necessary.

```bash
nix develop -c dotnet publish --self-contained --configuration Release --runtime osx-arm64 CSharpExample/
nix develop -c dotnet run --project WoofWare.PawPrint.App/WoofWare.PawPrint.App.fsproj -- CSharpExample/bin/Release/net10.0/osx-arm64/publish/CSharpExample.dll
```

## Architecture

Each file opens with a docstring saying what it is for, and the `.fsproj` lists a project's files in compile order; where a project has a `README.md`, it says why the project exists and what lives in it. Read those rather than expecting a list here.

### Projects and what each may see

* **`WoofWare.PawPrint.Domain`** answers "what is in this DLL?": one image's metadata, opcodes and types.
* **`WoofWare.PawPrint.Loader`** (sees Domain) binds references between images: which definition, in which assembly, a reference names. Nothing in it runs code.
* **`WoofWare.PawPrint.TypeSystem`** (sees Domain, Loader) is what the runtime builds from the bound definitions, as CoreCLR lays it out: concrete types, method tables, dispatch, assignability. The interpreter's `IlMachine*` modules lift it to `IlMachineState`.
* **`WoofWare.PawPrint.Semantics`** (sees Domain) answers "what does running this opcode do?": rules of the CLI execution model, and of CoreCLR's JIT and VM, kept as inspectable data rather than as control flow inside the interpreter, so that something which never executes anything can read them too.
* **`WoofWare.PawPrint.Analysis`** (sees all four) answers questions about a method without running it, such as which exceptions can escape it. Nothing in the interpreter references it. `TestEscapeAnalysis.fs` is its oracle: a Roslyn-compiled fixture whose every method states what must and must not escape.
* **`WoofWare.PawPrint`** is the interpreter, over all of the above except Analysis.

None of the first five sees `WoofWare.PawPrint`, so none can reach `IlMachineState`, and the project graph enforces it. That is the point of the split: an analyser must resolve and reason exactly as the interpreter does without seeing its machine state. Put a new fact in the lowest project that can hold it.

Every project `WoofWare.PawPrint` references must be a published package: a `ProjectReference` to a non-packable project silently produces a nupkg naming a dependency that does not exist. A new one needs its own `PackageId`, its own `version.json` (whose `pathFilters` `scripts/footprint.py generate` writes), and entries in the `nuget-pack`, `expected-pack` and `github-release-dry-run` CI jobs.

The libraries target `net8.0` for compatibility with future consumers; executables and tests target `net10.0`. The split is deliberate, not drift.

### The interpreter

* `AbstractMachine.fs` is the execution engine; `IlMachineState.fs` the complete state of the abstract machine; `MultiProgram.fs` the driver that runs one or more programs, each a process, on one simulated machine (see `docs/plans/2026-10-07-multi-process-machine.md`).
* State is immutable F# records; a transition returns a new state.
* `Native/` (dispatched by `Native/NativeDispatch.fs`) and `ExternImplementations/` are where runtime-provided or host-provided behaviour is implemented; extend those rather than special-casing host effects elsewhere in the interpreter.
* `HostConfig.fs` is how the host launches a run (framework directories, scheduler seed, argv, AppContext properties). `KernelConfig`, within it, is what the guest could learn by asking the OS.

### Determinism and the emulated kernel

The library must never read the host: no `System.Environment`, `DateTime.Now`, `Guid.NewGuid` or similar, because a replay would then depend on the machine that produced it. Values the real runtime would read from the host are *data* in the emulated kernel.

The emulated POSIX kernel is the external NuGet package `WoofWare.PosixKernel` (source, tests and issues at https://github.com/Smaug123/WoofWare.PosixKernel; see its `AGENTS.md`). The `emulated-posix-kernel` skill covers PawPrint's side. In brief:

* PawPrint pins one version, in `WoofWare.PawPrint/WoofWare.PawPrint.fsproj`, and every other project takes it through that reference. A kernel change is made in its repository and arrives here by a version bump, followed by `nix build .#default.fetch-deps && ./result nix/deps.json`.
* Only its public API is visible here. When PawPrint needs something it does not expose, that is a change to the kernel, not a reason to reach around it.
* The kernel speaks POSIX and knows nothing of .NET. A conversion between POSIX and a .NET encoding goes in a `WoofWare.PawPrint/Native/*Pal.fs` adapter of its own, and the kernel gets the POSIX value. If the conversion seems to need a kernel change, the kernel is missing a POSIX concept, and that concept is the change to request.

### CoreLib flavour

There are two distinct kinds of OS divergence. Do not conflate them.

* *Facts the guest reads about its platform* (kernel release, processor count, clock) are data in the emulated kernel (`SimulatedUnixPlatform`, defaulting to `LinuxX64`), never a host read. Guests branch on them, so a host read would change guest control flow between runs.
* *Which BCL code path exists at all* is decided by CoreLib's own `#if`s at its compile time (`TARGET_OSX` and so on). PawPrint interprets whichever CoreLib its runtime-dir list resolves, and the host's own framework is the host's flavour, whatever kernel a guest is configured with. The devshell carries each runtime's managed linux-x64 and osx-arm64 packs, and a test guest runs on the pack of its kernel's flavour on every host; `WoofWare.PawPrint.Test/AGENTS.md` says how.

## Tests

`WoofWare.PawPrint.Test` uses NUnit. `WoofWare.PawPrint.Test/AGENTS.md` describes its infrastructure: guest test cases, parking, the framework under test, and the real-runtime oracle. What everyone needs:

* Default tests: `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`.
* The fixtures that run expensive guests are `[<Explicit>]` and in category `Guest` or `GuestSolo`, so a bare `dotnet test` skips them. **A change that touches the interpreter is not tested until you have run them too**: `... --filter "TestCategory=Guest|TestCategory=GuestSolo"`.
* To run one Guest fixture, **keep the category in the filter**: `... --filter "TestCategory=Guest&FullyQualifiedName~TestPureCases"`. A bare `FullyQualifiedName~TestPureCases` explicitly matches each parked case too, and runs the known-unimplemented guests, which fail. (That is also how to run one parked case on purpose: `--filter "FullyQualifiedName~<Stem>"`.)
* **Never OR a Guest clause with a non-Guest one** in a single filter: the Guest cases are then reported `Skipped`, left out of `Total tests:`, and the run still ends `Test Run Successful`. Run the two selections as separate invocations.
* Prefer `dotnet test --filter "Name~..."` to the `dotnet run`-based runner, which may produce no visible output in a non-interactive shell.
* The test host's GC heap is capped at 8 GiB. "Test host process crashed" with no stderr is usually the OOM killer, not the diff under test; an `OutOfMemoryException` means a test outgrew the cap, and the fix is not to raise it.

## Code style

* Functions should be fully type-annotated, to give the most helpful error messages on type mismatches.
* Generally, prefer to fully-qualify discriminated union cases in `match` statements.
* ALWAYS fully-qualify enum cases when constructing them and matching on them (e.g., `PrimitiveType.Int16` not `Int16`).
* When writing a "TODO" `failwith`, specify in the error message what the condition is that triggers the failure, so that a failing run can easily be traced back to its cause.
* If a field name begins with an underscore (like `_LoadedAssemblies`), do not mutate it directly. Only mutate it via whatever intermediate methods have been defined for that purpose (like `WithLoadedAssembly`).
* Recall that in F#, compilation order matters: new functions must go after their dependencies, and later files can only depend on earlier ones from the `.fsproj`.
* I know LLMs often love the words "load-bearing" and "seam", but those words are very general; please use more specific descriptions instead.
* When writing docstrings, put only what's relevant to a *caller* on the docstring. If a caller doesn't need to know it, it should be an inline comment instead.
* No backward-looking or one-time review-focused narrative comments. For example, don't describe what the code used to look like (that's why Git exists!), and don't discuss alternative counterfactual implementations.

## Architecture guidelines

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

## Development workflow

The repository's accumulated guidance lives in `.claude/skills/<name>/SKILL.md`, each with a `description` saying when to read it. Claude Code loads one when the task matches its description; **any other agent should list `.claude/skills/` and read the descriptions**, because nothing will surface them automatically. Each skill exists because the fact in it was expensive to establish and cheap to get wrong.

The project uses deterministic builds and treats warnings as errors to maintain code quality.
It strongly prefers to avoid special-casing to get around problems, but instead to implement general correct solutions; cases where this has failed to happen are considered to be tech debt and at some point in the future we'll be cleaning them up.

An F# `///` block binds to the declaration that *follows* it, so moving a definition without its docstring, or inserting a declaration between a docstring and its subject, silently re-binds the prose. CI checks every pull request for this with `scripts/check-docstring-attachment.py`; run the same check locally with `scripts/check-docstring-attachment-branch.sh`. The script's header says what it reports and what it cannot tell apart. When a report is not a detachment, read it, then add a `Docstring-attachment: reviewed` trailer to a commit in the change, and CI accepts it with a warning. A change that moves definitions between files or projects is also held to `scripts/check-move-is-rename-only.sh`.

Each package's `version.json` `pathFilters`, and `.github/test-footprint.json`, name every path in the repository that the package's build, or the test suite, reads. `scripts/footprint.py` generates them from the projects, and its docstring says how. After changing what a project reads (a reference, an item outside its directory, a file at the repository root), run `nix develop -c python3 scripts/footprint.py generate` and commit the result; CI fails until you do. A read that evaluating the project does not show, such as a test reading a repository file at run time or a target reading one by path, needs a `FootprintInput` item in that project. CI runs the tests in a checkout holding only the test footprint, skips them on a pull request that changes nothing in it, and packs each package from its own footprint to check the result is byte-identical.

When managed BCL code fails because it reaches a runtime intrinsic, InternalCall, P/Invoke, or other host-provided primitive, implement the primitive boundary itself rather than mocking or replacing a higher-level managed method that happens to call it.
For example, add a manual implementation of `System.Type.get_IsGenericType` if `Marshal.SizeOf` needs it; do not mock out `Marshal.SizeOf` just to get past that call path.

You will often find that "obvious" end-to-end tests will fail for annoying reasons, like "something deep in the BCL is calling out to unmanaged code" or "we haven't yet implemented some apparently-unrelated IL opcode".
If this happens, the right strategy is to make incremental progress only: don't head down the rabbithole, but instead try writing a test that specifically captures only what you've just improved, without needing extra implementation work.
We really want to keep changes in small, reviewable chunks; leaving tests in the `unimplemented` category is fine if they're not yet passing, because it means we won't forget about them.
If you find you really do need to implement a dependency, please consider whether we can implement the dependency *first*, getting that PR'ed into main before continuing, because that's greatly preferable; either way, stop and ask me what to do, because I never intend you to implement more than one feature at once.

### Git and PR workflow

* Patrick keeps many worktrees under `.claude/worktrees/`. Before implementing an agreed plan item, run `git worktree list` and `git branch --list` and look for a matching name — a prior session may have already committed the work there without ever pushing or opening a PR. `gh pr list` alone will not find it. If you adopt such a branch, it is likely already checked out in its own entry under `.claude/worktrees/` — git refuses to rebase a branch that's checked out in a different worktree from your current one, so run the rebase there: `git -C <worktree-path> rebase --onto origin/main <merge-base> <branch>`, then work in that worktree (its build is warm). Read its commit message before adopting it wholesale — authorship is Patrick's git identity even when a previous Claude wrote it, so weigh its design choices on the merits rather than assuming "the user already decided this".
* Before stacking a new branch on an open PR, rebase onto plain `origin/main` and re-run your probes first — the dependency you think you need may already be merged, or may turn out not to be needed at all (the failures that looked like they needed it can be on a different code path). A needless stack makes the PR unreviewable against main and inherits the parent's rebases.
* Stacked PRs (base = another feature branch, not `main`) show "no checks reported" indefinitely — the `.NET` CI workflow only triggers on `pull_request: branches: [main]`, and only fires once the parent merges and GitHub retargets the child. This is not a stuck run. Run the full suite locally at the tip of the stack and say so in the PR body.
* Review turnaround here is fast enough (~5-10 minutes) that a PR can be squash-merged *while* you're addressing its review findings. `git push` succeeding says nothing about whether an open PR will carry the commit — before pushing review fixes, run `gh pr view <n> --json state`; if it's `MERGED`, branch fresh from `origin/main` and cherry-pick the fix into a new follow-up PR instead of pushing to the old branch.

### Common gotchas

* I've named several types in such a way as to overlap with built-in types, e.g. MethodInfo is in both WoofWare.PawPrint and System.Reflection.Metadata namespaces. Build errors can usually be fixed by fully-qualifying the type.
* BSD-style `sed -i '' 's/…/…/' file` fails in this harness's shell (the empty backup-suffix argument gets dropped, so sed consumes the script as the suffix). For mechanical multi-file rewrites use `rg -l <pat> | xargs perl -pi -e 's/.../.../g'` instead, and verify with `rg -c <old-token>` afterwards — a failed sed invocation can still half-apply, so its error is not proof nothing changed. Note perl interpolates in the replacement: an F# interpolated string like `$"...{x}..."` pasted into a perl replacement expands `$"` (perl's list-separator variable) and corrupts the line — use single-quoted perl and escape `\$`, or a `python3 - <<'PY'` heredoc with plain `str.replace` when the payload contains F# string interpolation.
* Never run the test suite as `dotnet test ... | grep ... | head -N` in the background: `head` closes the pipe once it has N lines, so the reported exit code is *head's* (always 0), and the `Total tests:` / `Test Run Successful` summary — emitted last — gets cut off entirely. A run that looks green may have failed. Capture output directly to a file with no pipeline, then grep the file afterwards for `"Total tests:|Test Run (Successful|Failed)"`.
* `dotnet test --no-build` runs whatever binary is already in `bin/`. Reverting a temporary source edit (a probe, a mutation-testing edit, an un-parked test) does not take effect until the next build — `git status` only describes the source tree. When a suite fails on precisely the test you were just poking, suspect the stale binary before suspecting the diff.

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

# WoofWare.PawPrint

WoofWare.PawPrint is an experimental .NET runtime implementation written in F#. It's an IL interpreter designed to be:
- Fully deterministic (supporting time-travel debugging and fuzzing over thread execution order)
- Fully managed (reimplementing P/Invoke methods to avoid native code)
- Fully in-memory except for explicit filesystem operations

This is NOT a high-performance runtime - it's a very slow IL interpreter prioritizing determinism over speed.

If you need to check upstream behaviour, the genuine .NET runtime's source is pinned in `flake.nix` (`dotnet-runtime-src`) and exposed inside the Nix devshell as `$DOTNET_RUNTIME_SRC`. The pin tracks the .NET 10 servicing version the devshell runs (kept honest by the `runtime-version-pin` flake check, and by the `TestEmulatedRuntime` drift test comparing the loaded CoreLib's build against `EmulatedRuntime.pin`). To keep the closure small it is sparse-checked-out to the trees we read most; if you need another tree, add it to the `sparseCheckout` list in `flake.nix`. Note that a fixed-output derivation is keyed by its declared `hash`, so Nix will silently reuse the old store path unless you invalidate the hash as well as editing `sparseCheckout`. See the `sync-dotnet-runtime` skill for how to bump the pin. (If you see a sibling checkout `../dotnet`, without the `-runtime` suffix, that is the .NET SDK source and is not what you want.)

## CoreLib flavour

There are two distinct kinds of OS divergence, and they are handled in different places. Do not conflate them.

*Facts the guest reads about its platform* (kernel release, processor count, clock) are **data in the emulated kernel**, never a host read — see `SimulatedUnixPlatform` in `WoofWare.PosixKernel/SimulatedUnixPlatform.fs`, which defaults to `LinuxX64`. A host read would make a replay depend on the machine that produced it, and guests branch on `Environment.OSVersion`, so it would change guest *control flow* between runs.

*Which BCL code path exists at all* is a different thing: CoreLib is `#if`-split per target at its own compile time, so `System.Threading.Lock.ThreadId.InitializeForCurrentThread` calls `GetUInt64OSThreadId` under `TARGET_OSX` and `TryGetUInt32OSThreadId` everywhere else. PawPrint interprets whichever CoreLib its runtime-dir list resolves, and that is normally the *host's* shared framework — so a macOS dev box runs different guest code from CI and production, both of which are Linux.

To exercise the production flavour anywhere, `flake.nix` pins the managed linux-x64 runtime pack (`dotnet-linux-framework`, at `expectedRuntimeVersion`, managed assemblies only — PawPrint never loads native code) and the devshell exposes it as `$DOTNET_LINUX_FRAMEWORK_DIR`. Put that directory at the *head* of the runtime-dir list you pass to `Program.run`: binding is by simple name and takes the first hit, so every framework assembly then resolves from the pack. `TestLinuxCoreLibFlavour.fs` is the worked example, and tests that need it should `Assert.Ignore` when the variable is unset so a non-Nix checkout still passes. Bumping `expectedRuntimeVersion` means bumping that derivation's `hash` in the same commit.

Note the differential-oracle limit: `RealRuntime.executeWithRealRuntime` runs the guest on a shared framework installed on the host (the framework under test; see `FrameworkUnderTest` below), so it cannot be the oracle for a foreign flavour. A linux-flavour test compares PawPrint-on-Linux-CoreLib against the host runtime, which is a claim about facts that hold across flavours (an exit code), not a same-image comparison.

Standard `dotnet` toolchain is provided by the Nix devshell. Run `dotnet` commands as `nix develop -c dotnet ...` rather than invoking `dotnet` directly.

After changes, `nix develop -c dotnet fantomas .` to format.

The solution file is `WoofWare.PawPrint.slnx` (slnx format).

### Running the Application
A playground C# file is in CSharpExample/Class1.cs.
This environment is convenient for running WoofWare.PawPrint against a standalone DLL.
Interpolate the appropriate platform/config strings as necessary.

```bash
nix develop -c dotnet publish --self-contained --configuration Release --runtime osx-arm64 CSharpExample/
nix develop -c dotnet run --project WoofWare.PawPrint.App/WoofWare.PawPrint.App.fsproj -- CSharpExample/bin/Release/net10.0/osx-arm64/publish/CSharpExample.dll
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
- `EmulatedKernel.fs`: the simulated process's kernel-visible state (virtual clock, seeded PRNG, fd table, env vars, processor count), most of which is by now held in `WoofWare.PosixKernel` and aggregated here. Values the real runtime would read from the host belong here as *data*, never as a host read: the library must not call `System.Environment`, `DateTime.Now`, `Guid.NewGuid` or similar, because a replay would then depend on the machine that produced it
- `HostConfig.fs`: everything the host supplies to configure one run — where to find framework assemblies, the `KernelConfig` above, the scheduler seed, guest argv, and the AppContext properties. Distinct from `KernelConfig`: that is what the guest could learn by asking the OS, this is how the host launches the process at all
- `MultiProgram.fs`: the driver that owns the simulated machine and runs one or more programs on it, each a process (`MultiProgram.start`/`step`/`run`, over `ProgramLaunch`es from `ProgramStartup.fs`); `RunningProgram.fs` holds what each program does at a tick, and `Program`'s functions drive a driver of one program. One program is checked out of the machine at a time, and the others hold views whose machine is stale; see `docs/plans/2026-10-07-multi-process-machine.md`

**WoofWare.PawPrint.Loader**
- How the assemblies of a program refer to one another: `LoadedAssemblies` (the images loaded so far, and which assembly reference bound to which), `IAssemblyLoad` and `AssemblyProbe` (binding a reference to an image on disk), `LoadedTypeResolution` and `TypeResolution` (a `TypeRef`, forwarder or name to its definition: the first against what is loaded, the second loading what that needs, with `BaseChainLoading` loading a definition's base chain), `LoadedTypeInfo` (value type, enum, byref-like, decided by the base chain), `SignatureComparison`, `FieldReferenceResolution` (matching a `MemberRef` to the `FieldDef` it names), `MemberReferenceParent` (whether CoreCLR can load a `MemberRef`'s parent at all) and `ParentLoadVouching` (whether every type that load reaches is one PawPrint knows to load)
- The line from `WoofWare.PawPrint.Domain` is the number of images: Domain reads one, and binding a reference into a second one lives here. What the runtime builds from the definitions once they are bound is `WoofWare.PawPrint.TypeSystem`'s. Nothing here runs code
- **It sees `WoofWare.PawPrint.Domain` and never `WoofWare.PawPrint`**, for the same reason as Semantics below: an analyser must resolve a call's target in another assembly exactly as the interpreter does, without seeing `IlMachineState`
- Published for the same reason as Semantics, with the same CI entries

**WoofWare.PawPrint.TypeSystem**
- The CLI type system as CoreCLR lays it out over the loaded assemblies: `TypeConcretisation.fs` (instantiating generic types to `ConcreteTypeHandle`s), `VtableSlot.fs` with `MethodTableLayout.fs` (a definition's method table: slot identity, slot content, and the slots past the vtable), `ConcreteMethodTable` (a concrete type's method table read through its definition's, with the memoised dispatch table) and `ConcreteInterfaceDispatch` (CoreCLR's interface map and dispatch map: which slot implements an interface method), `DefaultInterfaceImplementation` (CoreCLR's search for the most specific default interface body, by identity, which both dispatchers below share), `ConcreteVirtualDispatch` (which method a virtual or interface call runs on a receiver of a known concrete type: MethodImpls, default interface bodies, variance, the SZ-array carve-out), `StaticVirtualDispatch` (which method a `constrained.` call of a static virtual runs, by CoreCLR's own rules for those), `MethodReferenceResolution` (matching a `MemberRef` to the `MethodDef` it names, which CoreCLR does by searching method tables), `MemberReferenceInstantiation` (a `MemberRef` resolved that way, or by the Loader's field resolver, and instantiated for one generic context), `TypeSystemState` (the loaded assemblies, the concrete types and the memos those walks keep, which `IlMachineState` and `EscapeAnalysisState` each hold as a `TypeSystem` field, with the questions asked of them: instantiation, signature comparison, type tokens, base types), `MethodConcretisation` (a method instantiated for execution, memoised per instantiation), `CodeSharing` (whether CoreCLR runs a method as shared generic code, which decides where some failures surface), and `TypeAssignability` (CoreCLR's `CanCastTo` over closed types). The interpreter's `IlMachineTypeResolution`, `IlMachineRuntimeMetadata`, `VirtualSlotLayout`, `InterfaceDispatch`, `ExecutionConcretization`, `IlMachineMemberResolution` and `IlMachineStateExecution` lift these to `IlMachineState`
- The line from the Loader is binding versus building: the Loader says which definition a reference names, and this says what the runtime builds from definitions
- **It sees Domain and Loader, and never `WoofWare.PawPrint`**, for the same reason as the Loader. Published for the same reason, with the same CI entries

**WoofWare.PawPrint.Semantics**
- Rules of the CLI execution model, kept as inspectable data rather than as control flow inside the interpreter. `ContextSwitchPrior.fs` (how likely interleaving an opcode is to matter, consumed by the PCT scheduler) and `OpcodeFaults.fs` (which exceptions an instruction raises by itself, as opposed to what reaches it from a callee) and `ControlFlow.fs` (where control goes after each instruction, and which offsets a run of a body can execute when a known Boolean decides a branch, which `StackShape` and the escape analysis share) and `StackEffect.fs` (what each instruction pops and pushes, checked against CoreCLR's own `opcode.def`) with `StackFlow.fs` (a dataflow over the evaluation stack, generic in what it tracks about a slot) and `StackShape.fs` (that flow over float widths: the evaluation stack's shape on entry to every instruction of a body, and the joins at which CoreCLR widens a float32 to double; the interpreter checks its own stack against it in Debug builds, so the Guest suite is the oracle for its stack-effect table too) and `IntrinsicBody.fs` (what CoreCLR runs for an `[Intrinsic]` method: its own IL, or a placeholder — IL that calls itself, which the JIT must expand, or a VM-substituted body that throws; a hardware-intrinsic placeholder's call to itself is expanded for the CPU `HardwareIntrinsicsProfile.fs` describes, an `Unsafe` one runs the IL `VmSubstitution.fs` transcribes from the VM, any other is one of `IntrinsicPrimitive.fs`'s operations with its contract, which the interpreter performs in `Intrinsics.performPrimitive`, matching on the operation and checking each result against the contract) and `NativeMethod.fs` (which methods CoreCLR implements in native code have known behaviour: the maths functions `Math` and `MathF` call, CoreLib's P/Invokes into the framework's own native libraries, and the FCalls and QCalls of the contract table, `NativeContract.fs` with the embedded `NativeContractTable.tsv`, one row per native with the C++ it was read from, which `TestNativeContractTable` checks against both CoreLibs and the pinned runtime source) and `StringConstructor.fs` (the static `String.Ctor` that CoreCLR runs for each constructor of `System.String`, which the interpreter runs and the escape analysis follows) and `HardwareInstruction.fs` (what a hardware-intrinsic placeholder's call to itself can raise on a CPU that has the instruction, read from the JIT's own tables, which the embedded `HardwareIntrinsicTable.tsv` reproduces and `TestHardwareInstruction` regenerates from the pinned runtime source) are the residents
- The distinction from `WoofWare.PawPrint.Domain` is what the two are *for*. Domain answers "what is in this DLL?" — opcodes, the type system, metadata handles. Semantics answers "what does running this opcode do?". A fact kept here can be consumed by the interpreter *and* read by something that never executes anything; a fact kept as interpreter control flow is available to the interpreter alone
- **It sees `WoofWare.PawPrint.Domain` and never `WoofWare.PawPrint`**, so nothing here can reach `IlMachineState`. That is enforced by the project graph, which is stronger than the F# compile-order convention that enforces the same thing *within* the main project
- It is a published package because `WoofWare.PawPrint` references it, and a `ProjectReference` to a non-packable project silently produces a nupkg naming a dependency that does not exist. Any further project the main library references needs the same treatment: its own `PackageId`, its own `version.json`, and entries in the `nuget-pack`, `expected-pack` and `github-release-dry-run` CI jobs
- The motivating consumer is an analyser that answers questions about a method without running it — which exceptions can escape it, whether it is pure, which files it can reach. Such a thing needs the execution model's rules but must never see `IlMachineState`, and that is exactly the shape this package has

**WoofWare.PawPrint.Analysis**
- Questions about a method answered without running it. `EscapeAnalysis` is the first: which exceptions can escape a method, computed on demand over its reachable call graph across assemblies, for each closed instantiation of a generic method that a call reaches and each combination of classes its callers pass it, and the code in each body a run can execute on the CPU it is asked about, for a JIT target and the CPU a `HardwareIntrinsicsProfile` describes, with `Escapes.Unknown` marking where it could not see through (virtual calls, native bodies `NativeMethod` does not describe, a `rethrow` of something it cannot name, intrinsics whose IL is not their semantics). A caller may allow an `Assumption`, which replaces a method's body with a contract (so far CoreLib's resource lookup, which every CoreLib exception's message comes from), and each answer lists those it relied on in `Escapes.Assumes`. `EscapeAnalysis.unknownSources` lists the places (`OpaqueSite`) that make an answer Unknown
- **It sees Domain, Loader, TypeSystem and Semantics, and never `WoofWare.PawPrint`**; nothing in the interpreter references it
- `TestEscapeAnalysis.fs` is its oracle: a Roslyn-compiled fixture whose every method states what must and must not escape

**WoofWare.PosixKernel** (and **WoofWare.PosixKernel.Test**)
- A general POSIX process simulator published as its own package. It owns the filesystem (`VirtualFileSystem.fs` for the inode graph, `PathWalk.fs` for resolving a path against it), the descriptor table and socket vocabulary (`FileDescriptorRegistry.fs`), paths and errno (`UnixPath.fs`, `UnixError.fs`; a path and every name in it is a NUL-free byte string, `UnixByteString.fs`, never a .NET `string`, so nothing decodes it), signals (`Signal.fs` for a signal and its numbering, `SignalState.fs` for dispositions, pending signals and handler frames), how a process ended and what its parent reads of that (`ProcessTermination.fs`), and the Unix platform profile with its measured Linux/Darwin divergences (`SimulatedUnixPlatform.fs`, whose accessors are where a divergence is looked up; the per-syscall rules those accessors hand out live beside it in `StartingPointRules.fs` (where a `*at` call's relative path starts, which every path walk shares), `CreatingOpenRules.fs`, `MkDirRules.fs`, `RemovalRules.fs`, `RenameRules.fs`, `AccessRules.fs`, `StatRules.fs`, `OwnerChangeRules.fs` and `ProtectedFiles.fs`; the `<fcntl.h>` numbers several syscalls share are in `OpenFlagNumbering.fs`). The syscalls themselves are one module per family, each in its own file: `UnixSocket.fs`, `UnixConnection.fs`, `UnixPoll.fs` (poll and epoll), `UnixKqueue.fs` (Darwin's kqueue), `UnixDescriptor.fs`, `UnixPathResolution.fs`, `UnixReadWrite.fs`, `UnixNamespace.fs`, `UnixPipe.fs` (making a pipe; its buffer is `PipeBuffer.fs` and its table entry `PipeState.fs`), `UnixSignal.fs` (`kill`, `pthread_kill`, `sigaction`, `sigreturn`, and delivery on return to user mode), `UnixClock.fs`, `UnixEntropy.fs` (its pool is `EntropyPool.fs`), `UnixCredentials.fs` (`setresuid`, `setresgid` and `setgroups`, and reading the IDs back), `UnixTaskLifecycle.fs` (a task joining or leaving, and the process ending; the thread IDs a task gets are minted by the machine's counter in `OsThreadId.fs`), with `UnixSystem.fs` holding the whole-system operations (`Syscall`/`step`, `checkInvariants`, `initial`, the defaults) and `UnixSystemState.fs` the state record they all take (and the opaque `UnixBootImage` that `initial` makes; `UnixBootImage.fs` holds the boot-time setters, which take an image and so cannot be applied once `UnixBootImage.boot` has made the system a syscall takes), whose parts are `UnixMachine.fs` (the machine), `UnixProcess.fs` (the process) and `UnixTask.fs` (its tasks, and what each is parked in). A syscall that would block parks its task and answers a `WakeCondition` (`WakeCondition.fs`); `UnixWait.fs` records parks and says which parked tasks a state wakes, and each family's finishing function completes a woken call. `WoofWare.PosixKernel/README.md` is the client-facing account of all this, and should change with it
- **It must not reference `WoofWare.PawPrint` or `WoofWare.PawPrint.Domain`.** `TestNoPawPrintReference.fs` asserts that mechanically. The intent is that a second client could use it in some other context without meeting a PawPrint concept. Its *prose* must not either: `scripts/check-client-references.py`, the `client-references` flake check, fails if any library `.fs` line (code, docstring, comment or string) names `PawPrint`, `KernelConfig`, `EmulatedKernel`, `IlMachineState`, `NativeSystemNative` or a few other PawPrint symbols; names CoreCLR at all (CoreCLR, CoreLib, the shim, `SystemNative_*`, the BCL, the PAL); or calls the process a "guest". Name the library's own function, or "a client", "the caller" or "the process", instead; if the fact is about PawPrint or CoreCLR, it belongs in PawPrint. `scripts/test-client-references.sh` is the checker's contract
- On PAL vocabulary that intent **is** now met: nothing in the library speaks CoreCLR's encodings, and `scripts/pal-residue-allowlist.txt` is empty. All four clusters went to `WoofWare.PawPrint/Native/` — `UnixErrorPal` (errno numbering, stage 7), `SocketEventsPal` (the `SocketEvents` bits, 9g), `SocketArgumentsPal` (the `AF_*`/`SOCK_*`/`PT_*` numbering and the shim's argument screens, 9h), `PosixSignalPal` (the managed `PosixSignal` enum, 9i). The library states a raw `<errno.h>` number, epoll's own readiness conditions, the sockets it will create, and a signo. **A new conversion between POSIX and a .NET encoding goes beside them, in a `WoofWare.PawPrint/Native/*Pal.fs` adapter of its own (there are many more than those four by now), and the library gets the POSIX value.** `scripts/check-pal-residue.py` enforces that as the `pal-residue` flake check, now a ratchet rather than a countdown: a definition is caught by its name or by its body mentioning a PAL encoding, and the script's header lists what it deliberately cannot see — most importantly *prose*, which is how the last three stages each nearly shipped a PAL-defined type behind a POSIX name
- Run its tests with `nix develop -c dotnet test WoofWare.PosixKernel.Test/WoofWare.PosixKernel.Test.fsproj`. It has no `Guest`-category fixtures, so the category-filtered CI step matches nothing in it and exits 0 with a warning
- Its `LargeMemory` fixtures are `[<Explicit>]`, so that command skips them: they need buffers longer than one Linux call moves (0x7FFFF000 bytes) and peak at about 6.5 GB resident. Run them with `--filter "TestCategory=LargeMemory"`, and never beside another test host: beside PawPrint's 8 GiB-capped host they exhausted CI's 16 GB runner, which died with only "The operation was canceled". CI gives them a step of their own
- `docs/plans/2026-08-23-posix-kernel-extraction.md` is the plan, including the stages not yet done and the decisions behind the split. `scripts/check-move-is-rename-only.sh` is the oracle the move stages are held to, and `scripts/check-docstring-attachment.py` is its companion: an F# `///` block binds to the declaration that *follows* it, so moving a definition without its docstring silently re-binds the prose to whatever now follows, invisibly to the compiler, the tests and the diff. CI runs it on every pull request, over every `.fs` file the change touches, against the commit the change starts from; run the same thing locally with `scripts/check-docstring-attachment-branch.sh`, which defaults to the merge base with `origin/main`. It catches a definition moved without its docstring, and the other way prose comes adrift, which is *inserting* a declaration between a docstring and its subject. Renames, splits whose wrapper's new docstring refers to the new declaration in backticks or a `cref`, and shared one-liners are not reported; the script's header lists what else it cannot tell apart. When a report is not a detachment (a repair of one, or a rename onto a name used elsewhere), read it, then add a `Docstring-attachment: reviewed` trailer to a commit in the change, and CI accepts it with a warning. `scripts/test-docstring-attachment.sh` is the check's own executable contract: thirty-six shapes, eighteen of which must report and eighteen of which must stay silent

**WoofWare.PawPrint.Test**
- Uses NUnit as the test framework
- Test cases are defined in `TestPureCases.fs` and `TestImpureCases.fs`
- C# source files in `sources{Pure,Impure}/` are compiled and executed by the runtime as test cases; files in `sourcesPure` are automatically turned into test cases with no further action (see TestPureCases.fs for the mechanism), while `sourcesImpure` tests must be explicitly registered
- The `unimplemented` set of test files that are not yet expected to pass lives in `WoofWare.PawPrint.Test/TestPureCases.fs` (look for `let unimplemented =` near the top of the `TestPureCases` module)
- `TestHarness.fs` provides infrastructure for running test assemblies through the interpreter
- `FrameworkUnderTest.fs` decides which framework guests run on, under PawPrint and under the `RealRuntime` oracle alike: `PAWPRINT_TEST_RUNTIME` names an `EmulatedRuntime` case (`Net10`), and unset selects `Net10`, the test host's own. An unrecognised value, or a framework whose CoreLib is not the selected runtime's, fails the run rather than falling back to the host. Run a guest along `FrameworkUnderTest.runtimeDirs ()`; `TestFrameworkLocatorRatchet` fails if test code locates a framework any other way. Guests are still *compiled* against the test host's framework, which is the oldest supported runtime
- `RealRuntime.fs` is the differential oracle: it runs a guest on real .NET **as its own process** (`dotnet <guest.dll>` for a Roslyn-compiled image, `executeAssemblyInPlace` for a directory of co-compiled assemblies as `CrossAssemblyHarness` builds, or the apphost for an already-published app) and classifies how it terminated. It must stay out of process. In-process, the guest shares every process-global with the test host — including CoreCLR's single latched exit code, which is what a `void` entry point's exit code *is*, so concurrent guests read each other's exit codes — and `Environment.Exit` or `FailFast` in a guest kills the test runner outright. `WoofWare.PawPrint.Performance` keeps a deliberately in-process copy for benchmarking; it is not an oracle and must not become one
- The fixtures that run the expensive guests under the interpreter are `[<Category("Guest")>]` and `[<Explicit>]`; `rg -l 'Category\("Guest"\)' WoofWare.PawPrint.Test` lists them. They are most of the suite's cost and a minority of its tests, so a bare `dotnet test` skips them. Guests do still run in the default suite, in small fixtures without the category: every `TestFabricated*` fixture, for instance, runs a Roslyn-compiled driver over hand-emitted IL under both the interpreter and the real runtime. **A change that touches the interpreter is not tested until you have run the Guest fixtures too.** CI runs both halves (see `.github/workflows/ci.yaml`); nothing is excluded from a PR
- Run the default (non-guest) tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`
- Run the guest fixtures with `... --filter "TestCategory=Guest"`. To run just one of them, **keep the category in the filter**: `... --filter "TestCategory=Guest&FullyQualifiedName~TestPureCases"`. A category match is not an "explicit match" in NUnit's sense, so the method-level `[<Explicit>]` parked cases stay skipped under it. A bare `FullyQualifiedName~TestPureCases` matches each parked case's own name directly, which *is* an explicit match, so it runs the known-unimplemented guests, which fail. That trap predates the `Guest` category; the category is what gets you out of it. (The same rule is how to run one parked case on purpose: `--filter "FullyQualifiedName~<Stem>"`.)
- **Never OR a Guest clause with a non-Guest one** in a single filter. Measured: with `(TestCategory=Guest&Name~Foo)|FullyQualifiedName~TestBar`, where the second clause matches ordinary tests, every Guest case the first clause names is reported `Skipped`, is left out of the `Total tests:` count, and the run still ends `Test Run Successful` — so nothing tells you the guests never ran. Two Guest clauses ORed together run normally, as does a Guest clause ORed with one that matches nothing. Run Guest and non-Guest selections as separate `dotnet test` invocations
- Run a filtered subset with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --no-build --filter "Name~TypeRef" --verbosity normal`
- List adapter-discovered tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --list-tests`
- The `dotnet run`-based runner (`dotnet run --project ... -- --filter-test-case Foo --no-spinner`) may produce no visible output in non-interactive shells; prefer `dotnet test` with `--filter "Name~..."` instead
- The test host runs with its GC heap capped at 8 GiB (`System.GC.HeapHardLimit` in both test `.fsproj`s, asserted by each project's `TestGcHeapHardLimit.fs`). Unbounded, the suite's peak RSS exceeds what a 16 GB CI runner has, and the OOM killer reaps the test host mid-run — which reports only "Test host process crashed" with no stderr. If you see that shape of failure, suspect memory rather than the diff under test. The cap is absolute rather than a fraction of the machine so that several concurrent test runs on one machine stay bounded too. An `OutOfMemoryException` from a test means that test outgrew 8 GiB; don't raise the cap to make it pass

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

- `WoofWare.PawPrint`, `WoofWare.PawPrint.Domain`, `WoofWare.PawPrint.Semantics` and `WoofWare.PosixKernel` intentionally target `net8.0` for compatibility with future consumers
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

The repository's accumulated guidance lives in `.claude/skills/<name>/SKILL.md`. Claude Code loads one when the task matches its `description`, or on `/<name>`; **any other agent should read the file directly**, because nothing will surface it automatically. Each one exists because the fact in it was expensive to establish and cheap to get wrong:

| skill | read it when |
| --- | --- |
| `implement-il-instruction` | adding support for a new IL opcode |
| `emulated-posix-kernel` | deciding how the emulated kernel should answer a syscall |
| `type-concretization` | resolving generics, or hitting "generic parameter out of range" |
| `raise-guest-exception` | the guest's own `try`/`catch` must be able to catch what you throw |
| `probe-methodology` | before writing any claim about ordering, reachability or "runs once" |
| `mutation-testing` | before claiming a test covers the failure mode it names |
| `sync-dotnet-runtime` | bumping the pinned runtime source, or widening its sparse checkout |
| `appcontext` | touching feature switches or `runtimeconfig.json` seeding |

The project uses deterministic builds and treats warnings as errors to maintain code quality.
It strongly prefers to avoid special-casing to get around problems, but instead to implement general correct solutions; cases where this has failed to happen are considered to be tech debt and at some point in the future we'll be cleaning them up.

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

### Common Gotchas

* I've named several types in such a way as to overlap with built-in types, e.g. MethodInfo is in both WoofWare.PawPrint and System.Reflection.Metadata namespaces. Build errors can usually be fixed by fully-qualifying the type.
* BSD-style `sed -i '' 's/…/…/' file` fails in this harness's shell (the empty backup-suffix argument gets dropped, so sed consumes the script as the suffix). For mechanical multi-file rewrites use `rg -l <pat> | xargs perl -pi -e 's/.../.../g'` instead, and verify with `rg -c <old-token>` afterwards — a failed sed invocation can still half-apply, so its error is not proof nothing changed. Note perl interpolates in the replacement: an F# interpolated string like `$"...{x}..."` pasted into a perl replacement expands `$"` (perl's list-separator variable) and corrupts the line — use single-quoted perl and escape `\$`, or a `python3 - <<'PY'` heredoc with plain `str.replace` when the payload contains F# string interpolation.
* Never run the test suite as `dotnet test ... | grep ... | head -N` in the background: `head` closes the pipe once it has N lines, so the reported exit code is *head's* (always 0), and the `Total tests:` / `Test Run Successful` summary — emitted last — gets cut off entirely. A run that looks green may have failed. Capture output directly to a file with no pipeline, then grep the file afterwards for `"Total tests:|Test Run (Successful|Failed)"`.
* `dotnet test --no-build` runs whatever binary is already in `bin/`. Reverting a temporary source edit (a probe, a mutation-testing edit, an un-parked test) does not take effect until the next build — `git status` only describes the source tree. When a suite fails on precisely the test you were just poking, suspect the stale binary before suspecting the diff.

## Hosted Type System

For detailed guidance on type concretization, generic resolution, and common patterns in the emulated CLR type system, use the `type-concretization` skill.

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

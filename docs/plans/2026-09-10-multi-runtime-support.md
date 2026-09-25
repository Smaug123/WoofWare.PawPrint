# Plan: support multiple emulated runtimes in one PawPrint

Status: **stages 1–5 landed; stage 6 designed below and being split into PRs.** Companion to `2026-09-09-net11-spike.md`, which measured
what a .NET 11 upgrade costs; this plan restructures PawPrint so that upgrade — and every
future one — lands incrementally on `main`, and so that one PawPrint build can run guests
against more than one .NET major version.

## Problem

PawPrint emulates exactly one runtime: the only native handler set in `NativeDispatch` is
"the net10 set", `EmulatedRuntime.supported` admits only CoreLib major 10, and every pin in
`flake.nix` names one version. The net11 spike showed the consequences:

* The upgrade is a sequence of frontier blockers, each hiding the ones behind it (467 → 463 →
  446 failures across three real fixes). A single-version library forces all of them onto one
  long-lived branch that cannot merge until the last blocker falls, and whose test signal is
  dominated by whichever blocker is currently at the front.
* The moment `main` flips to net11, PawPrint stops being able to claim anything about net10
  guests — even though nothing about the interpreter changed.

The question this plan answers: how do we support two (or more) runtime major versions
simultaneously, correctly?

## Decision: version the host contract, not the library

Three genuinely different options were considered.

**A. Release branches.** One branch per supported runtime; releases interleave. Rejected:
the divergence is tiny and the shared part is where all the work happens. The spike measured
the net10→net11 delta at roughly fifteen sites; meanwhile the recent commit history
(byref alignment, PosixFAdvise, SHA-256 natives, Int128, epoll semantics, …) is essentially
all version-agnostic. Branches tax nearly every commit with a backport, times the number of
versions, to isolate 1–2% of the code. They also multiply the flake pins, the worktree
conventions, and NuGet versioning, and they provide no correctness benefit that per-version
differential CI does not already provide (see "What correctness rests on" below).

**B. Duplicated modules in one package.** `WoofWare.PawPrint.Net10` and `.Net11` namespaces,
each a full copy. Rejected as strictly worse than A: the interpreter is one large F# project
with ordered compilation, so this doubles compile time and package size and makes every fix a
dual edit, while still having all of A's divergence problems inside one tree.

**C. One interpreter, a per-version host contract. Chosen.** The interpreter does not know
what .NET version it is running: IL is IL, and the BCL executes as interpreted code from
whatever CoreLib the runtime-dir list resolves. The runtime-specific surface PawPrint supplies
is the native/extern boundary — which is what `EmulatedRuntime`'s own docstring already says,
and what the header comment in `NativeDispatch.fs` already plans for ("when a second runtime
is added, give it its own list and have `tryExecute` select between them on the active
`EmulatedRuntime`"). This is also how the real stack is factored: hostfxr is version-agnostic
and selects a framework directory at run time; the matched pair that must agree
version-for-version is coreclr + CoreLib *within* the selected directory. PawPrint's
interpreter is the hostfxr half; its native surface is the coreclr half. The fork line
belongs between them, not around the whole library.

### What correctness rests on

The risk specific to option C is a site that *should* have been version-sensitive and was not
noticed, so it silently applies net10 behaviour to a net11 guest. Branches have the identical
risk in different clothes — a site that should have been changed on the net11 branch and was
not — so the mitigation cannot be "choose the structure that makes the mistake impossible";
neither structure does. The mitigation is the machinery this repo already runs:

* the differential oracle, run per supported version against that version's real runtime,
  catches behavioural divergence regardless of which un-versioned site caused it;
* the pins (`expectedRuntimeVersion`, `dotnet-runtime-src`, the `runtime-version-pin` flake
  check, the `TestEmulatedRuntime` drift test), kept per supported version, catch skew between
  what we claim to emulate and what the devshell actually runs;
* refusal operates at two granularities, and the guarantee must be stated at both. An
  unsupported *major* is refused at CoreLib resolution, naming the majors supported. Within
  a supported major, any servicing or preview build is admitted: within-major internal
  stability is the design's premise, and demanding the exact pinned build at the door would
  make the shipped package unusable on any machine whose servicing release differs from
  ours, for no gain. Exactness is instead enforced where a servicing build could actually
  hurt: the shape classifiers refuse an unrecognised `Setup` signature or stub layout at the
  point of use, naming what was found, so a build that silently changed an internal shape
  stops loudly instead of running wrong. What "supported" *additionally* promises — pinned,
  drift-checked, CI-run — is a claim about which build our validation was measured against,
  not a load-time admission check.

## Design

### The contract key is the CoreLib major version, read from the loaded image

Contract selection must key off the CoreLib that actually loaded, not off a directory name or
a host-supplied claim: binding is by simple name across `GuestConfig.DotnetRuntimeDirs`, and
the flavour machinery already demonstrates that the head of that list can be a runtime pack in
an arbitrary directory. The loaded image carries both facts we need (measured on the devshell
runtime):

| fact | where | value on today's pin | used for |
| --- | --- | --- | --- |
| major version | CoreLib `AssemblyVersion` | `10.0.0.0` | selecting the contract |
| build identity | CoreLib `AssemblyInformationalVersion`, the text before `+` | `10.0.7-servicing.26217.108` | drift validation |

Major granularity is the right key: the CoreLib-to-runtime internal contract churns at major
boundaries and holds within a servicing train (every divergence the spike found is a
net10-vs-net11 fact, not a 10.0.x fact). The informational version's build metadata names an
internal build commit, not the public tag — `RuntimePin.SourceCommit`'s docstring records that
trap — so drift validation drops the build metadata and compares the rest exactly, as text.
That text keeps its prerelease label even on a released servicing build (`-servicing.<build>`),
which `RuntimeInformation.FrameworkDescription` omits for such a build (CoreLib's
`ProductVersionInfoGenerator` strips it only when the build was stabilised); the label is part
of the pin, so the pin names exactly one build.

Note the prerelease wrinkle: a preview CoreLib's informational version carries a prerelease
label, which is exactly what `WoofWare.DotnetRuntimeLocator` PR #206 taught the *locator* to
parse. PawPrint's drift comparison does not parse the text at all, so it cannot regress this
the way `System.Version` would.

### Where the active runtime lives

`EmulatedRuntime` is the support allowlist, a DU with one case per supported major (`Net10`,
later `Net11`), so every per-runtime selection is an exhaustive `match` that stops compiling
when a case is added. `EmulatedRuntime.pin` gives each case the build our validation was
measured against.

The active runtime is not stored anywhere: it is a pure function of the CoreLib the run
loaded. `EmulatedRuntime.ofCoreLib` reads it from that image's `AssemblyVersion` major, and the
image is `BaseClassTypes.Corelib`, which every consumer already holds — `NativeCallContext`
carries `BaseClassTypes`, and the intrinsic gate (`callMethodWithCommitment`),
`AppContextSeed.prepareCall` and `SignalDispatch.trySpawnHandler` all take it. Because the
runtime is computed from the image rather than recorded beside it, the two cannot disagree,
and a fixture that loads some other CoreLib gets that CoreLib's runtime without saying so.

Two placements that store the runtime were considered and not taken:

* **A field on `BaseClassTypes`.** `BaseClassTypes` lives in `WoofWare.PawPrint.Domain`, a
  published package, and `EmulatedRuntime` is a main-library concept; widening Domain's API
  for it is avoidable churn.
* **A field on `IlMachineState`.** Either set once during startup, which checks "read before
  set" and "set only once" only at run time, or supplied at construction, which means moving
  CoreLib discovery ahead of state construction and changing every hand-built test state.
  Both keep a second copy of a fact that is already in the image, with nothing tying the
  copy to it.

Admission happens at CoreLib *resolution*. `Program.beginStartup` walks the entry type's base
chain to the assembly that defines `System.Object`, and classifies that assembly before it
calls `Corelib.getBaseTypes`: a CoreLib of an unsupported major may lack a type `getBaseTypes`
demands, and that failure would hide the real reason. An unsupported major raises
`UnsupportedRuntimeException` naming the CoreLib, the major it states, the majors supported,
and the runtime directories it was resolved along. No guest code has run at that point, and
the refusal surfaces the same way from `beginStartup`, `prepare`, `run` and `runToFirstFork`.
Every consumer of the runtime runs strictly after resolution (no native call, intrinsic check
or host startup call happens without `BaseClassTypes` in hand), so `ofCoreLib` meeting an
unsupported image means a state that did not come through startup, and it fails loudly.

The `TestEmulatedRuntime` drift test starts a trivial guest, takes the CoreLib PawPrint
resolved, and asserts that its build identity equals the pin for its major. The same check
runs against each supported runtime's pinned linux-x64 pack (see "Nix, tests, and CI").

### Two kinds of version-sensitive fact, two mechanisms

The spike's findings sort cleanly into facts about *shape* (visible in the loaded image's
metadata) and facts about *behaviour* (what a native primitive must do, which methods are safe
to interpret). They get different treatments.

**Shape facts: classify the loaded image into a DU of supported shapes.** Where PawPrint
currently hardcodes an internal CoreLib shape, it should instead read the shape from the image
and parse it into a DU whose cases are the shapes we have validated, refusing anything else.
This is parse-don't-validate applied to the host contract. Concretely:

* `AppContext.Setup`'s arity: net10 declares `Setup(char**, char**, int)`, net11 adds an
  `Exception*` out-slot. `AppContextSeed` looks the method up by name, classifies the found
  signature into `SetupShape.ThreeArg | FourArg`, and builds the matching call (the four-arg
  case allocating the out-slot *and reading it back* — the spike left a throwing `Setup`
  silent, which this plan does not inherit). A third shape is refused, naming the signature
  found.
* `RuntimeFieldInfoStub`'s layout: net11 reordered the padding fields and made
  `m_fieldHandle` a bare `IntPtr`. `FieldHandleRegistry` and — the spike's hard-won lesson —
  its *readers* classify the stub's field list from metadata into the two known layouts and
  write/read accordingly. Writer and reader must land in the same commit; the spike measured
  20 test failures when only the writer moved.
* The delegate layout: net11 folds `MulticastDelegate`'s `_invocationList`/`_invocationCount`
  into `Delegate`'s `_helperObject`/`_extraData` and drops `_methodBase` (measured on RC1,
  below). `DelegateRepresentation` and its readers, `MulticastDelegateStub` among them,
  classify the two types' field lists into a `DelegateLayout` and write/read accordingly,
  under the same writer-and-reader rule.

Classification, not open-ended derivation: "write whatever fields exist" would silently adapt
to a future relayout nobody has validated. The DU's arms are the validated set; growth of the
set is a deliberate commit against a pinned image.

**Behaviour facts: rows in tables that are already data.** Where the divergence is in what a
primitive *does* or which route a method takes, the fact cannot be classified from the image's
shape, and lives in a table instead — keyed on a runtime only where nothing finer will do:

* `[Intrinsic]` methods need no version key at all. `IntrinsicBody` (in
  `WoofWare.PawPrint.Semantics`) classifies each by its body, as CoreCLR treats it: its own
  IL, a self-calling placeholder the JIT must expand, or a placeholder the VM substitutes.
  PawPrint runs the first and refuses the other two unless `Intrinsics.call` implements them.
  A body that changed between majors therefore runs as it now stands; only a method that
  became a placeholder needs an implementation.
* `NativeDispatch` keeps the single handler list that `handlersFor` selects; adding
  `Net11` makes that `match` incomplete, which is where the per-runtime composition gets
  decided, against a real image. Nothing composes it earlier, because the measured deltas are
  not at the list's granularity (below). Selection is on
  `EmulatedRuntime.ofCoreLib ctx.BaseClassTypes.Corelib`.
* The PAL conversions beside `WoofWare.PawPrint/Native/` take the runtime and `match` on it
  for the facts that a native library's contract changed under an unchanged call-site shape:
  `PosixSignal` gained `SIGKILL = -11` (so `GetPlatformSignalNumber(-11)` is 9 on net11 and 0
  on net10), and `SystemNative_Kill` passes a raw signo on net11 where net10's shim screens
  the `Interop.Sys.Signals` enum. These arrive with `Net11`; before it every such `match` would
  have one arm. `WoofWare.PosixKernel` itself is not touched: it speaks POSIX, and POSIX did
  not change; only the PAL encodings on the PawPrint side did.

**What the net10 → net11 delta measures as.** Read from the 10.0.7 and 11.0.0-preview.7 images
(osx-arm64 and linux-x64), with 10.0.9 as a servicing control that differs from 10.0.7 in none
of what follows:

* Intrinsic bodies: of the 163 bodies PawPrint's former review list named, 160 are identical
  across the two majors. Three changed, none of them into a placeholder:
  `Span<T>.ToArray` and `ReadOnlySpan<T>.ToArray` (now via `CopyTo`), and
  `Vector<T>.get_IsSupported` (now `call Scalar<T>.get_IsSupported`). Three rows' bodies differ
  between osx-arm64 and linux-x64, on both majors (`BitOperations.PopCount` ×2,
  `TrailingZeroCount(ulong)`).
* Native entry points: of the 296 identities PawPrint's handlers name, 32 are absent on net11
  and 2 changed signature. 29 of the absent ones are QCalls or InternalCalls, which only
  CoreLib can declare, so a handler for one the loaded CoreLib does not declare is never
  reached. They are arms inside `NativeQCall` and `NativeSystemNative`, not entries of the
  handler list; `NativeMonitor` is the only module whose every name is gone. The other 3 are
  `libSystem.Native` exports (`SystemNative_Dup`, `SystemNative_GetFileSystemType`, the
  flavour's OS-thread-id export), which a guest's own `DllImport` can reach: on real net11
  those raise `EntryPointNotFoundException`, and so does each of net11's 15 new exports on real
  net10, whereas PawPrint answers them. That gap is the same class as today's unknown entry
  point, which reaches `failUnimplemented` rather than a guest-catchable exception. Of the two
  signature changes, `RuntimeModule_GetTypes` gains an argument and its handler's arity check
  fails loudly; `SystemNative_Kill` is the PAL fact above.
* PAL encodings: `GetPlatformSignalNumber`, called on both real runtimes over -20..70, differs
  only at -11. `pal_signal.c`'s handled-signal arms and `GetSignalMax` are unchanged, as is every
  other `Interop+*` enum PawPrint transcribes (errno, `SocketEvents`, the socket argument enums,
  `FileAdvice`).

**A placement rule, so the two mechanisms do not leak.** Version conditionals live only in the
contract layer: the shape DUs, the versioned tables, and the dispatch composition. An
`if runtime.Major >= 11` in interpreter core files is a review flag — the question to ask is
which shape or table row it should have been. If sprawl appears in practice, a ratchet in the
spirit of `check-pal-residue.py` (grep for `EmulatedRuntime` outside an allowlisted module
set) is cheap to add; not proposed up front, because the rule may prove self-enforcing at this
codebase's review standard.

## What the spike's findings become

| spike finding | under this design |
| --- | --- |
| locator cannot parse prerelease versions | host-side; fixed upstream (locator PR #206), lands as a package bump |
| `AppContext.Setup` arity | `SetupShape` classification; four-arg arm reads the out-slot back |
| `Setup` skips the four host properties; new `AppContext_TryGetHostPropertyValue` QCall | net11 row in the QCall handler set; `appcontext` skill updated alongside |
| `RuntimeFieldInfoStub` relayout | stub-layout classification, writer and readers together |
| `EqualityComparer<T>.Create` intrinsic | nothing: its own IL runs |
| `FastAllocateString` gained `[Intrinsic]` | **version-agnostic bug, fixed on net10 (stage 2)**: a method PawPrint implements natively must reach that implementation regardless of an `[Intrinsic]` marker; the gate at the `isSafeIntrinsic` check in `IlMachineStateExecution` ran ahead of native dispatch |
| `Monitor.Enter` runs `ObjectHeader.AcquireThinLock`, reading twelve bytes before `RawData.Data` | real interpreter work (object-header addressing); net11-only code path, exercised by the net11 CI leg |
| `Environment.CurrentManagedThreadId` reads a thread-static CoreCLR fills natively | net11 row: seed `ManagedThreadId.t_currentManagedThreadId` at thread creation |
| `SystemNative_FileSystemSupportsLocking` unimplemented | net11 handler row |
| vtable slot placement off by a consistent offset for `Stream`/`MemoryStream`/`Task<T>` | missing base slots; investigate against the net11 image — possibly version-agnostic builder work |
| `TestLinuxCoreLibFlavour` sentinel import absent from net11 CoreLib | replaced by the `Environment.OSVersion` arm, a distinction both majors have (#1508) |
| `PosixSignal` membership / `GetPlatformSignalNumber` answers changed | PAL functions `match` on the runtime, arriving with `Net11` |
| `SystemNative_Kill` takes a raw signo on net11; net10's shim screens `Interop.Sys.Signals` {0, 9, 19} | same; `KillSignalPal`'s screen is net10 behaviour, and on net11 would refuse a raw SIGTERM or Darwin's SIGSTOP (17) with EINVAL |
| `System.Half` and `SZArrayHelper` carry a *type-level* `[Intrinsic]` on net11 (net10: neither type; `SZArrayHelper.GetEnumerator` alone at method level), so all 265 `Half` members and every `IList<T>`/`ICollection<T>` operation on an array are intrinsics | nothing where their IL is their semantics; an `Intrinsics.call` arm for any placeholder among them |
| other IL-bodied methods newly `[Intrinsic]` on net11: `Task.FromResult`, `Task.CompletedTask`, seven `ValueTask`/`ValueTask<T>` members, `Comparer<T>.Create`, `Enum.Equals`, `RuntimeHelpers.SetNextCallAsyncContinuation`/`SetNextCallGenericContext`/`WriteBarrier`, three `AsyncHelpers` and three `StructureMarshaler<T>` members, `Interlocked.And<T>`/`Or<T>`/`CompareExchange(int*, int, int)`, and several hundred SIMD and `System.Numerics` members | the same: an `Intrinsics.call` arm for each placeholder, as the net11 leg reaches them |
| `Span<T>.ToArray`, `ReadOnlySpan<T>.ToArray` and `Vector<T>.get_IsSupported` bodies changed | nothing: the new bodies run as IL (`Scalar<T>.get_IsSupported`, which the last now calls, needs an arm if it is a placeholder) |
| `TestKillSignalPal` reads a type net11 removed | skip the host read on a net11 host (above) |

## What the RC1 frontier measurement adds

Measured on 2026-09-23 against 11.0.0-rc.1.26425.128 (osx-arm64 images, macOS host), after
stages 2–5 had landed. Method: a throwaway branch put RC1's shared framework at the head of every
run's runtime-dir list and hacked in the minimum needed to get past each blocker in turn, so the
counts below are "how many tests this cause blocks once everything before it is out of the way",
not a measure of remaining work (the spike's lesson). Guests were compiled against net10; for the
Guest-suite run, Roslyn and the `RealRuntime` oracle were also pointed at RC1, making it a genuine
net11-against-net11 differential run from the net10 test host.

In order, the frontier after `Net11` is admitted:

1. `AppContext.Setup` has the four-argument shape (as on preview 7).
2. `RuntimeFieldInfoStub` is `m_keepalive: object, m_b: IntPtr, m_c..m_f: object,
   m_fieldHandle: IntPtr` (as on preview 7).
3. **Delegate relayout.** Every guest, `return 7` included, stops in a CoreLib class initialiser
   constructing a `MemberFilter` delegate: "System.MulticastDelegate does not declare expected
   instance field '_invocationCount'". 454 default-suite tests fail on this alone once items 1
   and 2 are passed.
4. `EqualityComparer<T>.Create` refused by the intrinsic gate (spike item 3). With a permissive
   gate, this was the only unreviewed intrinsic the whole default suite reached (580 calls, one
   method); none of the newly-`[Intrinsic]` methods listed above was reached by it.

With throwaway stand-ins for those four, a trivial guest runs: string concatenation, `Dictionary`, `List` and
`typeof(...).Name` all give net10's answers. The default suite then has 100 failures (71 thin
lock, 15 `BitCast`, 2 `TestNativeWaitOneCore`, 12 artefacts of the throwaway override), and the
Guest suite 455 of 1181, grouped by guest location below. Grouping by message is misleading
here: the thin-lock failure surfaces under a different message for each type of locked object.

| finding | tests blocked | under this design |
| --- | --- | --- |
| `MulticastDelegate` declares no fields on net11 (preview 7 and RC1); `Delegate` is `_helperObject, _target, _methodPtr, _methodPtrAux, _extraData` where net10 has `_target, _methodBase, _methodPtr, _methodPtrAux` on `Delegate` and `_invocationList`, `_invocationCount` on `MulticastDelegate`. Upstream, `_helperObject` holds the invocation list (now a `Wrapper[]`) or a `MethodInfo`/`DynamicMethod`, and `_extraData` the invocation count, a `MethodDesc*`, or `UnmanagedMarker` | every guest | shape fact: a `DelegateLayout` classification with writers (`DelegateRepresentation`) and readers (`MulticastDelegateStub`) together, net10 arm first on `main`, net11 arm with `Net11` |
| `ObjectHeader.AcquireThinLock` (above) | 71 default, 401 Guest (88%) | unchanged; the frontier after startup |
| `Unsafe.BitCast` refused in `MemoryExtensions.MinMaxInteger` ("value type containing runtime pointers") | 15 default, 9 Guest | interpreter byte-model work, reached by a net11 code path; the mechanism is version-agnostic |
| `SystemNative_AlignedAlloc` unimplemented (`Interop.Sys.AlignedAlloc`) | 20 Guest | a handler row; `NativeMemory.AlignedAlloc` reaches the same export on net10 [inferred, not checked], so it may land version-agnostically |
| `MarshalNative_HasLayout` QCall unimplemented | 13 Guest | QCall handler row |
| `System.GCMemoryInfoData` has no `_generationInfo0` (reached from `GC.GetMemoryInfo`) | 3 Guest | shape fact; the new layout was not characterised |
| `RuntimeAssembly.GetIsCollectible` InternalCall; `Delegate_GetMethodDesc` QCall | 1 each | handler rows; the second belongs with the delegate layout, since net11's `Delegate.MethodDesc` calls it only when `_extraData` is zero |
| `TestNativeWaitOneCore` finds no `QCall`/`WaitHandle_WaitOneCore` import on `WaitHandle` (RC1 still declares a static `WaitOneCore(IntPtr, int, bool)`; how it binds was not examined) | 2 default | a per-runtime fact about the fixture's lookup |
| differential mismatches: `EnvironmentCurrentManagedThreadId.cs` (the thread-static seed above) and `LockHeldByOtherThread.cs`, both PawPrint 1 against real 0 | 1 each | the first is the existing row; the second is not diagnosed. Both are wrong answers rather than refusals, which only the differential leg catches |

**Some expectations are themselves per-runtime.** Two cases fail because *real* net11 behaves
differently from real net10, not because of PawPrint. `ConvFloatSaturating.cs` exits 40 on real
RC1 where its registration declares 0. `DelegateBindOpenGenericDefinitionStaticVirtual.cs`
(parked) binds on real net10, and its parking comment records that; on real RC1 the same bind
throws `InvalidOperationException` ("Could not execute the method because either the method
itself or the containing type is not fully instantiated"). A case's expected outcome therefore
has to be recorded per runtime, alongside its parking (below), unless the case asserts only a
fact that holds on every supported major.

## Nix, tests, and CI

### The test host runs the oldest supported major; newer majors are guest-only

Every executable project (the test projects, `WoofWare.PawPrint.App`, `IlDump`, `Performance`,
the playgrounds) targets the **oldest** supported major, today `net10.0`. A newer major is only
ever a framework a guest runs on. When the window moves, net10 retires and the host moves to
net11, which by then is a released major; the host never runs a preview runtime.

This works because framework assemblies bind forward. Measured on RC1: a guest compiled against
net10 runs unchanged on real net11 with a runtimeconfig naming net11, and a net10 test host ran
the whole net11 differential run (PawPrint on RC1's CoreLib, the oracle on RC1 through RC1's
own muxer; the combined muxer below was separately measured to run both a net10 and a net11
guest). So guests keep compiling against the host's own framework, and
`PersistedAssemblyBuilder` images keep naming the host's CoreLib, on every leg. The CS1705 skew
that arises when a guest is compiled against a *newer* CoreLib than a leg runs cannot occur.

In-process host oracles speak for the host's framework, which is the oldest supported major on
every leg. The census found about 150 such sites in about 60 files, including 7 `DllImport`s into
`libSystem.Native` and private reflection into CoreLib. They answer the same thing on every leg,
and they pair with the model evaluated at the host's runtime: `TestPosixSignalPal` gains
`hostRuntime ()` beside `hostNumbering ()`, classifying the test host's own CoreLib through
`EmulatedRuntime`, and model-only assertions iterate `EmulatedRuntime.supported`. A newer
runtime's model rows have no in-process oracle. Where they need one (the PAL rows, e.g.
`GetPlatformSignalNumber(-11)` being 9 on net11), it is an out-of-process probe run on that
framework through the combined muxer: the `RealRuntime` shape, one small probe program per
table. `TestKillSignalPal` reads `Interop+Sys+Signals`, which net11 removed, from the host's
`System.Diagnostics.Process`; that read must change when net10 retires and the host becomes net11.

The cost is that a guest cannot use an API that exists only in a newer major. Nothing needs that
today. If a case ever does, it opts into compiling against its leg's framework, and only then
does Roslyn need to know which framework is under test.

Two alternatives were rejected:

* **Move the executables to the newest supported framework.** The whole solution would then be
  built by the preview SDK. Measured: with SDKs 10 and 11 both in one install, `dotnet --version`
  is the 11 preview whichever order they are combined in, so the preview F# compiler, MSBuild and
  analysers would build everything, under `TreatWarningsAsErrors`. The `build-nix` check would run
  on a preview runtime too, so preview instability would reach the net10 signal we most want
  stable. And every in-process oracle would stop speaking for the older leg, so each would need
  reclassifying or moving out of process, while Roslyn's 310 compilations and 22
  `PersistedAssemblyBuilder` sites would need a framework-under-test to avoid CS1705.
* **Build the test host once per leg**, targeting each leg's framework. Every host-framework
  default would then be right by construction on each leg, and the in-process oracles would cover
  the newer runtime's rows without probes. But the newer leg needs the preview SDK and a second
  build, and `#if NET11_0_OR_GREATER` in tests would be a new place for version conditionals to
  hide from the placement rule.

### Per-version pin sets

`flake.nix` holds one attrset per supported runtime:

* the package;
* `expectedRuntimeVersion`;
* the `dotnet-runtime-src` sparse checkout (rev and hash);
* the linux-x64 framework pack (and hash);
* the TFM.

The `runtime-version-pin` check and the pack's TFM check are generated per runtime.

The oldest major supplies the SDK. A newer major supplies only its runtime, which is all a
guest-only framework needs.

The net11 set pins **11.0.0-rc.1.26425.128**:

* the dotnet/runtime tag `v11.0.0-rc.1.26425.128` is commit
  `ab19415702aa8139d5369e47c73edb47343c34ad`;
* `microsoft.netcore.app.runtime.linux-x64` is published at that version;
* both the SDK and the runtime are in cache.nixos.org for aarch64-darwin and x86_64-linux.

That was the newest net11 nixpkgs carried on 2026-09-23. No RC2 was tagged yet.

It comes from **a second nixpkgs input, used only for the net11 set**. The net10 set stays on the
existing input, byte-identical. Bumping net11 (RC2, then GA) is then the `sync-dotnet-runtime`
motion against the second input alone. The two inputs collapse into one when one nixpkgs rev
carries both pins we want, and at the latest when net10 retires.

Two alternatives were rejected:

* **One input, bumped to a rev that carries net11.** Measured: nixpkgs-unstable on 2026-09-23
  moves net10 from 10.0.7 to 10.0.12, and the SDK from 10.0.203 to 10.0.401, a new feature band
  with a new F# compiler and new analysers. That drags a servicing bump and a toolchain change
  into this stage, and it couples the two pins' cadences for as long as both exist.
* **Wait for net11 GA before adding any pin.** This avoids carrying RC bumps, but it does not
  avoid any frontier work: everything measured on RC1 was also present on preview 7.

### The devshell

The devshell's `dotnet` is `dotnetCorePackages.combinePackages [ sdk_10_0 runtime_11_0 ]`: one
muxer, with only the oldest major's SDK. Measured on aarch64-darwin:

* it adds 130 MB to the closure (1.37 → 1.50 GB), against 1.42 GB for combining the full net11 SDK;
* `dotnet --version` stays on SDK 10;
* the muxer runs both a net10 guest and a net11 guest.

The muxer resolves the highest `hostfxr` present, so net10 apps start through RC1's `hostfxr`
(measured, with `COREHOST_TRACE`). That is accepted: `hostfxr` is the version-agnostic half of the
host that selects a framework directory, exactly the half this plan's design treats as shared.
If it ever misbehaves, `combinePackages` can link `host/` from the oldest major alone.

The per-runtime environment variables carry the major in their names, and **no unsuffixed
name exists**:

* `DOTNET_RUNTIME_SRC_NET10` and `DOTNET_RUNTIME_SRC_NET11`;
* `DOTNET_LINUX_FRAMEWORK_DIR_NET10` and `DOTNET_LINUX_FRAMEWORK_DIR_NET11`;
* `DOTNET_FRAMEWORK_DIR_NET11`, the net11 shared-framework directory inside the combined install.

Tests read them only through the test-side `FrameworkUnderTest` module (below), keyed on
`EmulatedRuntime` by an exhaustive `match`, so adding `Net12` does not compile until its
variables are named. `AGENTS.md` and the `sync-dotnet-runtime` skill change in the same PR.

Two alternatives were rejected:

* **Keep the unsuffixed names, pointing at the newest.** This silently retargets every existing
  reader. The Linux-flavour tests, whose only reader is `LinuxCoreLibFlavour.fs`, would start
  testing the net11 pack. `TestEmulatedRuntime` would still pass, because it compares each image
  with the pin for that image's own major. So nothing fails, and net10's flavour coverage simply
  stops.
* **Keep the unsuffixed names, pointing at the host's major.** Their meaning then changes every
  time the window moves.

### Selecting the framework under test

A census of the test projects found **four independent host-framework locators**. They agree
today only because the devshell holds a single framework:

* `RuntimeEnvironment.GetRuntimeDirectory()`, for Roslyn's references;
* `typeof<obj>.Assembly.Location`, for the oracle's framework and for fixtures that parse CoreLib;
* `DotnetRuntime.SelectForDll` on the test assembly, which reads the test assembly's runtimeconfig
  and the `dotnet` on `PATH`;
* the `dotnet` on `PATH`, which the F# case publish uses.

Which runtime a leg tests is one fact, owned by a test-side `FrameworkUnderTest` module:

* `PAWPRINT_TEST_RUNTIME` is parsed into `EmulatedRuntime` at the boundary.
* Unset means `Net10`, spelled out in that one place, and for a non-Nix checkout that is the host's
  own framework.
* An unrecognised value, or a runtime whose framework-directory variable is unset, **fails the run**
  rather than falling back to the host.

It has exactly two consumers:

* **the interpreter's runtime-dir lists.** `FrameworkUnderTest.runtimeDirs ()` replaces the 82
  `DotnetRuntime.SelectForDll` calls in 72 files;
* **the `RealRuntime` oracle's framework**, a single binding behind all 45 oracle calls. That binding
  determines the framework directory, the version its generated runtimeconfig names, and the muxer.

`TestFSharpPureCases` publishes framework-dependent rather than `--self-contained`, once, and
runs the publish on the selected framework with a generated runtimeconfig. A self-contained net10
publish would put net10's CoreLib at the head of PawPrint's search list on every leg, so the net11
leg would silently run net10.

Not consumers, by the test-host rule above:

* Roslyn's references;
* `PersistedAssemblyBuilder`'s core assembly;
* the 121 `typeof<obj>.Assembly.Location` reads in 109 files that hand PawPrint's reader an image
  to parse. Those move behind a named `HostImage` binding, so that "the host's CoreLib as a test
  fixture" is visible as such. The same-image oracles among them *require* that the image PawPrint
  reads is the host's.

The fixtures that assert a *shape* of CoreLib are different: they are claims about a runtime's
images, so they read **the selected runtime's** images rather than the host's, and they park like
every other test on that leg:

* `TestSetupShape`;
* `TestRuntimeFieldInfoStubLayout`;
* `TestNativeRuntimeMethodHandle`;
* `TestSafeIntrinsicFingerprints`' audit;
* `TestEmulatedRuntime`'s drift check, which compares the leg's framework and linux pack against
  that runtime's pin.

A leg never audits another runtime's images. If it did, net11's unreviewed intrinsic bodies
(stage 7) would fail the net10 leg, and no net11 park could stop that. As today, a linux pack whose
variable is unset is skipped, so a non-Nix checkout and `build-nix` still run the net10 leg.

Three machine checks keep the selection honest:

* **Precondition.** `runtimeDirs` asserts that the CoreLib at the head of the list it returns
  classifies to the selected runtime.
* **Ratchet.** A test in the style of `TestFixturesDeclareParallelism` fails if `SelectForDll` or
  `GetRuntimeDirectory` appears outside `FrameworkUnderTest`. It also fails on a raw
  `typeof<obj>.Assembly.Location` outside `HostImage`.
* **Postcondition.** The harnesses that wrap `Program.run` (`TestHarness`, `CrossAssemblyHarness`,
  `FabricatedGuest`, the pure and impure runners) assert that the run's `BaseClassTypes.Corelib`
  classifies to the selected runtime. This catches what the ratchet cannot see: a directory
  prepended ahead of the selection that happens to contain a CoreLib, and reuse of an existing
  path binding.

Three alternatives were rejected:

* **An environment variable naming a framework directory, defaulting to the host's**, the shape
  of the flavour override. Here the default is the dangerous case: a missing or misspelled variable
  gives a green net11 leg that tested net10.
* **One test project per runtime**, sharing sources. It doubles the build to isolate a
  selection that two bindings can hold.
* **The runtime as part of every test's identity**, with legs as `--filter` selections. It
  changes test identity across the ~72 guest-running files, and it doubles local suite time
  unless the newer runtime is filtered out.

### Parking and expected outcomes are per runtime, in both suite halves

Guest-running tests are not confined to the seven `Guest` fixtures: 72 files start guests, and
the RC1 measurement broke 454 *default*-suite tests on the delegate layout alone. So both suite
halves are per-runtime signals, and parking reaches both.

A fixture's parked set is a total function of the runtime:
`parked : EmulatedRuntime -> Map<case, reason>`. It is an exhaustive `match`, with the cases parked
everywhere folded into every arm. The same function carries per-runtime expected outcomes, for
cases like the two above whose real-runtime answer differs between majors.

* A park on an unsupported runtime is unrepresentable, because the DU is the supported set.
* Adding `Net12` stops compiling until its parks and expectations are decided.
* Retiring `Net10` deletes an arm.

Each parked case keeps the existing comment discipline: why it is parked, the condition for
un-parking it, and that its behaviour was verified on real .NET. Non-guest fixtures park with
`ParkedOn.check runtimes reason`, which calls `Assert.Ignore` when the leg's runtime is in
`runtimes`. The explicit "unimplemented" test sources stay, per runtime, so stale parks can be
swept.

Two alternatives were rejected:

* **Parks recorded in a header inside each guest source file.** These only reach file-based
  guests, and they are typed when parsed rather than at compile time.
* **A list of (case, non-empty set of runtimes).** It needs a hand-rolled non-empty set to rule out
  an empty park, and a newly added runtime is not forced to decide anything.

### CI

The test job becomes a **matrix over the supported runtimes**. Each leg builds the same binaries
and runs both halves (the default suite, then the `Guest` fixtures) with `PAWPRINT_TEST_RUNTIME`
set. The critical path stays one leg long.

The price, inferred from the 2026-09-23 CI time profile (the `build` job was 12–14 minutes, the
default suite about 2.5 minutes, the Guest suite 6–7.5 minutes):

* one more job per run for each extra runtime, or one per runtime and shard once the Guest
  fixtures are sharded;
* about three minutes of repeated setup per extra job (the Nix closure, restore and build).

The repository is public, so this costs runner concurrency rather than billed minutes.
`build-nix` stays a single job: it checks the package on the host's major, and it is not a leg.

A PR is green only when every leg passes. The alternative, scheduled runs for the non-newest
version, re-creates the silent drift this plan exists to prevent.

Two alternatives were rejected:

* **The newer leg as serial steps in the existing `build` job.** This adds no job, but it adds
  roughly nine minutes to the critical path.
* **Folding the newer runtime into the planned Guest sharding.** This depends on sharding that
  has not landed, and it still leaves the default suite serial.

The window is capped at **two majors**: the one we are moving from and the one we are moving to,
which matches how the window will actually be used. The support window is a promise with a
per-version carrying cost: pins, drift checks, CI jobs, and parked-case bookkeeping. Dropping a
version is a deliberate cleanup that deletes:

* its pin set;
* its table rows;
* its shape-DU arms, where an arm is left single-cased;
* its parking arms;
* its matrix leg.

Under option C that is a table cleanup, not a branch funeral.

## Staging

Ordered so `main` stays green on net10 throughout, and each PR is small and independently
reviewable. Items 1–3 are pure refactorings with no behaviour change on net10; the
mutation-testing skill applies to each table/classifier they introduce.

1. **(landed)** `WoofWare.DotnetRuntimeLocator` PR #206 merges and releases; PawPrint bumps
   the package. Prerequisite for anything net11 touching a real runtime directory.
2. **Fix the intrinsic-gate-versus-native-dispatch ordering on net10** (the
   `FastAllocateString` finding). Version-agnostic; testable today.
3. **De-singleton `EmulatedRuntime`.** `EmulatedRuntime` becomes the allowlist DU (of one,
   at this stage), and the active runtime is read from the resolved CoreLib; CoreLib
   resolution in `Program.beginStartup` refuses unknown majors with the supported set named.
   The drift test checks the loaded CoreLib's build against the pin for its major.
   `NativeDispatch` selects its handler list by the CoreLib's runtime, with one list to select
   from.
4. **Shape classifications on net10.** `SetupShape` (three-arg arm live, four-arg arm refused
   as unrecognised until stage 6 adds it) and the `RuntimeFieldInfoStub` layout classifier
   (current layout live), each with its readers. The fabricated-image machinery
   (`FabricatedGuest`, and the fabricated-image-load-oracle practice) can exercise the
   refusal arms without a net11 CoreLib.
5. **Intrinsic body classification.** `IntrinsicBody` decides from the body alone whether an
   `[Intrinsic]` method's IL may run, so it needs no version key, and it is the whole of this
   stage: `NativeDispatch` composition and the versioned PAL
   rows are decided when `Net11` makes their `match`es incomplete (stage 6), against a real
   image.
6. **Nix, tests and CI for a second runtime**, as designed above, in PRs that each keep `main`
   green. The first five are version-agnostic and land on net10 alone:
   * **P1 `FrameworkUnderTest`, the runtime-dir half.** The module, with `Net10` its only value;
     the 82 `SelectForDll` calls and the `RealRuntime` framework binding routed through it; the
     precondition, the ratchet, and the harness postcondition. Behaviour-identical on net10.
   * **P2 `HostImage`.** The fixture reads of the host's CoreLib go through one named binding,
     and the shape-asserting fixtures read the selected runtime's images.
   * **P3 the F# case publish becomes framework-dependent**, run on the selected framework.
   * **P4 parking keyed on `EmulatedRuntime`**, with per-runtime expected outcomes, across the
     `Guest` fixtures, and `ParkedOn.check` for the rest; every current park is `everywhere`.
   * **P5 the `DelegateLayout` classification**, net10 arm live, writers and readers together, the
     refusal arm exercised with a fabricated CoreLib as in stage 4.

   Then **P6, net11 arrives**, with:
   * the `Net11` case and pin, the net11 pin set on its own nixpkgs input, and the per-runtime
     checks and environment variables;
   * the combined devshell and the matrix leg;
   * the `NativeDispatch` composition decided against the real image;
   * the startup arms: `SetupShape.FourArg` reading its out-slot back, the stub layout's
     bare-handle arm and its readers, the delegate layout's net11 arm, the
     `EqualityComparer<T>.Create` row, and the `AppContext_TryGetHostPropertyValue` QCall (RC1
     declares `AppContext.TryGetHostPropertyValue(string, StringHandleOnStack)`);
   * every residual net11 failure parked through P4.

   **P6 is never opened without the startup arms.** Without them one cause blocks every test
   that starts a guest (454 default-suite tests on RC1 from the delegate layout alone), and
   parking all of them on that one reason would be parking by exclusion in all but name. There
   is no separate servicing-bump PR: the second nixpkgs input leaves the net10 set untouched.
7. **Walk the net11 blockers one at a time**, in the measured frontier order where known:
   * thin-lock addressing;
   * the `Unsafe.BitCast` refusal in `MemoryExtensions.MinMaxInteger`;
   * `SystemNative_AlignedAlloc`, `MarshalNative_HasLayout`, `RuntimeAssembly.GetIsCollectible`
     and `Delegate_GetMethodDesc`;
   * the `GCMemoryInfoData` layout;
   * `TestNativeWaitOneCore`'s per-runtime lookup;
   * the thread-static seed, and the `LockHeldByOtherThread.cs` mismatch;
   * the other new QCalls, and the vtable slots;
   * any placeholders among the `Half` and `SZArrayHelper` type-level `[Intrinsic]`s and the
     other newly-`[Intrinsic]` bodies;
   * the two PAL facts, with their out-of-process net11 probes.

   Each PR un-parks its cases
   (guest or non-guest) on the net11 leg, exactly the incremental discipline `AGENTS.md`
   already prescribes for frontier work.
8. **When the window moves** (net12 preview lands in nixpkgs): add its pin set and rows;
   retire net10's in one cleanup PR.

Stages 2–5 are useful even if multi-version support were later abandoned: each replaces a
hardcoded assumption with a classified fact, on the version we already run.

## Costs and risks, honestly

* **CI carries one more matrix leg** for the window's duration: another job per run, each
  repeating the setup and the full test time, on a critical path that stays one leg long. This
  is the real price of the word "supported", and it is bounded by capping the window at two.
* **A missed version-sensitive site** applies the wrong major's behaviour silently. Caught by
  the per-version differential legs; the placement rule keeps the sites enumerable so review
  can ask "which table should this be in".
* **Shape classification can mask a semantic change** that kept the old shape: a method whose
  signature did not change but whose contract did would classify as the familiar arm. The
  differential oracle is again the net; the tables are the fix once caught.
* **Per-version parking bookkeeping** grows the `unimplemented` structure and its comments.
  Accepted; the alternative (a green net11 leg by exclusion) hides exactly what we want
  measured.
* **nixpkgs lag** decides which net11 build we can pin (RC1 on 2026-09-23). The pin-set
  structure does not care which prerelease it names; bumping within net11 is the existing
  `sync-dotnet-runtime` motion, against the net11 input alone.
* **Each prerelease can move the frontier.** The delegate relayout was already present on
  preview 7 and was hidden behind an earlier blocker there, so a pin bump is re-measured,
  not assumed to be a no-op.

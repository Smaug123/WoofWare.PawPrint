# Plan: support multiple emulated runtimes in one PawPrint

Status: **plan, not yet started**. Companion to `2026-09-09-net11-spike.md`, which measured
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
runs against the pinned linux-x64 pack when `$DOTNET_LINUX_FRAMEWORK_DIR` is set.

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

Classification, not open-ended derivation: "write whatever fields exist" would silently adapt
to a future relayout nobody has validated. The DU's arms are the validated set; growth of the
set is a deliberate commit against a pinned image.

**Behaviour facts: rows in tables that are already data.** Where the divergence is in what a
primitive *does* or which route a method takes, the fact cannot be classified from the image's
shape, and lives in a table instead — keyed on a runtime only where nothing finer will do:

* `IntrinsicMethodKeys`' `safeIntrinsics` rows name no version at all. Each row carries the
  fingerprints of the IL bodies its review covered (`IlBodyFingerprint`: a hash of the body's
  signature, locals, instructions and exception regions, every token rendered by what it
  names), and the `[Intrinsic]` gate interprets a listed method only when its body is one of
  them; any other body is refused, naming the fingerprint found. What a row's verdict depends
  on is the body, so that is what it keys on. A row for a method that is not `[Intrinsic]` on
  some runtime is never consulted there, so a net11-only row is simply added; a body that
  changed between majors is refused on the new one until someone re-reviews it and appends
  its fingerprint. `TestSafeIntrinsicFingerprints` audits the table against the CoreLib the
  suite runs on and the pinned linux-x64 CoreLib, printing any unreviewed body a row names.
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

* `safeIntrinsics`: of 163 rows, 160 name bodies identical across the two majors. Three name
  bodies that changed, and each needs re-review, not a version switch:
  `Span<T>.ToArray` and `ReadOnlySpan<T>.ToArray` (now via `CopyTo`), and
  `Vector<T>.get_IsSupported` (now `call Scalar<T>.get_IsSupported`). Three rows' bodies differ
  between osx-arm64 and linux-x64, on both majors (`BitOperations.PopCount` ×2,
  `TrailingZeroCount(ulong)`), which is why a row can list more than one fingerprint.
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
| `EqualityComparer<T>.Create` intrinsic | an ordinary `safeIntrinsics` row with its body's fingerprint; the method does not exist on net10, so the row is never consulted there |
| `FastAllocateString` gained `[Intrinsic]` | **version-agnostic bug, fix now on net10**: a method PawPrint implements natively must reach that implementation regardless of an `[Intrinsic]` marker; the gate at the `isSafeIntrinsic` check in `IlMachineStateExecution` currently runs ahead of native dispatch |
| `Monitor.Enter` runs `ObjectHeader.AcquireThinLock`, reading twelve bytes before `RawData.Data` | real interpreter work (object-header addressing); net11-only code path, exercised by the net11 CI leg |
| `Environment.CurrentManagedThreadId` reads a thread-static CoreCLR fills natively | net11 row: seed `ManagedThreadId.t_currentManagedThreadId` at thread creation |
| `SystemNative_FileSystemSupportsLocking` unimplemented | net11 handler row |
| vtable slot placement off by a consistent offset for `Stream`/`MemoryStream`/`Task<T>` | missing base slots; investigate against the net11 image — possibly version-agnostic builder work |
| `TestLinuxCoreLibFlavour` sentinel import absent from net11 CoreLib | replace the sentinel with a distinction that exists in both supported majors |
| `PosixSignal` membership / `GetPlatformSignalNumber` answers changed | PAL functions `match` on the runtime, arriving with `Net11` |
| `SystemNative_Kill` takes a raw signo on net11; net10's shim screens `Interop.Sys.Signals` {0, 9, 19} | same; `KillSignalPal`'s screen is net10 behaviour, and on net11 would refuse a raw SIGTERM or Darwin's SIGSTOP (17) with EINVAL |
| `System.Half` and `SZArrayHelper` carry a *type-level* `[Intrinsic]` on net11 (net10: neither type; `SZArrayHelper.GetEnumerator` alone at method level), so all 265 `Half` members and every `IList<T>`/`ICollection<T>` operation on an array reach the gate | review and fingerprint those bodies as rows, or implement them in `Intrinsics.call`; until then the gate refuses them on net11 |
| other IL-bodied methods newly `[Intrinsic]` on net11: `Task.FromResult`, `Task.CompletedTask`, seven `ValueTask`/`ValueTask<T>` members, `Comparer<T>.Create`, `Enum.Equals`, `RuntimeHelpers.SetNextCallAsyncContinuation`/`SetNextCallGenericContext`/`WriteBarrier`, three `AsyncHelpers` and three `StructureMarshaler<T>` members, `Interlocked.And<T>`/`Or<T>`/`CompareExchange(int*, int, int)`, and several hundred SIMD and `System.Numerics` members | the same: a row or an `Intrinsics.call` arm each, as the net11 leg reaches them |
| `Span<T>.ToArray`, `ReadOnlySpan<T>.ToArray` and `Vector<T>.get_IsSupported` bodies changed | the gate refuses them on net11 by fingerprint; re-review, then append the net11 fingerprints (`Scalar<T>.get_IsSupported`, which the last now calls, may need its own row) |
| `TestKillSignalPal` reads a type net11 removed | skip the host read on a net11 host (above) |

## Nix, tests, and CI

### Per-version pin sets

`flake.nix` grows one attrset per supported version, each holding what today exists once:
SDK/runtime packages, `expectedRuntimeVersion`, the `dotnet-runtime-src` sparse checkout (rev
+ hash), the linux framework pack (+ hash), and the TFM. The `runtime-version-pin` check and
the linux-pack TFM check run per version. `$DOTNET_RUNTIME_SRC` and
`$DOTNET_LINUX_FRAMEWORK_DIR` become per-version variables (suffixed by major), with the
unsuffixed names kept pointing at the newest so existing habits and docs stay true.

The devshell's `dotnet` combines the pinned SDKs/runtimes with
`dotnetCorePackages.combinePackages`, which is nixpkgs' mechanism for a single muxer with
multiple shared frameworks — this is what lets one `dotnet test` host launch real-runtime
oracle processes for either version.

Closure cost: one more SDK, runtime, source checkout, and framework pack per version, all
prebuilt in nixpkgs' cache for the versions in question (measured for net11 preview 7 in the
spike).

### Selecting the framework under test

Today every path picks the test host's own framework: `Roslyn.metadataReferences` compiles
guests against `RuntimeEnvironment.GetRuntimeDirectory()`, and the harnesses build
`DotnetRuntimeDirs` with `DotnetRuntime.SelectForDll` on a test assembly. That becomes one
fact — "the framework under test" — resolved in one place and consulted by:

* Roslyn's metadata references (guests compile against the selected framework's assemblies);
* `GuestConfig.DotnetRuntimeDirs` (the interpreter loads the same assemblies);
* the `RealRuntime` oracle (the guest's generated `runtimeconfig.json` requests the selected
  version; the combined muxer resolves it);
* `TestFSharpPureCases`' publish. This path bypasses all three of the above: `publishOnce`
  publishes `--self-contained`, the interpreter's search list starts with the publish
  directory, and the oracle runs the bundled apphost. It needs one publish per supported
  version — selected TFM, pinned runtime version, separate output directories — after which
  interpreter and oracle both follow the publish with no further selection;
* the fixtures that read `typeof<obj>.Assembly.Location` to obtain a CoreLib image for
  PawPrint's own reader. Those are parsing whatever the *test host* runs on; under selection
  they should read the selected framework's CoreLib from disk instead;
* the fabricated-guest emitters. `TestFabricatedCpblk` and its siblings construct
  `PersistedAssemblyBuilder` with `typeof<obj>.Assembly` as the core assembly, so the emitted
  image references the *host's* CoreLib version — and `FabricatedGuest.runOnBoth` then
  compiles a Roslyn driver against it, which on the older leg would reference an assembly
  demanding a newer CoreLib than the driver's own (review reproduced this as CS1705 before
  either runtime runs the guest). The emitters must take their core assembly from the
  selected framework — `PersistedAssemblyBuilder` accepts a `MetadataLoadContext`-loaded
  core assembly for exactly this — so the stage-6 census covers Reflection.Emit core-assembly
  references alongside `DllImport` and host-reflection oracles.

### In-process host oracles cannot be selected

A separate class of fixture compares PawPrint against the host CLR *in process* —
`TestVirtualMethodSlots` reflects onto the host's own `RuntimeMethodHandle.GetSlot`, and
`TestPosixSignalPal` P/Invokes the host's `libSystem.Native` — and no environment variable
can change what those answer: they speak for the framework the test executable itself runs
on, which after stage 6 is the newest supported version. On the leg whose selected version is
not the host's, each such fixture must do one of three things, chosen per fixture when the
census (a stage-6 task: sweep the test project for `DllImport` and host-reflection oracles)
classifies it:

* assert only facts that hold across the supported majors (the differential-tests-assert-only-
  cross-runtime-facts discipline, applied across versions rather than flavours);
* run only on the leg whose version matches the host, ignoring itself elsewhere;
* move the oracle out of process, launched on the selected framework through the combined
  muxer — the `RealRuntime` shape.

Whichever is chosen, the model side of the comparison must use the versioned rows for the
version the oracle actually answers for — pairing net10 table rows with a net11 host answer
is precisely the mismatch the spike measured for `GetPlatformSignalNumber`. For the two PAL
fixtures the pairing is the one they already use for flavour: `TestPosixSignalPal` evaluates
its model at `hostNumbering ()`, and gains `hostRuntime ()` beside it, which classifies the test
host's own CoreLib through `EmulatedRuntime`. Host-oracle assertions evaluate the model at the
host's runtime and `Assert.Ignore` when that major is unsupported; model-only assertions
iterate `EmulatedRuntime.supported`, as they iterate every numbering today.
`TestKillSignalPal` reads `Interop+Sys+Signals` from the host's `System.Diagnostics.Process`,
a type net11 no longer has, so on a net11 host it must skip that read rather than throw.

The selection mechanism follows the existing flavour precedent: an environment variable naming
the framework directory, defaulting to the host's own when unset so a non-Nix checkout still
passes. `DOTNET_LINUX_FRAMEWORK_DIR` + `runtimeDirsPreferringLinux` is this exact shape
already; the flavour override becomes one instance of the general "select a framework pack"
mechanism rather than a sibling of it.

Compiling guests against the selected framework, not the host's, matters for the oracle: an
`extracted-shared-code`-style skew where the two runtimes see differently-compiled images
would break the differential claim. One compilation per (guest, version), both consumers of
each image identical — the existing one-image-both-runtimes discipline, now indexed by
version.

### Parking becomes version-aware, in both suite halves

`TestPureCases.unimplemented` is a set of file names. Under two versions, a guest can pass on
net10 and be blocked on net11 (everything in the table above starts out that way), so parking
must record *which versions* a case is parked for. Smallest sufficient change: the set becomes
a list of (file, parked-on-versions) with today's entries parked everywhere, and the fixture
consults the active version. The parking comments' discipline (why parked, un-park condition,
verified-on-real-.NET) carries over unchanged per version.

That set only reaches `TestPureCases`. All six guest-running fixtures (the `Guest`-category
list in `AGENTS.md`) maintain their own case registrations, and the version dimension applies
to each: `TestScheduleFork`, for instance, runs `InvertedMonitorDeadlock.cs`, whose `lock`
statement is blocked on the stage-7 thin-lock work, so its cases need per-version parking
exactly as `TestPureCases`' do — likewise `TestImpureCases`' explicit registrations and
`TestFSharpPureCases`' own list. A *non-guest* fixture blocked on one version — the
flavour sentinel in `TestLinuxCoreLibFlavour` is the known instance, and the host-oracle
census above may find more — is parked with a per-version `Assert.Ignore`, the mechanism the
flavour tests already use when `DOTNET_LINUX_FRAMEWORK_DIR` is unset, carrying the same
why/un-park-when comment discipline. Without this, a net11 leg cannot come up green at all:
the default suite is a per-version signal too, and excluding it (rather than parking within
it) would hide exactly what we want measured.

### CI

Each supported version gets both test legs (default suite and Guest fixtures), because the
non-guest suite exercises CoreLib loading constantly — the spike watched 405 of its tests fail
on a single startup-path divergence, so it is absolutely a per-version signal. That roughly
doubles CI's test time while the window holds two versions.

Two consequences to accept explicitly:

* `AGENTS.md`'s "nothing is excluded from a PR" policy extends to the new legs: a PR is not
  green until both versions pass. The alternative — scheduled runs for the non-newest version
  — re-creates the silent-drift problem this plan exists to prevent, so it is not proposed.
* The support window is a promise with a per-version carrying cost (pins, drift checks, CI
  minutes, parked-case bookkeeping). Cap it at **two majors** — the one we are moving from and
  the one we are moving to, matching how the window will actually be used. Dropping a version
  is then a deliberate cleanup: delete its pin set, its table rows, its shape-DU arms if now
  single-cased, and its CI legs. Under option C that is a table cleanup, not a branch funeral.

## Staging

Ordered so `main` stays green on net10 throughout, and each PR is small and independently
reviewable. Items 1–3 are pure refactorings with no behaviour change on net10; the
mutation-testing skill applies to each table/classifier they introduce.

1. **(in flight)** `WoofWare.DotnetRuntimeLocator` PR #206 merges and releases; PawPrint bumps
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
5. **Fingerprinted intrinsic rows.** Each `safeIntrinsics` row lists the fingerprints of the IL
   bodies it was reviewed against, and the gate refuses any other body. This needs no version
   key, so it is the whole of this stage: `NativeDispatch` composition and the versioned PAL
   rows are decided when `Net11` makes their `match`es incomplete (stage 6), against a real
   image.
6. **The flake and CI grow the net11 pin set** (preview or RC, whichever nixpkgs then
   carries), the combined devshell, the framework-under-test selection (including the
   per-version `TestFSharpPureCases` publish), and version-aware parking in both suite
   halves: every net11-blocked guest parked in the versioned `unimplemented` set, every
   net11-blocked non-guest fixture parked with a per-version `Assert.Ignore` — the flavour
   sentinel among them if its replacement has not landed first. This stage also runs the
   host-oracle census and classifies each in-process oracle fixture, since this is the stage
   at which the test host's own framework stops matching the older leg. The net11 legs come
   up green because everything not yet implemented is *parked, visibly*, and the four-arg
   `Setup` arm plus the stage-4/5 net11 rows land here against a real image. TFMs of the
   executable projects move to the newest supported framework; the published libraries stay
   `net8.0`.
7. **Walk the net11 blockers one at a time** — thin-lock addressing, the thread-static seed,
   the new QCalls, the vtable slots, the flavour sentinel, the `Half` and `SZArrayHelper`
   type-level `[Intrinsic]`s and the other newly-`[Intrinsic]` bodies, the three
   re-reviews of changed `safeIntrinsics` bodies, the two PAL facts — each PR un-parking its cases
   (guest or non-guest) on the net11 leg, exactly the incremental discipline `AGENTS.md`
   already prescribes for frontier work.
8. **When the window moves** (net12 preview lands in nixpkgs): add its pin set and rows;
   retire net10's in one cleanup PR.

Stages 2–5 are useful even if multi-version support were later abandoned: each replaces a
hardcoded assumption with a classified fact, on the version we already run.

## Costs and risks, honestly

* **CI time roughly doubles** for the window's duration. This is the real price of the word
  "supported", and it is bounded by capping the window at two.
* **A missed version-sensitive site** applies the wrong major's behaviour silently. Caught by
  the per-version differential legs; the placement rule keeps the sites enumerable so review
  can ask "which table should this be in".
* **Shape classification can mask a semantic change** that kept the old shape: a method whose
  signature did not change but whose contract did would classify as the familiar arm. The
  differential oracle is again the net; the tables are the fix once caught.
* **Per-version parking bookkeeping** grows the `unimplemented` structure and its comments.
  Accepted; the alternative (a green net11 leg by exclusion) hides exactly what we want
  measured.
* **nixpkgs lag** decides which net11 build we can pin (preview 7 today, RC 1 exists upstream
  but is not packaged). The pin-set structure does not care which prerelease it names; bumping
  within net11 is the existing `sync-dotnet-runtime` motion.

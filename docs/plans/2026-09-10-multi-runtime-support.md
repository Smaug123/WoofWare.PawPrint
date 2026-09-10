# Plan: support multiple emulated runtimes in one PawPrint

Status: **plan, not yet started**. Companion to `2026-09-09-net11-spike.md`, which measured
what a .NET 11 upgrade costs; this plan restructures PawPrint so that upgrade — and every
future one — lands incrementally on `main`, and so that one PawPrint build can run guests
against more than one .NET major version.

## Problem

PawPrint emulates exactly one runtime: `EmulatedRuntime.current` is a compile-time constant,
the native handler set in `NativeDispatch` is "the net10 set", and every pin in `flake.nix`
names one version. The net11 spike showed the consequences:

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
* an *unknown* CoreLib version is refused loudly at load, never approximated. "Supported"
  means pinned, drift-checked, and CI-run — nothing else. This is the correctness-over-
  availability rule: we can state the guarantee for a pinned version and cannot for an
  unpinned one, so an unpinned one does not run.

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
| servicing version | CoreLib `AssemblyInformationalVersion`, the SemVer before `+` | `10.0.7-servicing.26217.108` | drift validation |

Major granularity is the right key: the CoreLib-to-runtime internal contract churns at major
boundaries and holds within a servicing train (every divergence the spike found is a
net10-vs-net11 fact, not a 10.0.x fact). The informational version's build metadata names an
internal build commit, not the public tag — `EmulatedRuntime.SourceCommit`'s docstring already
records that trap — so drift validation compares the SemVer core only.

Note the prerelease wrinkle: a preview CoreLib's informational version carries a prerelease
label, which is exactly what `WoofWare.DotnetRuntimeLocator` PR #206 taught the *locator* to
parse. PawPrint's own drift comparison must not regress this by parsing with `System.Version`.

### Where the active runtime lives

`EmulatedRuntime.current` is deleted. `EmulatedRuntime` values become the support allowlist
(`net10`, later `net11`), and the active one becomes a field established when CoreLib loads.

Two placements were considered:

* **A field on `BaseClassTypes`.** Coherent (it is a fact established at CoreLib load, like
  everything else in that record) and already threaded everywhere — but `BaseClassTypes` lives
  in `WoofWare.PawPrint.Domain`, a published package, and `EmulatedRuntime` is a main-library
  concept. Moving it down or widening Domain's API for this is avoidable churn.
* **A field on `IlMachineState`, set by `IlMachineState.initial`. Chosen.** `initial` already
  takes the corelib, so the version is discoverable exactly there; `NativeCallContext` already
  carries `State`, so every native handler can see it; the intrinsic gate and
  `AppContextSeed.prepareCall` already have the state in hand. No published API changes.

`initial` reads the loaded CoreLib's `AssemblyVersion`, looks the major up in the allowlist,
and stores the matched `EmulatedRuntime`. No match is a loud refusal naming the version found
and the versions supported. The drift test stops comparing against a constant and instead
asserts that the *loaded* runtime's servicing version matches the pin for its major.

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

**Behaviour facts: per-version rows in tables that are already data.** Where the divergence is
in what a primitive *does* or which route a method takes, the fact cannot be read off the
image and lives in versioned data instead:

* `IntrinsicMethodKeys`' pattern entries gain an optional applicability range (introduced-in /
  removed-in major, both defaulting to "always"). The net11 `EqualityComparer<T>.Create`
  allowlisting becomes a row introduced at 11. The table stays inspectable data, and the
  per-version delta is a reviewable diff of rows, in the same spirit as
  `WoofWare.PawPrint.Semantics` keeping execution-model rules as data.
* `NativeDispatch` composes its handler list per runtime, as its own comment plans: a shared
  core list plus per-version additions (e.g. net11's `AppContext_TryGetHostPropertyValue`
  QCall, `SystemNative_FileSystemSupportsLocking`) and removals (e.g. net10-only handlers for
  entry points net11 deleted, such as `Monitor.TryEnter_FastPath`'s InternalCall). Selection
  is on the state's `EmulatedRuntime`.
* The PAL conversions beside `WoofWare.PawPrint/Native/` gain versioned rows where measured —
  the spike found `PosixSignal`'s managed enum membership and
  `SystemNative_GetPlatformSignalNumber`'s answers both changed. `WoofWare.PosixKernel`
  itself is not touched: it speaks POSIX, and POSIX did not change; only the PAL encodings on
  the PawPrint side did.

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
| `EqualityComparer<T>.Create` intrinsic | table row introduced at 11 |
| `FastAllocateString` gained `[Intrinsic]` | **version-agnostic bug, fix now on net10**: a method PawPrint implements natively must reach that implementation regardless of an `[Intrinsic]` marker; the gate at the `isSafeIntrinsic` check in `IlMachineStateExecution` currently runs ahead of native dispatch |
| `Monitor.Enter` runs `ObjectHeader.AcquireThinLock`, reading twelve bytes before `RawData.Data` | real interpreter work (object-header addressing); net11-only code path, exercised by the net11 CI leg |
| `Environment.CurrentManagedThreadId` reads a thread-static CoreCLR fills natively | net11 row: seed `ManagedThreadId.t_currentManagedThreadId` at thread creation |
| `SystemNative_FileSystemSupportsLocking` unimplemented | net11 handler row |
| vtable slot placement off by a consistent offset for `Stream`/`MemoryStream`/`Task<T>` | missing base slots; investigate against the net11 image — possibly version-agnostic builder work |
| `TestLinuxCoreLibFlavour` sentinel import absent from net11 CoreLib | replace the sentinel with a distinction that exists in both supported majors |
| `PosixSignal` membership / `GetPlatformSignalNumber` answers changed | versioned PAL rows |

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
  they should read the selected framework's CoreLib from disk instead.

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
is precisely the mismatch the spike measured for `GetPlatformSignalNumber`.

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

That set only reaches the Guest fixtures. A *non-guest* fixture blocked on one version — the
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
3. **De-singleton `EmulatedRuntime`.** Delete `current`; `IlMachineState.initial` classifies
   the loaded CoreLib's major against the allowlist (of one, at this stage) and stores the
   match; unknown majors are refused with the supported set named. Drift test re-keys onto the
   loaded runtime. `NativeDispatch`'s comment becomes code: the handler list is selected by
   the state's runtime, with one list to select from.
4. **Shape classifications on net10.** `SetupShape` (three-arg arm live, four-arg arm refused
   as unrecognised until stage 6 adds it) and the `RuntimeFieldInfoStub` layout classifier
   (current layout live), each with its readers. The fabricated-image machinery
   (`FabricatedGuest`, and the fabricated-image-load-oracle practice) can exercise the
   refusal arms without a net11 CoreLib.
5. **Versioned tables.** Applicability ranges on `IntrinsicMethodKeys` patterns; per-runtime
   composition in `NativeDispatch` (net10 list identical to today's — a
   `check-move-is-rename-only`-style diff discipline applies); versioned PAL rows where the
   spike measured changes. All rows still net10-only in effect.
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
   the new QCalls, the vtable slots, the flavour sentinel — each PR un-parking its cases
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

# WoofWare.PawPrint.Test

The repository root's `AGENTS.md` says how to run the suite and which filters to avoid. This file is about the suite's infrastructure. As there, do not add inventories: name a mechanism and the file that holds it, and let the code list its users.

## Guest test cases

* C# files in `sourcesPure/` become test cases automatically (`TestPureCases.fs` has the mechanism); those in `sourcesImpure/` must be registered in `TestImpureCases.fs`. Each is compiled and run under the interpreter.
* `TestHarness.fs` runs test assemblies through the interpreter.
* A known-unimplemented guest is *parked* rather than deleted, so we won't forget it. Parking is per runtime: each `Guest` fixture's parked cases, with the reason for each and the ending expected on that runtime, come from an exhaustive `onRuntime : EmulatedRuntime -> ...` match, into every arm of which the cases parked everywhere (`parkedEverywhere`) are folded. A run evaluates it at `FrameworkUnderTest.runtime ()`, and `TestGuestParking` checks the tables on every runtime without running a guest. A test outside the `Guest` fixtures parks with `ParkedOn.check runtimes reason`.

## Why guest fixtures are `Explicit`

The fixtures that run expensive guests under the interpreter are most of the suite's cost and a minority of its tests, so they are `[<Explicit>]` and a bare `dotnet test` skips them. Each carries category `Guest`, or `GuestSolo` if its single tests each run about as long as all of `Guest` does on one CI runner; CI runs each category on its own runner. `rg -l 'Category\("Guest(Solo)?"\)' WoofWare.PawPrint.Test` lists them.

* A runner must select its category positively. A negated clause (`TestCategory!=GuestSolo`, `FullyQualifiedName!~...`) is never an explicit match, so it skips every case of an `[<Explicit>]` fixture and the run still passes. `TestCiSelectsEveryExplicitFixture` reads the categories from `ci.yaml` and fails unless every `[<Explicit>]` fixture carries exactly one of them.
* A category match is not an explicit match in NUnit's sense, so the method-level `[<Explicit>]` parked cases stay skipped under one. A filter that names a case directly *is* an explicit match, which is why a bare `FullyQualifiedName~` filter runs parked cases.
* Guests still run in the default suite, in small fixtures without the category: every `TestFabricated*` fixture, for instance, runs a Roslyn-compiled driver over hand-emitted IL under both the interpreter and the real runtime.

## Which framework a guest runs on

* `FrameworkUnderTest.fs` decides which framework guests run on, under PawPrint and under the real-runtime oracle alike. `PAWPRINT_TEST_RUNTIME` names an `EmulatedRuntime` case; unset selects the test host's own. An unrecognised value, or a framework whose CoreLib is not the selected runtime's, fails the run rather than falling back to the host. Run a guest along `FrameworkUnderTest.runtimeDirs ()`, and read the pinned runtime source through `FrameworkUnderTest` too; `TestFrameworkLocatorRatchet` fails if test code locates a framework any other way. Guests are still *compiled* against the test host's framework, which is the oldest supported runtime.
* `HostImage.fs` is the test host's own CoreLib and framework directory, as fixtures for PawPrint's reader. A test that parses CoreLib without starting a guest reads that image, and a same-image oracle (PawPrint's reading of an image checked against host reflection over the same types) must. A test that asserts a *shape* of the runtime's CoreLib is a claim about the runtime under test, so it reads `FrameworkUnderTest.coreLibPath ()` instead; one that checks every supported runtime's own image reads each through `FrameworkUnderTest.requireCoreLibPathOf`, over `FrameworkUnderTest.everyRuntime`. The ratchet also fails on an assembly's `.Location` (or a module's `.FullyQualifiedName`) outside `HostImage.fs`, except where an allowance names the assembly it locates.

## The Linux CoreLib flavour

`flake.nix` pins each runtime's managed linux-x64 runtime pack, and the devshell exposes it as `$DOTNET_LINUX_FRAMEWORK_DIR_NET10` (and `_NET11`). A test reads the one for the runtime under test through `FrameworkUnderTest.requireLinuxFrameworkDirectory ()`, which ignores the test outside the Nix devshell. Put that directory at the *head* of the runtime-dir list passed to `Program.run`: binding is by simple name and takes the first hit, so every framework assembly then resolves from the pack. `TestLinuxCoreLibFlavour.fs` is the worked example. Bumping a runtime's `expectedRuntimeVersion` in `flake.nix` means bumping its `linuxFrameworkHash` in the same commit.

The real-runtime oracle runs on a framework installed on the host, so it cannot be the oracle for a foreign flavour: a linux-flavour test compares PawPrint-on-Linux-CoreLib against the host runtime, which is a claim about facts that hold across flavours (an exit code), not a same-image comparison.

## The real-runtime oracle

`RealRuntime.fs` runs a guest on real .NET **as its own process**, on the framework under test through a generated runtimeconfig, and classifies how it terminated. It must stay out of process. In-process, the guest shares every process-global with the test host — including CoreCLR's single latched exit code, which is what a `void` entry point's exit code *is*, so concurrent guests read each other's exit codes — and `Environment.Exit` or `FailFast` in a guest kills the test runner outright. `WoofWare.PawPrint.Performance` keeps a deliberately in-process copy for benchmarking; it is not an oracle and must not become one.

## Reading the repository

A test that reads a file from the repository at run time must name it in a `FootprintInput` item in `WoofWare.PawPrint.Test.fsproj`, so that it is in the test footprint (`scripts/footprint.py`). CI runs the suite in a checkout holding only that footprint, so an undeclared read fails there. Fail on a missing file rather than skipping it: a read that tolerates absence, such as listing a directory and checking each file in it, passes vacuously in that checkout instead.

## Memory

The test host runs with its GC heap capped at 8 GiB (`System.GC.HeapHardLimit` in both test `.fsproj`s, asserted by each project's `TestGcHeapHardLimit.fs`). Unbounded, the suite's peak RSS exceeds what a 16 GB CI runner has, and the OOM killer reaps the test host mid-run, which reports only "Test host process crashed" with no stderr. The cap is absolute rather than a fraction of the machine so that several concurrent test runs on one machine stay bounded too.

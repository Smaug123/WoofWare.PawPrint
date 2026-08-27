# Probe: how far does "what exceptions escape this method?" get without the interpreter?

This probe exists to put numbers under
`docs/plans/2026-08-26-exception-escape-analysis.md`. It answers the question that plan turns on:
**what does an escaping-exception analysis actually need, and which of those things does the
repository already have?**

It references `WoofWare.PawPrint.Domain` and `WoofWare.PawPrint.Semantics`, and nothing else —
never the interpreter. Everything it manages to do is therefore something an analyser could do
today without seeing `IlMachineState`; every `Unknown` it reports is a place where an analyser
would need something more, and each one is counted under a named reason so the size of that wall
is a number rather than an impression.

## Running it

```bash
nix develop -c dotnet build docs/probes/exception-escape/Fixture/Fixture.csproj -c Release
# The oracle. Exits non-zero if any expectation fails.
nix develop -c dotnet run --project docs/probes/exception-escape/ExnSpike.fsproj -- \
    docs/probes/exception-escape/Fixture/bin/Release/net10.0/Fixture.dll
# A self-contained framework image, and an ordinary library. The walls differ; that is the point.
nix develop -c dotnet run --project docs/probes/exception-escape/ExnSpike.fsproj -- \
    "$DOTNET_LINUX_FRAMEWORK_DIR/System.Private.CoreLib.dll"
nix develop -c dotnet run --project docs/probes/exception-escape/ExnSpike.fsproj -- \
    "$DOTNET_LINUX_FRAMEWORK_DIR/System.Text.Json.dll"
```

An optional second argument is a substring filter; matching methods have their computed escaping
set printed.

The `measured-*.txt` files are the outputs at the pinned runtime version. Re-measure rather than
trusting them if the pin moves.

## What is in here

* Which exceptions each IL opcode can raise by itself comes from `OpcodeFaults`, in
  `WoofWare.PawPrint.Semantics` — the same table the interpreter raises through. The probe had its
  own copy until that table existed; sharing it is what makes the interpreter's own guest corpus an
  oracle for the analyser's semantics.
* `Census.fs` — counts the raw material: body kinds, `throw` sites and what precedes them, exception
  regions and their clause types, callee token kinds, and MemberRef parents.
* `Escape.fs` — the interprocedural fixpoint. `Unknown` is the top element, reached at eight named
  walls, each counted by site. Its `Options` record switches the refinements on and off — the two
  type-initialisation prunes, and the report-level kind filter — and the driver runs several
  combinations so the size of what each buys is measured rather than assumed.
* `CctorCensus.fs` — how much of the `TypeInitializationException` load a static checker could
  discharge: invoking sites classified by whether the target type has an initializer at all, and by
  whether that initializer can throw. Also names the types whose initializers can, since a
  percentage is not something a reader can picture.
* `Driver.fs` — reporting, and the oracle: `fixtureExpectations` states what each fixture method's
  escaping set must be, and the process exits non-zero on any mismatch. A second, smaller table is
  checked against the *sound* run, for the one case that is about an opcode-raised fault the
  control run suppresses by construction.
* `Fixture/Cases.cs` — fourteen cases whose expected answer is stated in the driver.

## Reading the results

The most useful thing measured is that the walls' sizes **flip between assemblies**. CoreLib
references almost nothing outside itself, so every one of its 19,588 MemberRef call sites has a
`TypeSpec` parent rooted in CoreLib — an instantiation of a local type, not a foreign call — and
its `ForeignCallee` count is exactly zero. System.Text.Json inverts this: foreign callees are its
largest wall at 41.8% of call sites. Measuring "the cross-assembly wall" on CoreLib alone measures
nothing, which is why both are checked in.

## The modes

The driver runs four:

| mode | what it is for |
| --- | --- |
| sound, without the self-initialisation prune | shows what that one refinement is worth |
| sound | the honest answer, both type-initialisation prunes on |
| practical | the honest answer minus `FaultKind.ResourceExhaustion` — **unsound by choice**, and labelled so |
| control | opcode-raised faults suppressed entirely; not an answer, a measurement of how much they swamp |

The practical mode is the one a person would read. It is deliberately not the default and never the
one an "is this safe?" claim should rest on: dropping a kind means the result is no longer an
over-approximation.

## What it is not

It is not a whole-program analysis, and does not try to be. Its deliberate limits, each of which is
what makes the corresponding count meaningful:

* a `callvirt` contributes `Unknown` rather than the join over every possible override, so the
  `VirtualCall` count measures what devirtualisation would buy;
* the subtype relation is built from the analysed assembly's own TypeDefs, so a base chain that
  leaves the assembly is not followed. `Fixture.Cases::CaughtByBase` therefore reports
  `InvalidOperationException` escaping a `catch (SystemException)`, while the same-assembly
  `CaughtByLocalBase` correctly absorbs its exception. That contrast is the cross-assembly wall
  visible in two adjacent methods, and it is why both are in the fixture.

## The oracle is falsifiable

Checked rather than assumed: making the universal-clause test (`catch (Exception)` / `catch
(Object)` absorbs everything) return `false` kills exactly one expectation, `CatchesBoth`, and no
other. The expectations were written from the instrument's stated envelope before it was run, not
recorded from its output.

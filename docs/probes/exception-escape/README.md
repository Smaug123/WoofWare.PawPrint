# Probe: how far does "what exceptions escape this method?" get from `WoofWare.PawPrint.Domain` alone?

This probe exists to put numbers under
`docs/plans/2026-08-26-exception-escape-analysis.md`, which argues about whether PawPrint's
semantics need reifying into a first-order command language before an analyser can be built.
It answers the narrower question that argument turns on: **what does an escaping-exception
analysis actually need, and which of those things does the repository already have?**

It deliberately references `WoofWare.PawPrint.Domain` and nothing else. Everything it manages
to do is therefore something an analyser could do today without seeing `IlMachineState`; every
`UNKNOWN` it reports is a place where an analyser would need something more.

## Running it

```bash
nix develop -c dotnet build docs/probes/exception-escape/Fixture/Fixture.csproj -c Release
nix develop -c dotnet run --project docs/probes/exception-escape/ExnSpike.fsproj -- \
    "$DOTNET_LINUX_FRAMEWORK_DIR/System.Private.CoreLib.dll"
nix develop -c dotnet run --project docs/probes/exception-escape/ExnSpike.fsproj -- \
    docs/probes/exception-escape/Fixture/bin/Release/net10.0/Fixture.dll "Cases::"
```

The second argument is an optional substring filter; matching methods have their computed
escaping set printed.

`measured-corelib-10.0.7.txt` and `measured-fixture.txt` are the outputs at the pinned runtime
version. Re-measure rather than trusting them if the pin moves.

## What is in here

* `Implicit.fs` — which exceptions each IL opcode can raise *by itself*, with no callee
  involved. This is the entire "IL semantics" an escape analysis needs: a classification, not
  an interpreter. PawPrint's own interpreter has the same knowledge, but scattered across the
  16 files that call `IlMachineStateExecution.raiseRuntimeException`.
* `Census.fs` — counts the raw material: body kinds, `throw` sites and what precedes them,
  exception regions and their clause types, callee token kinds.
* `Escape.fs` — an actual interprocedural fixpoint over one assembly's own MethodDefs.
  `Unknown` is the top element and is reached at four named walls, each counted separately:
  a callee outside this assembly, a generic instantiation, a `callvirt`, and a method with no
  IL body.
* `Fixture/Cases.cs` — the correctness oracle. Each method's name states the escaping set it
  should produce, so the answer is checkable by eye.

## What it is not

It is not sound as a whole-program analysis, and it is not trying to be. Two specific
unsoundnesses are load-bearing to the measurement:

* a `callvirt` contributes `Unknown` rather than the join over every possible override, so the
  count of `VirtualCall` sites measures how much devirtualisation would have to buy;
* the subtype relation is built from the analysed assembly's own TypeDefs only, so
  `Fixture.Cases::CaughtByBase` reports `InvalidOperationException` escaping a
  `catch (SystemException)`. That is the cross-assembly-resolution wall showing up in the one
  place a reader can see it directly. `CaughtByLocalBase`, whose hierarchy is in the same
  assembly, does absorb its exception, which is what confirms the mechanism is right and only
  the inputs are missing.

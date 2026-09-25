# WoofWare.PawPrint.Semantics

Rules of the CLI execution model, as inspectable data.

`WoofWare.PawPrint.Domain` answers "what is in this DLL?": IL opcodes, the type system, metadata
handles. This library answers a different kind of question — "what does running this opcode do?" —
and keeps the answers as tables rather than as code that does the thing.

That distinction is the whole point. A fact kept as data can be consumed by the interpreter *and*
read by something that never executes anything: an analyser computing which exceptions can escape a
method, which methods can reach the filesystem, or which are pure. A fact kept only as control flow
inside an interpreter is available to the interpreter alone.

What lives here:

* `ContextSwitchPrior` — how likely interleaving an opcode with another thread's steps is to reveal
  a guest-visible difference. Consumed by PawPrint's Probabilistic Concurrency Testing scheduler.
* `OpcodeFaults` — which exceptions an instruction can raise by itself, as opposed to which reach it
  from a callee. Consumed by the interpreter, which raises through it and checks itself against it,
  and readable by an analyser that never runs anything.
* `StackShape` — the shape of the evaluation stack on entry to every instruction of a body, joined
  over every path that reaches it, and the control-flow joins at which CoreCLR's importer widens a
  float32 slot to double because another path delivers a double there. What a token-bearing
  instruction does to the stack comes in as data (`StackShapeTokens` reads it from a PE module's
  own signature blobs); the interpreter checks its own stack against the analysis in Debug builds.
* `IntrinsicBody` — what CoreCLR executes for an `[Intrinsic]` method: its own IL, or one of two
  placeholders, IL that calls itself (which the JIT must expand) or a CoreLib body that cannot return
  (which the VM substitutes). Only a placeholder's call to itself is expanded; the rest of its IL
  runs. On a hardware-intrinsic class that call is a capability query or an instruction, and
  `IntrinsicBody.expandSelfCall` says what it does on a given CPU: yields a constant for a query,
  raises `PlatformNotSupportedException` for an instruction the CPU lacks. The interpreter runs a
  method's own IL, expands its self-call, and refuses the rest unless it implements the method
  itself; an analyser can treat the self-call the same way.
* `IntrinsicPrimitive` — the operations CoreCLR's runtime performs in code of its own when a
  CoreLib intrinsic is called (barriers, atomics, `GetMethodTable`, the estimates, ...), how each
  CoreLib method is recognised as one, and each operation's contract: the faults it raises and
  under what conditions, whether it returns, and the nullness of its result. `TestIntrinsicContracts`
  holds the contracts to real .NET.
* `VmSubstitution` — the IL CoreCLR's VM runs in place of CoreLib's body for each
  `System.Runtime.CompilerServices.Unsafe` method corelib.h binds, transcribed from
  `getILIntrinsicImplementationForUnsafe` (jitinterface.cpp). The interpreter runs a stub where it
  has no implementation of its own; an analyser can read every one, including those whose CoreLib
  body works but is not what CoreCLR runs.
* `HardwareIntrinsicsProfile` — the virtual CPU as the guest sees it: which hardware-intrinsic
  classes answer `IsSupported` and which vector APIs answer `IsHardwareAccelerated`. PawPrint runs
  on `ScalarOnly`, where every answer is false.

The dependency direction is the invariant: this library sees `WoofWare.PawPrint.Domain` and never
`WoofWare.PawPrint`, so nothing in here can reach the interpreter's mutable machine state. That is
enforced by the project graph rather than by discipline.

## Stability

Pre-1.0, and moving. It is published because `WoofWare.PawPrint` depends on it and a NuGet package
cannot depend on something unpublished; treat the surface as unstable until that changes.

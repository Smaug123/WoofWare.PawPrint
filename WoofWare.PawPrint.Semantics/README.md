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
* `ControlFlow` — where control goes after each instruction, and which offsets a run of a body can
  execute: following branches, fall-through and `leave`, entering a handler only once its protected
  block has run, and following a conditional branch one way only where it pops a Boolean known in
  advance. `StackShape` and the escape analysis both read it.
* `StackEffect` — what each instruction pops, and what each value it pushes is (a literal, an
  argument, a call's result, an element of the array popped, ...), without saying what such a
  value is in any one analysis's terms. CoreCLR's own `opcode.def` is its oracle.
* `StackFlow` — a dataflow over the evaluation stack, generic in what it tracks about each slot: a
  `SlotLattice` says what each `StackEffect` push is in its terms and how two paths' values meet,
  and the flow joins them over every path, sharing CoreCLR's spill temps across a join.
* `StackShape` — `StackFlow` over float widths: the shape of the evaluation stack on entry to every
  instruction of a body, joined over every path that reaches it, and the control-flow joins at which CoreCLR's importer widens a
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
  holds the contracts to real .NET, and PawPrint's implementations to the contracts.
* `NativeMethod` — methods CoreCLR implements in native code (an `InternalCall` or a P/Invoke)
  whose behaviour is known, and what each can do to its caller, stated as an `IntrinsicPrimitive`
  contract is. So far these are the C runtime's maths functions that `Math` and `MathF` call,
  none of which can fault. `TestNativeMethod` checks the recognition against both CoreLibs'
  metadata, and calls each method on real .NET over its edge values.
* `HardwareInstruction` — what a hardware-intrinsic placeholder's call to itself can raise when the
  JIT expands it into the instruction on a CPU that has it: `NullReferenceException` for a null
  address where the instruction touches memory, `ArgumentOutOfRangeException` where an immediate
  operand is out of range, `DivideByZeroException` or `OverflowException` from an x86 integer
  divide, or unknown for the JIT's helper and special intrinsics. It is read from the JIT's own
  tables (`hwintrinsiclist*.h`), which `HardwareIntrinsicTable.tsv` reproduces, and its mapping from
  an intrinsic class to the table's instruction set (`lookupIsa`) is transcribed for Arm64 and x64.
  `TestHardwareInstruction` regenerates that table from the pinned runtime source and fails on a
  difference; checks that every placeholder in the arm64 and linux-x64 CoreLibs has a row; and on an
  arm64 or x64 host calls every instruction the CPU has with null, misaligned and valid addresses
  and every immediate value, and fails on any exception a contract leaves out.
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

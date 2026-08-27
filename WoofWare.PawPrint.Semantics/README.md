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

The dependency direction is the invariant: this library sees `WoofWare.PawPrint.Domain` and never
`WoofWare.PawPrint`, so nothing in here can reach the interpreter's mutable machine state. That is
enforced by the project graph rather than by discipline.

## Stability

Pre-1.0, and moving. It is published because `WoofWare.PawPrint` depends on it and a NuGet package
cannot depend on something unpublished; treat the surface as unstable until that changes.

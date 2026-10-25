# WoofWare.PawPrint.Semantics

Rules of the CLI execution model, as inspectable data.

`WoofWare.PawPrint.Domain` answers "what is in this DLL?": IL opcodes, the type system, metadata
handles. This library answers a different kind of question — "what does running this opcode do?" —
and keeps the answers as tables rather than as code that does the thing.

That distinction is the whole point. A fact kept as data can be consumed by the interpreter *and*
read by something that never executes anything: an analyser computing which exceptions can escape a
method, which methods can reach the filesystem, or which are pure. A fact kept only as control flow
inside an interpreter is available to the interpreter alone.

It holds facts of four kinds:

* the IL execution model: what each instruction pops, pushes and raises by itself, where control
  goes after it, and the shape of the evaluation stack on entry to it;
* what CoreCLR's JIT does with a body: what it compiles, which types it loads where, which
  signatures it refuses, and which instructions it folds away;
* the methods CoreCLR implements itself rather than in CoreLib's IL: intrinsics, FCalls, QCalls,
  P/Invokes into the framework's own native libraries, and the VM's substitute bodies, with what
  each can raise; and the CoreLib fields the VM lays out as one native object;
* the platform a guest sees: the supported runtimes, the virtual CPU's hardware intrinsics, and how
  likely interleaving an instruction with another thread is to matter.

Each module's docstrings say what it describes and where it was read from. Most tables are checked
by `WoofWare.PawPrint.Test` against the pinned runtime source, real .NET, or both.

The dependency direction is the invariant: this library sees `WoofWare.PawPrint.Domain` and never
`WoofWare.PawPrint`, so nothing in here can reach the interpreter's mutable machine state. That is
enforced by the project graph rather than by discipline.

## Stability

Pre-1.0, and moving. It is published because `WoofWare.PawPrint` depends on it and a NuGet package
cannot depend on something unpublished; treat the surface as unstable until that changes.

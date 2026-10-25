# WoofWare.PawPrint.TypeSystem

The CLI type system as CoreCLR lays it out, over the whole set of loaded assemblies.

`WoofWare.PawPrint.Loader` answers "which image, and which definition in it, does this reference
name?". This library answers what the runtime builds from those definitions: the concrete type a
generic instantiation denotes, and a type's method table — which slot each method owns, and what
each slot holds. Like the Loader, it runs no code.

It answers four kinds of question:

* what a generic type or method denotes once instantiated, as a handle identifying one concrete
  type or method, and how signatures compare in those terms;
* what a type's method table holds: which slot each method owns, what each slot holds once
  MethodImpls are applied, and which slot implements each interface method;
* which method a call runs: a member reference bound to the definition it names, and a virtual,
  interface or `constrained.` call dispatched on a receiver of known type, default interface
  bodies and variance included;
* what the runtime and the JIT decide about concrete types: whether one can be cast to another,
  whether a method runs as shared generic code, whether storage holds GC references, and what the
  type loader records about a value type's layout for marshalling.

`TypeSystemState` holds what all of these are computed over, and the memos they keep. Each module's
docstrings say what it decides, and which part of CoreCLR it follows.

The motivating consumer, besides PawPrint's interpreter, is an analyser that answers questions about
a method without running it. Such a thing must know which method a call runs exactly as the
interpreter does, and must never see the interpreter's machine state; this package depends on
`WoofWare.PawPrint.Domain` and `WoofWare.PawPrint.Loader` alone, and the project graph enforces that.

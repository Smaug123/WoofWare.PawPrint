# WoofWare.PawPrint.TypeSystem

The CLI type system as CoreCLR lays it out, over the whole set of loaded assemblies.

`WoofWare.PawPrint.Loader` answers "which image, and which definition in it, does this reference
name?". This library answers what the runtime builds from those definitions: the concrete type a
generic instantiation denotes, and a type's method table — which slot each method owns, and what
each slot holds. Like the Loader, it runs no code.

What lives here:

* `TypeConcretization` — instantiating a generic type definition over the whole set of loaded
  assemblies, to a handle that identifies one concrete type, and comparing signatures and
  substitutions in terms of those handles.
* `VtableSlot`, `MethodTableLayout` — a type definition's method table as CoreCLR's
  `MethodTableBuilder` lays it out: which slot each declaration owns, what each vtable slot holds
  once MethodImpls are applied, and the slots beyond the vtable.
* `MethodReferenceResolution` — which method a `MemberRef` names, as CoreCLR binds it: the
  `MethodDef` of a generic definition, with the instantiation of the type that declares it (the
  parent or one of its ancestors), a method the runtime supplies on an array type, or nothing (a
  `MissingMethodException`, or a `TypeLoadException` if the parent names no type). It lives here
  rather than beside the Loader's field resolver because CoreCLR binds a method reference by
  searching each type's method table, as `MethodTableLayout` lays it out, in the order CoreCLR does;
  it is checked against the real runtime's own answer.
* `MemberReferenceInstantiation` — a `MemberRef` resolved as `MethodReferenceResolution` and the
  Loader's `FieldReferenceResolution` bind it, then instantiated for one generic context: the method
  or field it names, with the arguments of the type that declares it, memoised per context.
* `TypeSystemState` — the state all of the above is computed over: the assemblies loaded, the
  concrete types instantiated so far, and the memos of method-table walks, member resolution and
  method concretisation. The interpreter's machine state holds one. Its module asks the questions
  that need nothing else: instantiating a type or signature, comparing signatures and
  instantiations as CoreCLR does, resolving a type token, and finding a concrete type's base.
* `MethodConcretisation` — a method instantiated for execution: its declaring type's and its own
  generic parameters bound to concrete types, and its signature and locals concretised against
  them, memoised per instantiation.
* `ConcreteMethodTable`, `ConcreteInterfaceDispatch` — the method table of a concrete type, read
  through its definition's, and CoreCLR's interface map and dispatch map: which slot of the receiver
  implements an interface method, before any default interface body is considered.
* `ConcreteVirtualDispatch` — which method a virtual or interface call runs on a receiver of a
  known concrete type, as CoreCLR's `MethodTable::FindDispatchImpl` decides it: the dispatch table
  and MethodImpls, the dispatch map, default interface bodies, variance, and the methods the
  runtime supplies for an SZ array's implicit interfaces, or that the call is ambiguous because
  more than one default body is most specific, or that it is not modelled, where default bodies
  conflict through a variant interface; and which implementation of a static abstract interface
  member a `constrained.` type supplies. It names the method it chooses and the generic arguments it
  runs with, and leaves instantiating that method to `MethodConcretisation`: CoreCLR reads a
  method's locals only when it compiles the method.
* `TypeAssignability` — whether a value of one closed type can be stored where another is
  expected, as CoreCLR's `CanCastTo` decides it: the base chain, interfaces, variance, and the
  array rules.

The motivating consumer, besides PawPrint's interpreter, is an analyser that answers questions about
a method without running it. Such a thing must know which method a call runs exactly as the
interpreter does, and must never see the interpreter's machine state; this package depends on
`WoofWare.PawPrint.Domain` and `WoofWare.PawPrint.Loader` alone, and the project graph enforces that.

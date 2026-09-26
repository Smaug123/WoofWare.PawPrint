# WoofWare.PawPrint.Loader

How the assemblies of a .NET program refer to one another.

`WoofWare.PawPrint.Domain` reads one image: its opcodes, its type definitions, its metadata handles.
Almost every interesting question about a program needs more than one image, though. A `TypeRef`
names a type by the assembly reference it is scoped to, and answering "which type is that?" means
binding the reference to an image, following that image's type forwarders, and perhaps doing the same
again for a nested type's parent. Whether a type is a value type is decided by its base-type chain,
which crosses assemblies freely. This library holds the rules for all of that, and nothing that runs
code.

What lives here:

* `LoadedAssemblies` — the set of images loaded so far, keyed by their own definition identity, and
  the record of which assembly reference was bound to which of them. The two identities differ in
  practice (the .NET Framework compatibility facades reference implementation assemblies as
  `Version=0.0.0.0`), and the type keeps them apart.
* `IAssemblyLoad`, `AssemblyProbe` — binding an assembly reference to an image on disk, by simple
  name, from an ordered list of runtime directories.
* `LoadedTypeResolution` — resolving a `TypeRef`, a forwarded `ExportedType` or a
  namespace-qualified name to its definition, using only what is loaded, and naming the assembly
  reference to bind when that is not enough. `TypeResolution` is the loop that binds it and asks
  again, and also makes sure a type's whole base chain is loaded.
* `LoadedTypeInfo` — whether a type is a value type, an enum or byref-like, and how a signature
  encodes it, decided by walking its base chain across the loaded assemblies.
* `SignatureComparison` — whether two method signatures from different assemblies denote the same
  types, which is how a `MemberRef` is matched to the `MethodDef` it names.
* `TypeConcretization` — instantiating a generic type definition over the whole set of loaded
  assemblies, to a handle that identifies one concrete type.
* `VtableSlot`, `MethodTableLayout` — a type definition's method table as CoreCLR's
  `MethodTableBuilder` lays it out: which slot each declaration owns, what each vtable slot holds
  once MethodImpls are applied, and the slots beyond the vtable.

The motivating consumer, besides PawPrint's interpreter, is an analyser that answers questions about
a method without running it. Such a thing must resolve a call's target in another assembly exactly as
the interpreter does, and must never see the interpreter's machine state; this package depends on
`WoofWare.PawPrint.Domain` alone, and the project graph enforces that.

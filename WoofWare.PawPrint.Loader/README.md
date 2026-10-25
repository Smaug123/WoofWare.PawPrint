# WoofWare.PawPrint.Loader

How the assemblies of a .NET program refer to one another.

`WoofWare.PawPrint.Domain` reads one image: its opcodes, its type definitions, its metadata handles.
Almost every interesting question about a program needs more than one image, though. A `TypeRef`
names a type by the assembly reference it is scoped to, and answering "which type is that?" means
binding the reference to an image, following that image's type forwarders, and perhaps doing the same
again for a nested type's parent. Whether a type is a value type is decided by its base-type chain,
which crosses assemblies freely. This library holds the rules for all of that, and nothing that runs
code. What the runtime builds from the definitions once they are found — concrete instantiations and
method tables — is `WoofWare.PawPrint.TypeSystem`'s.

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
  again, and also makes sure a type's whole base chain is loaded, which `BaseChainLoading` walks.
  `TypeResolution.tryResolveTypeRefIdentity` names the definition a `TypeRef` names without loading
  any type, telling an assembly no runtime directory supplies from a type an assembly lacks.
* `LoadedTypeInfo` — whether a type is a value type, an enum or byref-like, and how a signature
  encodes it, decided by walking its base chain across the loaded assemblies.
* `SignatureComparison` — whether two method signatures from different assemblies denote the same
  types, and whether two type references name one definition (`compareTypeTokens`), which
  `WoofWare.PawPrint.TypeSystem`'s signature comparison asks too.
* `FieldReferenceResolution` — which field a `MemberRef` names: the parent's own fields only, never
  a literal one, matched by name and exact type, as CoreCLR's `FindField` binds it, or nothing (a
  `MissingFieldException`). `MemberReferenceParent` reads the parent token, which
  `WoofWare.PawPrint.TypeSystem`'s method resolver reads too, and decides whether CoreCLR could load
  the parent at all, which it loads in full before looking for the member.
  `ParentLoadVouching` says whether every type that load reaches is one PawPrint knows to load
  (CoreLib's own, combined so that none of the loader's other failures can arise), which a caller
  needs before it can say which exception binding a reference throws.
* `TypeLoad` — CoreCLR's full load of a type, walked level by level as the class loader brings
  each type it reaches through them, and the first type reference that load reaches that names
  nothing (a `TypeLoadException`) or that needs an assembly no runtime directory supplies (a
  `FileNotFoundException`). Once every type it reaches has the levels below `CLASS_LOADED`, the
  load brings each type definition's type parameters' constraints to the level below, except in
  CoreLib, which skips that; it does not check that an instantiation satisfies them. The JIT asks
  for every type a method names at that level. A caller
  loading many types passes from one load to the next the definitions those that met no miss
  loaded in full, which a load passes over as CoreCLR passes over a type already loaded. It walks
  too the approximate load by which CoreCLR lays out the arguments a signature names
  (`approximateLoadMissFrom`), which loads an instantiation's value-type arguments but not its
  class arguments, and a definition's fields but not its interfaces' type arguments. The host's
  CoreCLR is the oracle of both (`TestTypeLoad`), the second through delegates whose thunks shuffle
  arguments of each type.

The motivating consumer, besides PawPrint's interpreter, is an analyser that answers questions about
a method without running it. Such a thing must resolve a call's target in another assembly exactly as
the interpreter does, and must never see the interpreter's machine state; this package depends on
`WoofWare.PawPrint.Domain` alone, and the project graph enforces that.

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

It answers three kinds of question:

* which image an assembly reference binds to, from an ordered list of runtime directories, keeping
  the identity a reference names apart from the identity of the image it bound to;
* which definition a type reference, a forwarded type or a namespace-qualified name denotes, what
  its base chain makes it (a value type, an enum, byref-like), whether two method signatures from
  different assemblies agree, and which field a member reference names;
* whether CoreCLR's class loader can load a type, or a member reference's parent, and which
  exception it raises where it cannot.

Each module's docstrings say what it decides, and which part of CoreCLR it follows.

The motivating consumer, besides PawPrint's interpreter, is an analyser that answers questions about
a method without running it. Such a thing must resolve a call's target in another assembly exactly as
the interpreter does, and must never see the interpreter's machine state; this package depends on
`WoofWare.PawPrint.Domain` alone, and the project graph enforces that.

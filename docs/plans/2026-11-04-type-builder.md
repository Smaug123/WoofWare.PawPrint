# Emitting types with `TypeBuilder`

Status: option A chosen (2026-11-04); stage 0 implemented. Tracking issue: #2091. Follows
`docs/plans/2026-09-26-expression-thunk-dynamic-method.md`, whose stage 3 gave a dynamic assembly real
metadata (#1636) and deferred "`TypeBuilder` would later have to *extend* that metadata".

## The gap

The issue's guest defines a dynamic assembly, a type `Thing : object, IThing`, a `get_Value` method
returning 42 with an explicit `MethodImpl` onto `IThing.get_Value`, creates the type, instantiates it with
`Activator.CreateInstance` and calls through the interface. It needs
`RuntimeFeature.IsDynamicCodeSupported` overridden to `true`, as every `sourcesImpure` emit guest does,
since `AssemblyBuilder.DefineDynamicAssembly` refuses otherwise.

Measured on `main` at 96b17b82e: real .NET 10 exits 0. PawPrint stops at
`AssemblyNative_GetLoadContextForAssembly`, which `AssemblyBuilder.DefineDynamicAssembly` calls to find
the caller's load context. (The anonymous host never reaches it, because it passes
`AssemblyLoadContext.Default`.)

Past that, the path runs `RuntimeTypeBuilder`'s and `RuntimeModuleBuilder`'s QCalls. CoreLib declares 31
of them in `System.Reflection.Emit`; the issue's guest reaches, judging from the managed source, about a
dozen: `ModuleBuilder_GetTypeRef` (for `object` and `IThing`), `TypeBuilder_DefineType` (with the
interface list), `TypeBuilder_DefineMethod` (twice, counting the default constructor `CreateType` adds),
`ModuleBuilder_GetMemberRefOfMethodInfo` (for `IThing.get_Value` and `object..ctor`),
`TypeBuilder_DefineMethodImpl`, `TypeBuilder_GetTokenFromSig`, `TypeBuilder_SetMethodIL`, perhaps
`TypeBuilder_SetParamInfo`, and `TypeBuilder_TermCreateClass`. Then reflection, allocation and
interface dispatch over the new type, all reading the dynamic module's metadata.

## The constraint: tokens

Measured on .NET 10, defining types `A` and `B`, then methods `B1`, `A1`, `B2`, `A2` in that order:

```
A=02000002 B=02000003 b1=06000001 a1=06000002 b2=06000003 a2=06000004
```

and after `CreateType`, reflection over the created types reports the same tokens
(`A.A1 06000002`, `B.B1 06000001`, ...). A type `C` defined afterwards got `C1=06000007`, because each
`CreateType` added a default constructor (`06000005`, `06000006`).

So a dynamic module's `MethodDef` (and `Field`, `Param`, `Property`, `Event`) RIDs are in definition
order, not grouped by type, and they stay so for the module's lifetime. CoreCLR's read-write metadata
does this with indirection tables: `CMiniMdRW::AddChildRowIndirectForParent` (md/enc/metamodelrw.cpp)
appends the row and, when it is not at the end of its parent's range, starts a `MethodPtr`/`FieldPtr`
table so that `TypeDef.MethodList` ranges over the pointer table instead. A guest observes the tokens
through `MetadataToken`, and the IL that `ILGenerator` emits holds them.

`System.Reflection.Metadata.Ecma335.MetadataBuilder` cannot write pointer tables: a type's methods are
the contiguous run from its `MethodList`, so an image it writes cannot carry these RIDs.

## The decision: how a dynamic module's metadata grows

**A. An append-only row log, written with pointer tables.** The dynamic assembly's state is a log of
metadata rows in definition order, each QCall appending a row (or filling a column of a row whose type is
not yet created). After each QCall that changes it, PawPrint serialises the log with its own ECMA-335
writer, emitting `MethodPtr`/`FieldPtr`/`ParamPtr`/`PropertyPtr`/`EventPtr` in an uncompressed (`#-`)
table stream exactly when CoreCLR would, and re-reads the image through `Assembly.read`, replacing the
dynamic assembly's `DumpedAssembly` under the same identity. Every RID is then the token CoreCLR handed
out, there is one token space, and everything that reads an image (the loader, the type system,
`MetadataImport`, IL bodies) works unchanged. Because the log only appends, a handle in the old image
means the same in the new one, so nothing already loaded is invalidated; the replacement asserts that
every changed row belongs to a type not yet created. This is CoreCLR's own model. Cost: a table, heap and
PE writer (the heaps are simple; the tables are those Reflection.Emit writes, about twenty), and the
replace operation in `LoadedAssemblies`. Its oracle is a round-trip property over generated logs, read
back by SRM's `MetadataReader` (which reads pointer tables, by its source; stage 1 confirms it), plus the guest-level differential cases.

**B. `MetadataBuilder` with a token map.** Keep writing grouped images with `MetadataBuilder`, and keep a
bijection between CoreCLR's definition-order tokens and the image's handles, translated at every
boundary: QCall arguments, the tokens inside IL bodies and signatures (rewritten at serialisation), and
every token reflection or `MetadataImport` reports back. The writer is free. But there are two token
spaces for one module, every reader of a dynamic module's token has to know which it holds, and a type
created earlier can have its handles shift when a still-open type that precedes it gains a member, so
the type system's handle-keyed caches would need remapping too.

**C. No image for a dynamic module.** Make an assembly's content a union,
`Image of PEReader | Emitted of EmittedModule`, build the `TypeDefs`/`Methods`/... dictionaries directly
from the log keyed by CoreCLR's tokens, and hold IL bodies and blobs as byte arrays. The most explicit
about the difference, and no serialiser. But it is the same decision #1636 rejected: about 34 lines in the
interpreter, Loader and TypeSystem reach for `PeReader` or a `MetadataReader`, and `Assembly.read`'s parsing would gain a second source that has to agree
with it, which is a difference CoreCLR does not draw (a dynamic module there has metadata).

**Chosen: A** (2026-11-04). It keeps one token space, matching what the guest measures; it changes nothing that
reads an image; and its new code is a pure function from a log to bytes, with a mechanical oracle. B's
cheap writer buys a translation layer at every boundary. C's blast radius is the whole interpreter.

## Stages

Each is its own PR, and each re-runs the issue's guest to find the next stop rather than predicting it.

0. **`AssemblyNative_GetLoadContextForAssembly`.** CoreCLR returns 0 for an assembly in the default
   binder, and managed code maps 0 to `AssemblyLoadContext.Default`. Every assembly PawPrint loads is in
   the default binder (3a refuses any other context), so the handler answers 0. No design decision.

   Measured after stage 0: `DefineDynamicModule` reaches no QCall, and the issue's guest stops in
   `DefineType` at `ModuleBuilder_GetTypeRef`, resolving the parent type `object` (stage 3).
1. **The writer**, under option A: the row log as immutable data, its serialiser, and the round-trip
   property, with no interpreter change. `DynamicAssemblyImage.build` becomes the log's first three rows.
2. **The log in the machine state**: the dynamic assembly holds its log, and `LoadedAssemblies` gains the
   superset-replacement operation, with its invariant asserted.
3. **The definition QCalls the issue reaches**, each appending what CoreCLR's emitter does, and each
   other `TypeBuilder` QCall refused by name. Probably split in two: references (`GetTypeRef`,
   `GetMemberRef*`, `GetTokenFromSig`, with the `AssemblyRef` rows `Assembly::AddAssemblyRef` writes)
   and definitions (`DefineType`, `DefineMethod`, `DefineMethodImpl`, `SetMethodIL`, ...).
4. **`TypeBuilder_TermCreateClass`**: load the type from the replaced image and hand back its
   `RuntimeType`; then whatever `Activator.CreateInstance` and the interface call reach. The issue's guest
   goes into `sourcesImpure` as the acceptance case.

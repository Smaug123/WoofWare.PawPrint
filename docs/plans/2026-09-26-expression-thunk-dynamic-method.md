# Running the expression interpreter's emitted thunk

Status: plan, 2026-09-26, measured on `main` at 30a3051e. Tracking issue: #849. Motivated by the
ASP.NET ladder's rung L (`docs/plans/2026-08-17-aspnet-critical-path.md` on `aspnet-ladder`).

## The gap

`Expression<TDelegate>.Compile()` runs the expression interpreter, since PawPrint seeds
`IsDynamicCodeSupported=false`. The interpreter hands back a delegate of the caller's type by
way of `DelegateHelpers.CreateObjectArrayDelegate`, which has prebuilt thunks
(`GetCSharpThunk`) only for delegates of at most two parameters. For a wider one it emits a
thunk with Reflection.Emit (`CreateObjectArrayDelegateRefEmit`), whatever the dynamic-code
switch says: `CanEmitObjectArrayDelegate` is a constant `true` in CoreCLR's build of
System.Linq.Expressions, and the emit runs inside a scope that forces dynamic code back on. So
real .NET takes this path too, and it is not a corner. `RequestDelegateFactory` compiles a
three-parameter delegate for every minimal-API endpoint that binds a JSON body.

The thunk is an anonymously hosted `DynamicMethod` whose IL is:

```
ldc.i4 n; newarr object; stloc arr        (or call Array.Empty<object>() when n = 0)
per argument i:  ldloc arr; ldc.i4 i; ldarg i+1; [ldobj T if byref]; box T; stelem.ref
try   { ldarg.0; ldloc arr; callvirt Func<object[], object>::Invoke; stloc ret }
finally { per byref argument: ldarg; ldloc arr; ldc.i4; ldelem.ref; unbox.any T; stobj T }
ldloc ret; unbox.any TReturn; ret
```

It is then bound with `CreateDelegate(delegateType, handler)`, closed over the interpreter's
`Func<object[], object>`.

Two probes locate what stands in the way, both exiting 42 on real .NET:

* **A ten-line console guest** compiling `Expression.Lambda<Func<int, int, int, int>>`. Under
  PawPrint it stops at `AssemblyNative_InitializeAssemblyLoadContext`:
  `DynamicMethod.GetDynamicMethodsModule` touches `AssemblyLoadContext.Default` on its way to
  defining the "Anonymously Hosted DynamicMethods Assembly".
* **The same thunk IL in a `DynamicMethod` owned by a type**, with the dynamic-code switch
  overridden to `true`, which takes the anonymous hosting out of the picture. It stops in
  `ModuleHandle_GetDynamicMethod`: the signature's first parameter,
  `Func<object[], object>`, is spelled `GENERICINST INTERNAL(Func\`2) 2 SZARRAY OBJECT OBJECT`,
  and PawPrint carries the eight `ELEMENT_TYPE_INTERNAL` bytes faithfully (#1061) but cannot
  decode them.

So there are three independent gaps, plus whatever the thunk's execution reaches after them:

1. **`ELEMENT_TYPE_INTERNAL` decoding.** Stage 2 of
   `docs/plans/2026-08-18-element-type-internal.md`, which calls it uncontentious.
2. **Reflected methods in method position.** The `callvirt` names `Func<object[], object>::Invoke`,
   a method on a generic type, so `DynamicILGenerator` records it as a `GenericMethodInfo` scope
   entry (method handle plus declaring type handle); the zero-argument form's `call` names a
   `RuntimeMethodHandle`. `IlDecoding.scopeOperandKind` refuses `callvirt` outright and admits
   `call` only for `DynamicMethod` and `VarArgMethod` entries.
3. **Anonymous hosting.** The default `AssemblyLoadContext` has to exist, and
   `AppDomain_CreateDynamicAssembly` has to produce a `RuntimeAssembly` whose manifest module
   `ModuleHandle_GetDynamicMethod` can then accept as a scope.

## Stages

Each is its own PR, and each re-runs both probes and the three-parameter guest rather than
predicting the next stop.

### Stage 1: decode `ELEMENT_TYPE_INTERNAL`

`SignatureHelper.AddOneArgTypeHelperWorker` writes generic parameters as `VAR`/`MVAR`, a
constructed generic as `GENERICINST` over its definition, and byrefs, pointers and arrays
structurally. So an `INTERNAL` run only ever names a *leaf*: a non-generic type, the definition
directly under a `GENERICINST`, or a function-pointer type (which fails `IsSimpleType` and has no
branch of its own). The first two are exactly `TypeDefn.FromDefinition (identity, kind)`, with
the kind read off the resolved type, so decoding loses nothing. A function-pointer handle, or any
handle that is not a closed non-generic type or an open generic definition, is refused by name.

The walker is hand-written over the null-module alphabet (primitives, `OBJECT`, `STRING`,
`TYPEDBYREF`, `I`, `U`, `VAR`, `MVAR`, `GENERICINST`, `BYREF`, `PTR`, `SZARRAY`, `ARRAY`,
`INTERNAL`) and takes the `UInt8Source[]` the guest wrote, since the eight bytes are
`NativeIntByte`s rather than numbers. It recognises eight bytes of one source at indices 0..7 in
order, and refuses anything else, so a scrambled blob fails rather than decoding to a plausible
wrong type. It replaces the `SignatureDecoder` call for dynamic methods' method signatures
(`ModuleHandle_GetDynamicMethod`, `Delegate_BindToMethodInfo`) and locals signatures
(`DynamicMethodBody`).

Tests: a property that the walker inverts `SignatureHelper` for generated types over the whole
alphabet, with the host's `SignatureHelper` as the encoder and its `INTERNAL` handles mapped to
PawPrint's; a `sourcesImpure` guest (switch overridden) with a `DynamicMethod` over a
user-defined struct, a generic instantiation and an enum. The three-parameter
`Expression.Compile` guest goes into `sourcesPure` now, parked in `unimplemented`: it is a true
differential case, because both runtimes emit the thunk regardless of the switch.

### Stage 2: reflected methods in method position

Admit `RuntimeMethodHandle` and `GenericMethodInfo` scope entries for `call`, and admit
`callvirt` for them (a `callvirt` naming a `DynamicMethod` stays refused: it is a
`MissingMethodException` on real .NET). Resolution goes through the method handle registry, with
`GenericMethodInfo`'s type handle supplying the declaring instantiation. Acceptance is the owned
thunk probe, registered as a `sourcesImpure` case.

### Stage 3: anonymous hosting

The decision is below. Acceptance is the parked three-parameter guest, un-parked.

## Stage 3's decision: what a dynamic assembly is

PawPrint identifies a loaded assembly everywhere by its definition full name, and every name
resolves to a `DumpedAssembly`, which carries a mandatory `PEReader`. There is no case for an
assembly without an image. Three structurally different answers:

**A. Give the dynamic assembly real metadata.** At `AppDomain_CreateDynamicAssembly`, build a
minimal ECMA-335 image host-side with `MetadataBuilder` (an `Assembly` row carrying the requested
name, version, culture and key; a `Module` row; the `<Module>` type) and load it as an ordinary
`DumpedAssembly`, recording beside it that it is dynamic and with which `AssemblyBuilderAccess`,
so `IsDynamic`, `Location` and friends answer truthfully. This is CoreCLR's own model:
`Assembly::CreateDynamic` defines the assembly row in a writable in-memory metadata emitter
(`IMetaDataAssemblyEmit::DefineAssembly`, vm/assembly.cpp) and gives the assembly a
`ReflectionModule` over it, so a dynamic assembly there is not image-less either. Everything keyed
by assembly name then works unchanged. Cost: one QCall, the image builder, the dynamic-provenance record and its readers.
`TypeBuilder` would later have to *extend* that metadata; that is the separate ~30-QCall project
and this does not pre-empt its design.

**B. A new identity case.** Make "a loaded assembly" a DU, `FromImage of name | Dynamic of id`,
throughout: `LoadedAssembly`, `ResolvedTypeIdentity.DefiningAssemblyFullName`, the
`RuntimeAssembly`/`RuntimeModule` caches, `AssemblyHandle`/`ModuleHandle` native ints. The most
explicit about the difference, and the compiler would find every site. But it is a
whole-interpreter change, and the difference it encodes is one CoreCLR does not make: a dynamic
assembly there *has* metadata, which is why metadata queries against it work.

**C. Special-case the anonymous host.** Map the anonymously hosted module onto an assembly that
already exists, such as CoreLib. Smallest by far, but `Module.Assembly` and `IsDynamic` then
lie about identity, which AGENTS.md rules out.

**Chosen: A** (2026-09-26). It matches CoreCLR's own model, preserves identity, and has a small
blast radius. B's cost buys a distinction the runtime being emulated does not draw. C is listed
for completeness.

The `AssemblyLoadContext` half is not contentious. `InitializeAssemblyLoadContext` gets
implemented for the default context only, returning a native-int identity for its binder.
A collectible context is refused, because `LoaderAllocator` documents that PawPrint has one
arena and that this QCall is where a second would begin. A custom non-collectible context is
also refused: loading into a second context is not modelled.

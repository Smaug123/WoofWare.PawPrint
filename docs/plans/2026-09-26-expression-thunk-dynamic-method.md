# Running the expression interpreter's emitted thunk

Status: all stages implemented (1 and 2 on 2026-09-26, 3a and 3b on 2026-09-30). Measured on `main` at 30a3051e, and each probe
re-run after each stage. Tracking issue: #849. Motivated by the ASP.NET ladder's rung L
(`docs/plans/2026-08-17-aspnet-critical-path.md` on `aspnet-ladder`).

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

Measured after stage 1: the owned-thunk probe gets past `ModuleHandle_GetDynamicMethod`'s
signature and stops decoding the body, at the `callvirt` naming `Func<object[], object>::Invoke`
(stage 2). The three-parameter guest still stops at `AssemblyNative_InitializeAssemblyLoadContext`
(stage 3), as it must, since the anonymous hosting comes before the thunk is ever built.

### Stage 2: reflected methods in method position

`call` and `callvirt` accept `RuntimeMethodHandle` and `GenericMethodInfo` scope entries, resolved
through the method handle registry to the method's metadata identity. That identity already carries
the declaring instantiation, so `GenericMethodInfo`'s type handle is required to agree with it
rather than used, as for `GenericFieldInfo`. The callee then takes the metadata path's own last
steps, now shared between the two: `call` of an abstract method is "Bad IL format.", and `callvirt`
checks its receiver for null and dispatches on it.

As implemented, `callvirt` accepts a `DynamicMethod` entry as well, rather than refusing it: real
.NET answers a `callvirt` of any static method with `MissingMethodException` ("Method not found:
'?'."), and a `DynamicMethod` is always static, so one rule about the resolved callee covers both
(both measured). Also measured, against controls that run: a method of an open generic type
definition is an `InvalidProgramException`, and a generic method definition with no instantiation
runs with its type parameters unbound (`Array.Empty<T>`'s definition throws
`TypeInitializationException`), which PawPrint refuses rather than models. Real .NET makes no
visibility check on a dynamic method's callee: a private method of an unrelated type runs, owned or
anonymously hosted, without `restrictedSkipVisibility`. So PawPrint needs none.

`ldftn`, `ldvirtftn` and `newobj` naming a reflected method stay refused; the thunk uses none of
them. `sourcesImpure/DynamicMethodReflectedCalls.cs` is the acceptance case, and includes the owned
thunk.

Measured after stage 2: the owned-thunk probe answers 42. The three-parameter guest still stops at
`AssemblyNative_InitializeAssemblyLoadContext` (stage 3).

### Stage 3: anonymous hosting

The decision is below. Acceptance is the parked three-parameter guest, un-parked. Split in two,
because the load-context half is independently observable and needs none of the image building.

**3a: the default `AssemblyLoadContext`.** `AssemblyNative_InitializeAssemblyLoadContext` returns
`NativeIntSource.AssemblyBinderPtr AssemblyBinder.Default` for the default context and refuses the
other three combinations of its two flags, as below. The binder is stateless: CoreCLR's default
binder keeps the context's GC handle only to raise `Resolving` events, which PawPrint's binding
never does, and it only `_ASSERTE`s against a second default context, so there is nothing to
record. `sourcesPure/AssemblyLoadContextDefault.cs` is the acceptance case (all seven checks
measured on real .NET), and `TestAssemblyLoadContextRefusals` pins the two custom-context refusals.

Measured after 3a: the three-parameter guest stops at `AppDomain_CreateDynamicAssembly` (3b).

**3b: the dynamic assembly.** Option A below, as implemented:

* `DynamicAssemblyImage.build` makes a minimal PE image holding what CoreCLR's emit scope holds
  when `Assembly::CreateDynamic` returns: a `Module` row named `RefEmit_InMemoryManifestModule`,
  the `Assembly` row with the requested name, version, culture, flags and hash algorithm (SHA1 when
  none is given), and `<Module>`. It is read back through `Assembly.read` like any image. As the
  metadata emitter does, a version component of 65535 and a hash algorithm of -1 are read as "not
  given" and stored as 0 (measured).
* The provenance record lives in `LoadedAssemblies` (`WithDynamicAssembly`, `IsDynamic`): a dynamic
  assembly is in the load context but invisible to the binder, so the exact-identity fallback in
  `TryResolveReference` never finds one and `WithBoundReference` refuses to bind to one. That is
  CoreCLR's arrangement, where the binder does not consult dynamic assemblies.
* Its readers are `RuntimeAssembly.GetIsDynamic` (new) and `ModuleHandle_GetPEKind`, which answers
  `(0, 0)` for a dynamic module as `PEAssembly::GetPEKindAndMachine` does, rather than the headers
  PawPrint built. Everything else that reads an assembly already answers a dynamic one as real .NET
  does (all measured: empty `Location`, null `CodeBase`, no types, no references, metadata stream
  version 0x20000, present in `AppDomain.GetAssemblies`).
* The module version ID is a version-4 GUID over bytes drawn from the kernel's entropy pool, as
  `minipal_guid_v4_create` draws secure random bytes. Real .NET gives a different one each run. No
  guest can observe it yet, because `MetadataImport.GetScopeProps` is not implemented.
* Refused, although real .NET admits each (measured): a second dynamic assembly with an identity
  already loaded, whether dynamic or an image, because `LoadedAssemblies` keys on that identity; a
  collectible one (the second loader-allocator door); and one with a public key, because CoreCLR
  raises `SecurityException` for a key `StrongNameIsValidPublicKey` rejects and PawPrint does not
  yet validate keys (#1404 adds the validator); and one whose flags use bits above 15, because
  `MetadataBuilder` writes the four-byte `Flags` column from a `UInt16`. No flag ECMA-335 defines
  is up there.

`sourcesImpure/DynamicAssemblyHosting.cs` is the acceptance case, `TestDynamicAssemblyRefusals`
pins the five refusals, and `TestDynamicAssemblyImage` checks the image round-trips any identity.
All thirteen mutants of the rules above are caught.

Measured after 3b: the three-parameter guest exits 0 and is un-parked. A plain
`AssemblyBuilder.DefineDynamicAssembly` still stops at `AssemblyNative_GetLoadContextForAssembly`,
which it calls to find the caller's load context unless a contextual-reflection scope names one;
the anonymous host passes `AssemblyLoadContext.Default` and never asks.

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

# Design: Method-generic parameter RuntimeType support

## Why

`RuntimeTypeHandleTarget.GenericParameter` currently represents only **type**-generic
parameters (e.g. `T` in `class Box<T>`). Method-generic parameters (e.g. `TResult`
in `TResult Foo<TResult>()`) are explicitly noted as "not yet represented"
(`NativeIntSource.fs:88`). This blocks any managed code that reflects on a
generic method's type parameters:

```csharp
typeof(Foo).GetMethod("Bar").GetGenericArguments()[0].GenericParameterAttributes
```

The gap manifests at multiple sites:

| Site | File:line | Current behaviour |
|------|-----------|-------------------|
| DU definition | `NativeIntSource.fs:88` | Comment: "Method generic parameters are not yet represented" |
| `ldtoken !!n` | `IlMachineTypeResolution.fs:460-462` | `failwith "TODO: ldtoken for unbound generic method parameter"` |
| `GetDeclaringMethod` | `NativeRuntimeType.fs:2530-2540` | Always returns null (correct for type params, wrong for method params) |
| Constraint fold | `NativeRuntimeType.fs:1771-1773` | `failwith "... method-generic parameter constraint; impossible without a method context"` |

## Design

### 1. Identity representation

Add a new DU case to `RuntimeTypeHandleTarget`:

```fsharp
| MethodGenericParameter of
    declaringType : ResolvedTypeIdentity *
    declaringMethod : ComparableMethodDefinitionHandle *
    position : int
```

Why these three fields:

- **`declaringType`**: Provides the assembly name for `LoadedAssembly` lookups and
  the type identity for `GetDeclaringType`. Follows the type-generic `GenericParameter`
  pattern.
- **`declaringMethod`**: The method's `MethodDefinitionHandle` in the declaring type's
  assembly. Needed to look up `assembly.Methods.[handle].Generics` for the
  parameter metadata. Wrapped in `ComparableMethodDefinitionHandle` per the
  existing pattern for handle-carrying DU cases.
- **`position`**: Zero-based generic parameter index within the method's parameter list.

Why not extend the existing `GenericParameter` case with an `Option<ComparableMethodDefinitionHandle>`:
the two cases have genuinely different semantics at every consumer site
(`GetDeclaringType` returns the type vs. the method's declaring type;
`GetDeclaringMethod` returns null vs. a `RuntimeMethodInfo`;
`GetToken` looks up `typeInfo.Generics` vs. `methodInfo.Generics`).
Merging them would add branching at every match site rather than letting the
DU do its job.

### 2. Entry point: RuntimeMethodHandle.GetMethodInstantiation (QCall)

In CoreCLR, `RuntimeMethodInfo.GetGenericArguments()` calls:
```csharp
RuntimeMethodHandle.GetMethodInstantiationPublic(this)
```
which is backed by the QCall `RuntimeMethodHandle_GetMethodInstantiation`. This
returns a `RuntimeType[]` where each element represents one of the method's
generic parameters.

PawPrint must implement this QCall to construct `MethodGenericParameter` targets
for each position, allocate `RuntimeType` objects for them, and pack them into
a managed array. This mirrors how `RuntimeTypeHandle.GetInstantiation`
(`NativeRuntimeType.fs:1614-1644`) constructs `GenericParameter` targets for type
parameters.

The QCall takes `(RuntimeMethodHandleInternal method, ObjectHandleOnStack types,
BOOL fAsRuntimeTypeArray)`. PawPrint's method-handle registry maps
`MethodHandlePtr int64` → `MethodHandle` → `MethodDefinitionHandle`. From the
definition handle we can reach `assembly.Methods.[handle].Generics` to get the
count and metadata.

### 3. Entry point: `HasMethodInstantiation` (InternalCall)

`RuntimeMethodHandle.HasMethodInstantiation` is an InternalCall that returns true
if a method is generic. The managed `GetGenericArguments()` calls
`GetMethodInstantiationPublic` unconditionally, but `IsGenericMethod` checks
`HasMethodInstantiation`. Either way, this InternalCall needs implementation to
avoid a dispatch failure when managed code queries whether a method is generic.

### 4. Consumer site updates

All `match typeHandleTarget with` blocks that handle `GenericParameter` need a
`MethodGenericParameter` arm:

| Site | What `MethodGenericParameter` should do |
|------|-----------------------------------------|
| `ToString` | `"method generic parameter #position of method handle on declaringType"` |
| `typeDefinitionTokenOfRuntimeTypeHandleTarget` (GetToken) | Look up `assembly.Methods.[declaringMethod].Generics.[position].Handle` and return `MetadataToken.GenericParameter handle` |
| `declaringRuntimeType` (GetDeclaringType) | Return the RuntimeType for `declaringType` (the type containing the method) |
| `baseRuntimeType` (GetBaseType) | TODO — same status as type-generic params |
| `corElementType` | TODO — same status as type-generic params |
| `IsGenericVariable` | Return `true` |
| `GetGenericVariableIndex` | Return `position` |
| `GetDeclaringMethod` | Return the `IRuntimeMethodInfo` for the declaring method (non-null!) |
| `requireEmptyInterfaceMap` | TODO — same status as type-generic params |
| `instantiateGenericRuntimeTypeTarget` | Fail (parameters can't be instantiated) |
| `getOrAllocateTypeNameString` | Return the parameter name from `assembly.Methods.[declaringMethod].Generics.[position]` |
| `GetInstantiation` | Return empty (parameters have no instantiation) |
| `GetConstraints` | Read constraints from the method's generic parameter metadata |
| `MethodTableProjection` sites | TODO failwiths matching type-generic pattern |
| `Intrinsics.fs` (IsValueType, IsEnum) | Mirror type-generic constraint-flag logic |

### 5. What already works

- **`GenericParameter.readAll`** and **`ComparableGenericParameterHandle`**: Already called
  for method generics at `MethodInfo.fs:780`. The `Handle` field is populated for
  both type-generic and method-generic parameters.
- **`MetadataImport.GetGenericParamProps`**: Works with any 0x2A token. No changes needed.
- **`MetadataToken.GenericParameter`** and **`MetadataToken.ofInt`**: Handle both
  type and method parameter tokens.

### 6. Prerequisite: `Type.GetMethod()` reflection

An end-to-end test like `typeof(Foo).GetMethod("Bar").GetGenericArguments()[0]`
requires `Type.GetMethod()` to work. This is a non-trivial reflection path going
through `RuntimeType.GetMethodCandidates` → `RuntimeTypeHandle.GetMethodAt` (QCall)
→ method enumeration. Whether this path works today is unverified.

If `GetMethod` doesn't work yet, the test can't exercise method-generic parameters
through the reflection API. The alternatives are:
- Implement enough of the method reflection path to make `GetMethod` work (large scope)
- Find a simpler entry point that creates method-generic parameter RuntimeTypes
- Write infrastructure-only stages that are tested by "compiles and existing tests pass"

### 7. Out of scope

- **`ldtoken !!n` for unbound method generics.** This is an edge case for
  open generic method definitions in metadata inspection. Low priority.
- **Method-generic parameter constraints.** `GetConstraints` for method
  parameters reads from `MethodInfo.Generics` instead of `TypeInfo.Generics`
  but otherwise follows the same pattern.
- **Cross-assembly method-generic parameters.** Tokens are scoped to the
  declaring assembly; the `declaringType.Assembly` field provides the scope.

Implement this plan with each stage on its own branch, stacked as necessary on previous branches, so that a reviewer can review each branch in isolation.

Design: [method-generic-params.md](2026-05-09-method-generic-params.md)

## Stage 0: Probe whether `Type.GetMethod()` works

**Dependencies**: None

**Implements**: Design §6 (prerequisite verification)

Every end-to-end test for method-generic parameters begins with
`typeof(Foo).GetMethod("Bar")`. This goes through the BCL's
`RuntimeType.GetMethodCandidates` → `RuntimeTypeHandle.GetMethodAt` (QCall) →
method enumeration path. No existing pure test exercises `GetMethod`.

Write a minimal test `MethodReflectionProbe.cs` in `sourcesPure/`:

```csharp
class Foo {
    public static int Add(int a, int b) => a + b;
}
class Program {
    static int Main(string[] args) {
        var m = typeof(Foo).GetMethod("Add");
        if (m == null) return 1;
        if (m.Name != "Add") return 2;
        return 0;
    }
}
```

If this test passes on PawPrint: proceed to Stage 1.
If it fails: identify the missing InternalCall/QCall, implement it as a
**separate prerequisite PR** (not part of this plan), and revisit. The failure
message will pinpoint exactly which unimplemented primitive blocks the path.

**Correctness oracle**:
- The probe test passes on both PawPrint and the real runtime (exit code 0),
  or the failure is clearly identified and logged as a dependency.

**Result**: Probe failed. The blocker is:

```
Unimplemented native method (InternalCall):
  System.Private.CoreLib System.RuntimeTypeHandle::GetNumVirtuals(System.RuntimeType) -> System.Int32
```

`GetNumVirtuals` is part of `RuntimeType.GetMethodCandidates` — the BCL calls it
to enumerate methods on a type. Without it, `Type.GetMethod()` cannot work.

The probe test is registered in the `unimplemented` set in `TestPureCases.fs`.
Stage 1 (DU infrastructure) can proceed independently, but Stages 2+ are
blocked until `GetNumVirtuals` (and likely `GetMethodAt` and related method
enumeration InternalCalls/QCalls) are implemented.

---

## Stage 1: Add `MethodGenericParameter` DU case

**Dependencies**: Stage 0 (GetMethod works)

**Implements**: Design §1 (identity representation), §4 (consumer site updates)

Add `MethodGenericParameter of declaringType : ResolvedTypeIdentity *
declaringMethod : ComparableMethodDefinitionHandle * position : int` to
`RuntimeTypeHandleTarget` in `NativeIntSource.fs`.

Update every `match typeHandleTarget with` block that handles `GenericParameter`
to also handle `MethodGenericParameter`. Implement trivially where the answer is
obvious:

| Site | Implementation |
|------|---------------|
| `ToString` | Format string with method handle and position |
| `IsGenericVariable` | `true` |
| `GetGenericVariableIndex` | `position` |
| `GetInstantiation` | `ImmutableArray.Empty` |
| `instantiateGenericRuntimeTypeTarget` | Fail (parameters can't be instantiated) |

All other sites get TODO failwiths following the type-generic pattern, e.g.:
```fsharp
| RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
    failwith $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
```

The compiler will ensure exhaustive matching; every site that handles
`GenericParameter` must also handle `MethodGenericParameter`.

**Correctness oracle**:
- Project compiles with no new warnings.
- All existing tests pass. No change in unimplemented test count.
- The new DU case is exhaustively matched at every site (compiler-enforced).

---

## Stage 2: `GetToken` + `HasMethodInstantiation` + `GetMethodInstantiation`

**Dependencies**: Stage 1

**Implements**: Design §2 (GetMethodInstantiation entry point), §3
(HasMethodInstantiation), §4 (GetToken consumer)

Three pieces, all needed for the first testable end-to-end path:

### 2a. `typeDefinitionTokenOfRuntimeTypeHandleTarget` — MethodGenericParameter arm

Replace the TODO failwith with:
```fsharp
| RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
    let assembly =
        state.LoadedAssembly declaringType.Assembly
        |> Option.defaultWith (fun () ->
            failwith $"...")
    let methodInfo = assembly.Methods.[declaringMethod.Get]
    if position >= methodInfo.Generics.Length then
        failwith $"..."
    let param, _md = methodInfo.Generics.[position]
    MetadataToken.toInt (MetadataToken.GenericParameter param.Handle.Get)
```

This mirrors the type-generic `GenericParameter` arm but indexes into
`methodInfo.Generics` instead of `typeInfo.Generics`.

### 2b. `RuntimeMethodHandle.HasMethodInstantiation` InternalCall

Takes an `IRuntimeMethodInfo` (or `RuntimeMethodHandleInternal`), returns bool.
Look up the method via the method-handle registry, check whether its generic
parameter count is > 0.

### 2c. `RuntimeMethodHandle.GetMethodInstantiation` QCall

Takes `(RuntimeMethodHandleInternal method, ObjectHandleOnStack types, BOOL
fAsRuntimeTypeArray)`. Look up the method via the method-handle registry, read
its `MethodDefinitionHandle`, find the declaring type's `ResolvedTypeIdentity`,
construct `MethodGenericParameter(declaringType, methodHandle, position)` targets
for each position, allocate RuntimeType objects, pack into a managed array, and
write to the ObjectHandleOnStack.

This mirrors how `RuntimeTypeHandle.GetInstantiation` (NativeRuntimeType.fs:1614-1644)
constructs `GenericParameter` targets for type parameters.

### 2d. `getOrAllocateTypeNameString` — MethodGenericParameter arm

Replace the TODO with a lookup of the parameter name from
`assembly.Methods.[declaringMethod].Generics.[position]` (the `GenericParameter.Name`
field). This is needed because the managed `Type.Name` property reads the name
string, which flows through this path.

### Test

`MethodGenericParameterReflection.cs` in `sourcesPure/`:

```csharp
using System;
using System.Reflection;

namespace MethodGenericParameterReflection
{
    class Util
    {
        public static TResult Identity<TResult>(TResult x) => x;
        public static (TK, TV) Pair<TK, TV>(TK k, TV v) => (k, v);
    }

    class Program
    {
        static int Main(string[] args)
        {
            MethodInfo identity = typeof(Util).GetMethod("Identity");
            if (identity == null) return 1;

            Type[] gargs = identity.GetGenericArguments();
            if (gargs.Length != 1) return 2;

            Type t = gargs[0];
            if (!t.IsGenericParameter) return 3;
            if (t.GenericParameterPosition != 0) return 4;
            if ((t.MetadataToken >> 24) != 0x2A) return 5;
            // Unconstrained method generic param: attributes = None (0x0)
            if ((int)t.GenericParameterAttributes != 0x0) return 6;

            // Two-parameter generic method
            MethodInfo pair = typeof(Util).GetMethod("Pair");
            if (pair == null) return 7;
            Type[] pargs = pair.GetGenericArguments();
            if (pargs.Length != 2) return 8;
            if (pargs[0].GenericParameterPosition != 0) return 9;
            if (pargs[1].GenericParameterPosition != 1) return 10;
            // Distinct tokens
            if (pargs[0].MetadataToken == pargs[1].MetadataToken) return 11;

            return 0;
        }
    }
}
```

**Correctness oracle**:
- `MethodGenericParameterReflection.cs` passes on both PawPrint and the real runtime.
- All existing tests continue to pass.

---

## Stage 3: `GetDeclaringMethod` for method-generic parameters

**Dependencies**: Stage 2

**Implements**: Design §4 (`GetDeclaringMethod` consumer)

Replace the `GetDeclaringMethod` implementation (NativeRuntimeType.fs:2530-2540)
to distinguish between type-generic and method-generic parameters:

- `GenericParameter` → return null (unchanged)
- `MethodGenericParameter` → return the `IRuntimeMethodInfo` for the declaring method

This requires allocating a `RuntimeMethodInfoStub` for the declaring method,
using the method-handle registry. The existing `MethodHandleRegistry.getOrAllocate`
can produce the `RuntimeMethodHandle`; the stub wraps it as `IRuntimeMethodInfo`.

Expand the test to check `DeclaringMethod`:

```csharp
// Type-generic parameter: DeclaringMethod is null
Type typeParam = typeof(Box<>).GetGenericArguments()[0];
if (typeParam.DeclaringMethod != null) return 20;

// Method-generic parameter: DeclaringMethod is non-null
Type methodParam = typeof(Util).GetMethod("Identity").GetGenericArguments()[0];
if (methodParam.DeclaringMethod == null) return 21;
if (methodParam.DeclaringMethod.Name != "Identity") return 22;
```

**Correctness oracle**:
- Expanded test passes on both runtimes.
- The `DeclaringMethod` property correctly distinguishes type-generic (null)
  from method-generic (non-null) parameters.

---

## Stage 4: Constrained method-generic parameters

**Dependencies**: Stage 2

**Implements**: Design §4 (`GetConstraints` consumer, `Intrinsics.fs` sites)

Test method-generic parameters with constraints:

```csharp
class Constrained
{
    static T MakeNew<T>() where T : new() => new T();
    static void UseRef<T>(T x) where T : class { }
    static void UseVal<T>(T x) where T : struct { }
}
```

This exercises:
- `GenericParameterAttributes` for constrained method-generic parameters
  (already working from Stage 2 since `GetGenericParamProps` is token-based)
- Potentially `GetConstraints` if the test checks constraint types

Note: Stage 4 is independent of Stage 3 — both depend only on Stage 2.

**Correctness oracle**:
- Test `MethodGenericParameterConstraints.cs` passes on both runtimes.
- Attribute values match: `new()` → 0x10, `class` → 0x04, `struct` → 0x18.

---

## Risks and open questions

- **Stage 0 may fail.** `GetMethod` touches `RuntimeTypeHandle.GetMethodAt`,
  `RuntimeMethodHandle.GetAttributes`, and other QCalls/InternalCalls that may
  not be implemented. If Stage 0 fails, this entire plan is blocked behind a
  method-reflection prerequisite PR. The probe test's error message will identify
  exactly which primitive is missing.

- **`GetMethodInstantiation` QCall dispatch.** PawPrint's QCall dispatch
  mechanism may differ from its InternalCall dispatch. The implementation needs
  to match whatever pattern QCalls like `GetConstraints` and `GetInstantiation`
  use (ObjectHandleOnStack writes, managed array allocation).

- **Method-handle registry coverage.** The `MethodHandleRegistry` maps
  `MethodHandlePtr int64` → `MethodHandle`. For `GetMethodInstantiation` to
  work, the method must have been registered when it was first reflected upon.
  If `GetMethod` registers methods correctly, this should be fine. If not,
  the registration path needs extending.

- **`GetDeclaringMethod` return type.** The InternalCall returns
  `IRuntimeMethodInfo`, which is an interface. PawPrint needs to produce a
  `RuntimeMethodInfoStub` instance (or equivalent). The existing
  `MethodHandleRegistry.getOrAllocate` produces `RuntimeMethodHandle` structs;
  we may need the intermediate `RuntimeMethodInfoStub` allocation path.

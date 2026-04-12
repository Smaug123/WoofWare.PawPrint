# Managed Pointer Semantics: ObjectRef/Byref Deconflation and Flattening

## Problem statement

The current `ManagedPointerSource` type conflates two fundamentally different CLI concepts:

1. **Object references** (CLI `O` type, spec III.1.1.4): GC-tracked pointers to heap objects.
   A value in its own right — you hold an object reference, call methods on it, compare identity.
   Already represented correctly at the `CliType` level as `CliType.ObjectRef of ManagedHeapAddress option`.

2. **Managed pointers / byrefs** (CLI `&` type, spec III.1.1.5): GC-tracked pointers to *storage locations*.
   Created by `ldloca` (address of local), `ldarga` (address of argument), `ldflda` (address of field),
   `ldelema` (address of array element). You read from them (`ldind.*`/`ldobj`) and write to them
   (`stind.*`/`stobj`). They point at *where a value lives*, not at the value itself.

The conflation lives at the `EvalStackValue` level: `EvalStack.ofCliType` converts every
`CliType.ObjectRef (Some addr)` into `EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr)`,
erasing the type distinction. Pattern matches throughout the codebase then treat `ObjectRef addr` and
`ManagedPointer (Heap addr)` as interchangeable. Null object references become `ManagedPointerSource.Null`,
conflated with null managed pointers.

The `dereferencePointer` function for `Heap addr` returns the entire object's contents wrapped as
`CliType.ValueType result.Contents` — an operation the code itself labels "awfully dubious, this ain't
no value type" (IlMachineState.fs). This is a symptom: dereferencing a "managed pointer to an object"
is semantically undefined, because managed pointers point at locations (fields, elements, variables),
not at objects.

Additionally, the recursive structure of ManagedPointerSource (where `Field` and `InterpretedAsType`
wrap another `ManagedPointerSource`) makes write-back through projection chains difficult, producing
7-way matches with many `failwith "todo"` arms.

This plan addresses both problems in a single pass: deconflate ObjectRef from ManagedPointerSource,
and flatten the recursive structure into a root + projection list.

## Where the conflation lives

The ObjectRef-as-ManagedPointer smuggling occurs in these specific places:

1. **`EvalStack.ofCliType`** (EvalStack.fs): `CliType.ObjectRef (Some i)` → `ManagedPointer (Heap i)`,
   `CliType.ObjectRef None` → `ManagedPointer Null`. This is the root cause.

2. **`EvalStack.toCliTypeCoerced`** (EvalStack.fs): When target is `CliType.ObjectRef`, converts
   `EvalStackValue.ObjectRef ptr` to `ManagedPointerSource.Heap ptr |> CliRuntimePointer.Managed |>
   CliType.RuntimePointer`. This is backwards — it should return `CliType.ObjectRef`.

3. **`Newobj`** (UnaryMetadataIlOp.fs): Pushes `ManagedPointer (Heap allocatedAddr)` as `this` for
   the constructor, instead of `ObjectRef allocatedAddr`.

4. **Pattern matches** throughout: `| ObjectRef addr | ManagedPointer (Heap addr) ->` appears in
   AbstractMachine.fs, IlMachineState.fs, IlMachineStateExecution.fs, UnaryMetadataIlOp.fs,
   NullaryIlOp.fs, Intrinsics.fs, System.Threading.Monitor.fs, EvalStackValueComparisons.fs.

5. **`NativeIntSource.ManagedPointer`**: `toCliTypeCoerced` converts `NativeIntSource.ManagedPointer
   (Heap s)` back to `CliType.ObjectRef (Some s)`. NativeIntSource also smuggles ObjectRefs.

6. **`CliRuntimePointer.Managed`**: Delegate construction extracts `ManagedPointerSource.Heap target`
   from `CliRuntimePointer.Managed`.

## Target type design

### EvalStackValue (split null and non-null object references)

```fsharp
type EvalStackValue =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of NativeIntSource
    | Float of float
    | ManagedPointer of ManagedPointerSource
    | NullObjectRef
    | ObjectRef of ManagedHeapAddress
    | UserDefinedValueType of CliValueType
```

Using two top-level cases makes the null split compiler-visible at the `EvalStackValue` level.
After this change, `EvalStackValue.ObjectRef _` means "non-null object reference" only; null object
references must be matched as `EvalStackValue.NullObjectRef`.

This is stricter than `ObjectRef of ManagedHeapAddress option`: a broad `EvalStackValue.ObjectRef _`
pattern no longer accidentally covers null as well. To preserve that benefit, avoid `_ -> ...`
catch-all arms in `EvalStackValue` matches touched by this refactor.

Note: `CliType.ObjectRef of ManagedHeapAddress option` stays as-is — the null/non-null split is only
at the `EvalStackValue` level. `CliType` already had the correct separation; the conflation lived
entirely in how `EvalStackValue` represented object references. The `ofCliType` and `toCliTypeCoerced`
functions bridge the two representations:
- `CliType.ObjectRef None` ↔ `EvalStackValue.NullObjectRef`
- `CliType.ObjectRef (Some addr)` ↔ `EvalStackValue.ObjectRef addr`

### ManagedPointerSource (complete redesign)

```fsharp
/// The root storage location that a managed pointer points into.
[<NoComparison>]
type ByrefRoot =
    /// Address of a local variable slot on the stack.
    | LocalVariable of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a method argument slot on the stack.
    | Argument of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
    /// Address of a named field within a heap-allocated object.
    /// Created by `ldflda` on an ObjectRef.
    | HeapObjectField of obj : ManagedHeapAddress * fieldName : string
    /// Address of an indexed element within a heap-allocated array.
    /// Created by `ldelema`.
    | ArrayElement of arr : ManagedHeapAddress * index : int

/// A navigation step applied after reaching the byref root.
[<NoComparison>]
type ByrefProjection =
    /// Navigate to a named field within the current value.
    /// Created by `ldflda` on an existing managed pointer.
    | Field of fieldName : string
    /// Reinterpret the pointed-to value as a different type.
    /// Created by `Unsafe.As`.
    | ReinterpretAs of ConcreteType<ConcreteTypeHandle>

/// A managed pointer (byref / CLI `&` type).
/// Points at a storage location, not at an object.
[<NoComparison>]
type ManagedPointerSource =
    | Null
    | Byref of root : ByrefRoot * projections : ByrefProjection list

    override this.ToString () =
        let formatProj acc proj =
            match proj with
            | ByrefProjection.Field name -> $"<field %s{name} of {acc}>"
            | ByrefProjection.ReinterpretAs ty -> $"<{acc} as %s{ty.Namespace}.%s{ty.Name}>"
        match this with
        | ManagedPointerSource.Null -> "<null managed pointer>"
        | ManagedPointerSource.Byref (root, projs) ->
            let rootStr =
                match root with
                | ByrefRoot.LocalVariable (source, method, var) ->
                    $"<variable %i{var} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.Argument (source, method, var) ->
                    $"<argument %i{var} in method frame %O{method} of thread %O{source}>"
                | ByrefRoot.HeapObjectField (addr, fieldName) ->
                    $"<field %s{fieldName} of heap object %O{addr}>"
                | ByrefRoot.ArrayElement (arr, index) ->
                    $"<element %i{index} of array %O{arr}>"
            projs |> List.fold formatProj rootStr
```

### Key invariant

**`ManagedPointerSource` never carries a bare object reference.** Every heap-targeting byref root
specifies exactly what it points at: a named field (`HeapObjectField`) or an indexed element
(`ArrayElement`). There is no `Heap of ManagedHeapAddress` case with empty projections. The only way
to represent "I have a reference to a heap object" is `EvalStackValue.ObjectRef addr`.

This invariant is enforced structurally by the type: you cannot construct a `ByrefRoot` that points
at "an entire heap object" without specifying a field or element. The "awfully dubious" code path
in `dereferencePointer` becomes impossible.

### Helper module

```fsharp
[<RequireQualifiedAccess>]
module ManagedPointerSource =
    let appendProjection (projection : ByrefProjection) (src : ManagedPointerSource) : ManagedPointerSource =
        match src with
        | ManagedPointerSource.Null -> failwith "cannot project from null managed pointer"
        | ManagedPointerSource.Byref (root, projs) ->
            ManagedPointerSource.Byref (root, projs @ [projection])
```

Performance note: `projs @ [projection]` is O(n) in projection list length. In practice, projection
chains are very short (typically 0–2 elements). If this becomes a concern, store projections in
reverse order and reverse on use.

### Ancillary types needing audit

The ObjectRef-through-ManagedPointerSource conflation also leaks into types that carry
`ManagedPointerSource`:

1. **`NativeIntSource`** (BasicCliType.fs): Has `ManagedPointer of ManagedPointerSource`. After
   deconflation, if a native int needs to carry an object reference (e.g. from `conv.i` on an
   ObjectRef), it may need an `ObjectReference of ManagedHeapAddress option` case.

2. **`CliRuntimePointer`** (BasicCliType.fs): Has `Managed of ManagedPointerSource`. After
   deconflation, if a runtime pointer needs to carry an object reference (currently done via
   `Managed (Heap addr)`), it may need an `ObjectReference of ManagedHeapAddress option` case.

3. **`UnsignedNativeIntSource`** (BasicCliType.fs): Has `FromManagedPointer of ManagedPointerSource`.
   Audit whether it ever carries ObjectRefs.

These should be resolved during implementation by following compiler errors. The principle is the
same: if a type currently smuggles ObjectRefs through `ManagedPointerSource.Heap`, it needs its own
ObjectRef case.

### Constructor mapping (old → new)

| Old | New |
|-----|-----|
| `ManagedPointerSource.Null` (as null objref) | `EvalStackValue.NullObjectRef` — not a ManagedPointerSource |
| `ManagedPointerSource.Null` (as null byref) | `ManagedPointerSource.Null` (unchanged) |
| `ManagedPointerSource.LocalVariable(t,f,v)` | `ManagedPointerSource.Byref(ByrefRoot.LocalVariable(t,f,v), [])` |
| `ManagedPointerSource.Argument(t,f,v)` | `ManagedPointerSource.Byref(ByrefRoot.Argument(t,f,v), [])` |
| `ManagedPointerSource.Heap addr` (smuggled objref) | `EvalStackValue.ObjectRef addr` — not a ManagedPointerSource |
| `ManagedPointerSource.Field(Heap addr, name)` | `ManagedPointerSource.Byref(ByrefRoot.HeapObjectField(addr, name), [])` |
| `ManagedPointerSource.ArrayIndex(arr, i)` | `ManagedPointerSource.Byref(ByrefRoot.ArrayElement(arr, i), [])` |
| `ManagedPointerSource.Field(src, name)` (on existing byref) | `ManagedPointerSource.appendProjection (ByrefProjection.Field name) src` |
| `ManagedPointerSource.InterpretedAsType(src, ty)` | `ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs ty) src` |

The critical row is `Heap addr` (smuggled objref): this stops being a `ManagedPointerSource` entirely
and becomes an `ObjectRef`. And `Field(Heap addr, name)` — the common `ldflda`-on-ObjectRef pattern —
becomes the new `HeapObjectField` root directly, with no intermediate "bare heap pointer".

## Semantic rules

### Which IL instructions produce which eval stack types

| IL instruction | What it produces | Eval stack type |
|----------------|-----------------|-----------------|
| `newobj` | A new object | `ObjectRef addr` |
| `ldloc` / `ldarg` (reference type) | An object reference | `ObjectRef addr` or `NullObjectRef` |
| `ldnull` | Null object reference | `NullObjectRef` |
| `ldloca` | Address of local variable | `ManagedPointer (Byref(LocalVariable(...), []))` |
| `ldarga` | Address of argument | `ManagedPointer (Byref(Argument(...), []))` |
| `ldflda` on ObjectRef | Address of field in heap object | `ManagedPointer (Byref(HeapObjectField(addr, name), []))` |
| `ldflda` on ManagedPointer | Deeper projection into existing byref | `ManagedPointer (appendProjection (Field name) src)` |
| `ldelema` | Address of array element | `ManagedPointer (Byref(ArrayElement(arr, i), []))` |
| `ldind.*` / `ldobj` | Read value at managed pointer | `readManagedByref state src` |
| `stind.*` / `stobj` | Write value at managed pointer | `writeManagedByref state src value` |

### The `ldflda`-on-ObjectRef distinction

This is the most important semantic change. Currently, `ldflda` on an ObjectRef:
1. Creates `ManagedPointerSource.Heap addr` (bare pointer to whole object)
2. Wraps in `ManagedPointerSource.Field(ptr, fieldName)` (navigate to field)

After the refactor:
1. Creates `ManagedPointerSource.Byref(ByrefRoot.HeapObjectField(addr, fieldName), [])` directly

There is no intermediate "bare pointer to a whole object". The managed pointer is born already
pointing at a specific field. This eliminates the semantic hole.

When `ldflda` operates on an *existing managed pointer* (e.g. navigating to a sub-field of a struct
that's already pointed to by a byref), it appends a `ByrefProjection.Field`:
```
Byref(LocalVariable(t,f,v), []) + ldflda "x" → Byref(LocalVariable(t,f,v), [Field "x"])
Byref(HeapObjectField(addr,"outer"), []) + ldflda "inner" → Byref(HeapObjectField(addr,"outer"), [Field "inner"])
```

## Core API: readManagedByref and writeManagedByref

These replace `dereferencePointer`, `getFieldValue`, `setFieldValue`, and the 7-way matches in
`stind`, `Initobj`, etc. They should be placed in `IlMachineState.fs`.

### readManagedByref

```fsharp
let readManagedByref (state : IlMachineState) (src : ManagedPointerSource) : CliType =
    match src with
    | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
    | ManagedPointerSource.Byref (root, projs) ->
        let rootValue =
            match root with
            | ByrefRoot.LocalVariable (t, f, v) ->
                (getFrame t f state).LocalVariables.[int<uint16> v]
            | ByrefRoot.Argument (t, f, v) ->
                (getFrame t f state).Arguments.[int<uint16> v]
            | ByrefRoot.HeapObjectField (addr, fieldName) ->
                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.DereferenceField fieldName
            | ByrefRoot.ArrayElement (arr, index) ->
                getArrayValue arr index state
        projs |> List.fold (fun value proj ->
            match proj with
            | ByrefProjection.Field name ->
                match value with
                | CliType.ValueType vt -> CliValueType.DereferenceField name vt
                | v -> failwith $"could not find field {name} on non-ValueType {v}"
            | ByrefProjection.ReinterpretAs ty ->
                failwith $"TODO: reinterpret as type %s{ty.Assembly.Name}.%s{ty.Namespace}.%s{ty.Name}"
        ) rootValue
```

Note: `HeapObjectField` reads the specific field directly via `AllocatedNonArrayObject.DereferenceField`.
No "awfully dubious" wrapping of the entire object as a `CliType.ValueType`.

### writeManagedByref

```fsharp
let writeManagedByref (state : IlMachineState) (src : ManagedPointerSource) (newValue : CliType) : IlMachineState =
    match src with
    | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
    | ManagedPointerSource.Byref (root, []) ->
        // Direct write to the root location, no projections to navigate.
        match root with
        | ByrefRoot.LocalVariable (t, f, v) ->
            state |> setLocalVariable t f v newValue
        | ByrefRoot.Argument (t, f, v) ->
            failwith "TODO: set argument"
        | ByrefRoot.HeapObjectField (addr, fieldName) ->
            let updated =
                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.SetField fieldName newValue
            { state with ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap }
        | ByrefRoot.ArrayElement (arr, index) ->
            state |> setArrayValue arr newValue index
    | ManagedPointerSource.Byref (root, projs) ->
        // Projected write: read root, navigate projections, write new value, reconstruct backward.
        let rootValue, writeBack =
            match root with
            | ByrefRoot.LocalVariable (t, f, v) ->
                (getFrame t f state).LocalVariables.[int<uint16> v],
                (fun updated -> state |> setLocalVariable t f v updated)
            | ByrefRoot.Argument (t, f, v) ->
                (getFrame t f state).Arguments.[int<uint16> v],
                (fun _ -> failwith "TODO: set argument through projection")
            | ByrefRoot.HeapObjectField (addr, fieldName) ->
                (ManagedHeap.get addr state.ManagedHeap
                 |> AllocatedNonArrayObject.DereferenceField fieldName),
                (fun updated ->
                    let obj =
                        ManagedHeap.get addr state.ManagedHeap
                        |> AllocatedNonArrayObject.SetField fieldName updated
                    { state with ManagedHeap = ManagedHeap.set addr obj state.ManagedHeap })
            | ByrefRoot.ArrayElement (arr, index) ->
                getArrayValue arr index state,
                (fun updated -> state |> setArrayValue arr updated index)
        let updatedRoot = applyProjectionsForWrite rootValue projs newValue
        writeBack updatedRoot
```

With the recursive write-through helper:

```fsharp
let rec private applyProjectionsForWrite
    (rootValue : CliType) (projs : ByrefProjection list) (newValue : CliType) : CliType =
    match projs with
    | [] -> newValue
    | [ ByrefProjection.Field name ] ->
        CliType.withFieldSet name newValue rootValue
    | ByrefProjection.Field name :: rest ->
        let fieldValue = CliType.getField name rootValue
        let updatedField = applyProjectionsForWrite fieldValue rest newValue
        CliType.withFieldSet name updatedField rootValue
    | ByrefProjection.ReinterpretAs _ :: _ ->
        failwith "TODO: write through reinterpret"
```

Note on double-read for `HeapObjectField` with projections: the implementation reads the heap object
once to get the root value for navigation, then again to write back the updated value. Between these
two reads, nothing mutates the heap (the interpreter is single-threaded and deterministic).

## Prerequisites: characterization tests

Before any code changes, write C# test files exercising the patterns most at risk from this refactor.
Pure tests go in `sourcesPure/`, impure tests in `sourcesImpure/`.

Check which of these are already covered by existing test files. For any gaps, write new tests:

1. **ref to local variable** (read and write through ref):
   ```csharp
   static int RefToLocal() {
       int x = 42;
       ref int r = ref x;
       r = 100;
       return x; // should be 100
   }
   ```

2. **ref to field of heap object** (ldflda on ObjectRef):
   ```csharp
   class MyObj { public int X; }
   static int RefToHeapField() {
       MyObj c = new MyObj();
       c.X = 1;
       ref int r = ref c.X;
       r = 99;
       return c.X; // should be 99
   }
   ```

3. **ref to array element** (ldelema):
   ```csharp
   static int RefToArrayElement() {
       int[] arr = { 1, 2, 3 };
       ref int r = ref arr[1];
       r = 99;
       return arr[1]; // should be 99
   }
   ```

4. **ref to field of local struct** (ldflda on struct):
   ```csharp
   struct S { public int X; public int Y; }
   static int RefToStructField() {
       S s = new S { X = 1, Y = 2 };
       ref int r = ref s.X;
       r = 99;
       return s.X; // should be 99
   }
   ```

5. **Nested struct field byref** (ldflda chain):
   ```csharp
   struct Inner { public int Value; }
   struct Outer { public Inner In; }
   static int NestedStructByref() {
       Outer o = new Outer();
       o.In.Value = 42;
       ref Inner r = ref o.In;
       r.Value = 99;
       return o.In.Value; // should be 99
   }
   ```

6. **Monitor.Enter with ref bool** (impure, out-byref):
   ```csharp
   static int MonitorEnterRef() {
       object lockObj = new object();
       bool taken = false;
       System.Threading.Monitor.Enter(lockObj, ref taken);
       int result = taken ? 1 : 0;
       if (taken) System.Threading.Monitor.Exit(lockObj);
       return result; // should be 1
   }
   ```

Verify these pass on the existing code before starting the refactor. Any that fail reveal gaps in
current support — document them as known-failing and do not try to fix them as part of this refactor.
The goal is to establish a baseline, not to extend functionality.

## Implementation approach

Big-bang, compiler-guided, in a single branch.

Rationale:
- `ManagedPointerSource` is project-internal (not referenced from test, domain, or app projects).
- Splitting `EvalStackValue` object references into `NullObjectRef` and `ObjectRef` breaks every
  match site on object references (~66 occurrences across 9 files).
- Changing `ManagedPointerSource` breaks ~285 occurrences across 14 files.
- The F# compiler catches every broken pattern match and construction site.
- The existing test suite (33+ C# source files run on both real CLR and PawPrint) catches semantic
  regressions.
- The characterization tests from the prerequisites step provide additional coverage for the
  riskiest code paths.

The main risk is subtle semantic bugs in `readManagedByref`/`writeManagedByref` that are not
exercised by current tests. Mitigations: the new core functions are small and reviewable; they
replace code that was largely `failwith "todo"` for the `Field`, `InterpretedAsType`, and `Argument`
cases, so the semantic surface area is actually *decreasing*.

### Order of operations

1. Write and verify characterization tests (green baseline).
2. Split `EvalStackValue` object references into `NullObjectRef` and `ObjectRef of ManagedHeapAddress`.
3. Replace `ManagedPointerSource` type definition with new types.
4. Add `readManagedByref`, `writeManagedByref`, `appendProjection`.
5. Fix every compiler error (the bulk of the work — mechanical but large).
6. Audit `NativeIntSource`, `CliRuntimePointer`, `UnsignedNativeIntSource` for ObjectRef smuggling.
7. Build, test, format.

Steps 2–6 are effectively one pass since every match site breaks. The ordering within is conceptual,
not sequential — you'll be fixing compiler errors file by file.

## Key code path changes

### EvalStack.ofCliType (the root cause)

Currently converts ObjectRef to ManagedPointer. Fix:
```fsharp
| CliType.ObjectRef None -> EvalStackValue.NullObjectRef
| CliType.ObjectRef (Some addr) -> EvalStackValue.ObjectRef addr
```

Small boundary fix, most impactful change in the whole refactor. After this, ObjectRefs flow through the
system as `EvalStackValue.NullObjectRef`/`EvalStackValue.ObjectRef`, not as `ManagedPointer (Heap ...)`.

### EvalStack.toCliTypeCoerced

The `CliType.ObjectRef _` target currently converts everything through ManagedPointerSource. Fix:
```fsharp
| CliType.ObjectRef _ ->
    match popped with
    | EvalStackValue.NullObjectRef -> CliType.ObjectRef None
    | EvalStackValue.ObjectRef addr -> CliType.ObjectRef (Some addr)
    | EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L) -> CliType.ObjectRef None
    | EvalStackValue.ManagedPointer _ ->
        // A managed pointer is not an object reference. If this is reached,
        // something upstream created the wrong eval stack type.
        failwith "cannot coerce managed pointer to object reference"
    | ...
```

### UnaryMetadataIlOp.Newobj

Currently pushes `ManagedPointer (Heap allocatedAddr)`. Fix:
```fsharp
EvalStackValue.ObjectRef allocatedAddr
```

### UnaryMetadataIlOp.Ldflda

Currently creates `ManagedPointerSource.Heap addr` then wraps in `Field`. Fix:
```fsharp
let ptr =
    match ptr with
    | EvalStackValue.ObjectRef addr ->
        // ldflda on an ObjectRef creates a byref directly to the named field.
        ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, field.Name), [])
    | EvalStackValue.NullObjectRef ->
        failwith "TODO: throw NullReferenceException"
    | EvalStackValue.ManagedPointer src ->
        // ldflda on an existing byref appends a field projection.
        ManagedPointerSource.appendProjection (ByrefProjection.Field field.Name) src
    | _ -> failwith $"expected pointer type for ldflda"
```

Note: in the old code, `ldflda` on an ObjectRef created a bare `Heap addr` then wrapped it in
`Field(ptr, fieldName)` as a separate step. In the new code, `ldflda` on an ObjectRef creates
the `HeapObjectField(addr, fieldName)` root directly. The `ObjectRef → ManagedPointer` path and
the `ManagedPointer → ManagedPointer` path are now different code, enforcing the distinction.

### NullaryIlOp.stind

Currently a 7-way match. Fix:
```fsharp
| EvalStackValue.ManagedPointer src ->
    writeManagedByref state src (EvalStackValue.toCliTypeCoerced varType valueToStore)
```

One line replaces the entire match.

### UnaryMetadataIlOp.Initobj

Currently a 7-way match. Fix:
```fsharp
| EvalStackValue.ManagedPointer src ->
    writeManagedByref state src zeroOfType
```

### UnaryMetadataIlOp.Ldfld

Currently a 7-way match on ManagedPointerSource plus ObjectRef (lines 805–844). After deconflation,
ObjectRef is handled directly, and all ManagedPointer cases collapse into `readManagedByref`:
```fsharp
| EvalStackValue.ObjectRef addr ->
    let v =
        ManagedHeap.get addr state.ManagedHeap
        |> AllocatedNonArrayObject.DereferenceField field.Name
    IlMachineState.pushToEvalStack v thread state
| EvalStackValue.NullObjectRef ->
    failwith "TODO: throw NullReferenceException"
| EvalStackValue.ManagedPointer src ->
    let v =
        readManagedByref state
            (ManagedPointerSource.appendProjection (ByrefProjection.Field field.Name) src)
    IlMachineState.pushToEvalStack v thread state
```

Note: for `ObjectRef`, we go through `AllocatedNonArrayObject.DereferenceField` directly rather
than constructing a temporary `HeapObjectField` byref and calling `readManagedByref`. This avoids
an unnecessary managed-pointer round-trip for the common case.

### UnaryMetadataIlOp.Stfld

Currently a 7-way match (lines 610–648) with the same `ObjectRef addr | ManagedPointer (Heap addr)`
conflation pattern. After deconflation:
```fsharp
| EvalStackValue.ObjectRef addr ->
    let v =
        ManagedHeap.get addr state.ManagedHeap
        |> AllocatedNonArrayObject.SetField field.Name valueToStore
    { state with ManagedHeap = ManagedHeap.set addr v state.ManagedHeap }
| EvalStackValue.NullObjectRef ->
    failwith "TODO: throw NullReferenceException"
| EvalStackValue.ManagedPointer src ->
    writeManagedByref state
        (ManagedPointerSource.appendProjection (ByrefProjection.Field field.Name) src)
        valueToStore
```

### Pattern matches that extract heap addresses

Many sites currently match:
```fsharp
| EvalStackValue.ObjectRef addr
| EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr) -> ...
```

After deconflation, these simplify to:
```fsharp
| EvalStackValue.ObjectRef addr -> ...
| EvalStackValue.NullObjectRef -> ... // must handle null
```

The `ManagedPointer (Heap addr)` arm disappears — it was only reachable because of the conflation.

### IlMachineState delegate construction

The pattern that extracts heap addresses from `CliRuntimePointer.Managed (ManagedPointerSource.Heap
target)` simplifies. After deconflation, the ObjectRef is stored correctly, so the match becomes:
```fsharp
| CliType.ObjectRef (Some target) -> Some target
| CliType.ObjectRef None -> None
| CliType.RuntimePointer (CliRuntimePointer.Managed _) ->
    failwith "delegate target should be an ObjectRef, not a managed pointer"
```

### IlMachineStateExecution.getTypeOfObj

Currently does a 7-way match on ManagedPointerSource to extract the heap address for type lookup.
After deconflation, the ObjectRef case is handled directly:
```fsharp
| EvalStackValue.ObjectRef addr ->
    let o = ManagedHeap.get addr state.ManagedHeap
    state, o.ConcreteType
| EvalStackValue.NullObjectRef ->
    failwith "TODO: throw NullReferenceException"
| EvalStackValue.ManagedPointer _ ->
    failwith "cannot get type of managed pointer target"
```

## Edge cases and pitfalls

### 1. Distinguishing null ObjectRef from null managed pointer

Currently `ManagedPointerSource.Null` serves both roles. Every site that matches it must be
classified during the refactor:

- Sites checking "is this object reference null?" (e.g. `isinst`, `brfalse`, `ceq` with null):
  these should match `EvalStackValue.NullObjectRef`.
- Sites checking "is this managed pointer null?" (e.g. `stind` through null pointer, `Unsafe.NullRef`):
  these should match `ManagedPointerSource.Null`.
- Sites that handle both (e.g. `cgt.un` comparing pointers with null): need both cases.

The compiler will force this decision at every site.

### 2. EvalStackValueComparisons.ceq between ManagedPointer and ObjectRef

Currently (line 157–166), `ceq` compares `ManagedPointer (Heap src)` with `ObjectRef var1` by
checking `src = var1`. After deconflation, this path is unreachable — a `ManagedPointer` never
carries a bare heap address. The match arms become:

```fsharp
| EvalStackValue.NullObjectRef, EvalStackValue.NullObjectRef -> true
| EvalStackValue.ObjectRef addr1, EvalStackValue.ObjectRef addr2 -> addr1 = addr2
| EvalStackValue.NullObjectRef, EvalStackValue.ObjectRef _
| EvalStackValue.ObjectRef _, EvalStackValue.NullObjectRef -> false
| EvalStackValue.ManagedPointer p1, EvalStackValue.ManagedPointer p2 -> p1 = p2
| EvalStackValue.ManagedPointer _, EvalStackValue.NullObjectRef
| EvalStackValue.NullObjectRef, EvalStackValue.ManagedPointer _
| EvalStackValue.ManagedPointer _, EvalStackValue.ObjectRef _
| EvalStackValue.ObjectRef _, EvalStackValue.ManagedPointer _ ->
    // In CLI, ceq between O and & types is unspecified.
    // If this fires, investigate the upstream IL.
    failwith "ceq between managed pointer and object reference"
```

### 3. BinaryArithmetic.addInt32ManagedPtr

The previous sketch was too tied to "last projection wins". That misses the new
`ByrefRoot.HeapObjectField(addr, fieldName)` shape, where a direct heap-field byref has no trailing
`Field` projection at all.

The right approach is to **normalize the managed pointer into the immediate arithmetic target**
before doing pointer arithmetic. `BinaryArithmetic` does not fundamentally care how the pointer was
constructed; it cares whether it points at:
- null
- an array element
- a field within some containing value

Add private helper types local to `BinaryArithmetic.fs`:

```fsharp
type private FieldContainer =
    | HeapObject of ManagedHeapAddress
    | ByrefContainer of ManagedPointerSource

type private ArithmeticTarget =
    | NullTarget
    | ArrayTarget of ManagedHeapAddress * int
    | FieldTarget of FieldContainer * string
```

Then normalize `ManagedPointerSource` into `ArithmeticTarget`:

```fsharp
let private decomposeArithmeticTarget (ptr : ManagedPointerSource) : ArithmeticTarget =
    match ptr with
    | ManagedPointerSource.Null -> NullTarget
    | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
        ArrayTarget (arr, index)
    | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, fieldName), []) ->
        FieldTarget (HeapObject addr, fieldName)
    | ManagedPointerSource.Byref (root, projs) ->
        match List.rev projs with
        | ByrefProjection.Field fieldName :: revRest ->
            let parentPtr = ManagedPointerSource.Byref (root, List.rev revRest)
            FieldTarget (ByrefContainer parentPtr, fieldName)
        | ByrefProjection.ReinterpretAs _ :: _ ->
            failwith $"refusing to do pointer arithmetic on reinterpreted pointer: {ptr}"
        | [] ->
            // Bare root with no projections and not an ArrayElement or HeapObjectField —
            // i.e. LocalVariable or Argument with no field navigation.
            failwith $"refusing to do pointer arithmetic on a bare stack slot address: {ptr}"
```

And a helper to recover the containing value for field-layout queries:

```fsharp
let private getFieldContainerValue (state : IlMachineState) (container : FieldContainer) : CliValueType =
    match container with
    | HeapObject addr ->
        // We access .Contents to get the object's field layout as a CliValueType.
        // This is NOT the same as the "awfully dubious" dereferencePointer usage:
        // there, .Contents was returned as the *dereferenced value of a byref*;
        // here, we are reading the field layout to compute byte offsets for pointer arithmetic.
        (ManagedHeap.get addr state.ManagedHeap).Contents
    | ByrefContainer ptr ->
        match IlMachineState.readManagedByref state ptr with
        | CliType.ValueType vt -> vt
        | x -> failwith $"expected value-type container for field arithmetic, got {x}"
```

This lets `addInt32ManagedPtr` handle direct heap-field byrefs and projected byrefs uniformly:

```fsharp
match decomposeArithmeticTarget ptr with
| NullTarget -> Choice2Of2 v
| ArrayTarget (arr, index) ->
    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index + v), [])
    |> Choice1Of2
| FieldTarget (container, fieldName) ->
    let containerValue = getFieldContainerValue state container
    let offset, _ = CliValueType.GetFieldLayout fieldName containerValue

    match CliValueType.FieldsAt (offset + v) containerValue |> List.tryExactlyOne with
    | None -> failwith $"TODO: couldn't identify field at offset {offset + v}"
    | Some field ->
        let newFieldName = CliConcreteField.ToCliField(field).Name

        match container with
        | HeapObject addr ->
            ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, newFieldName), [])
            |> Choice1Of2
        | ByrefContainer parentPtr ->
            ManagedPointerSource.appendProjection (ByrefProjection.Field newFieldName) parentPtr
            |> Choice1Of2
```

This is the key point: do field arithmetic relative to the **containing value**, not relative to
whether the field name came from the root or the projection list.

### 4. BinaryArithmetic.sub.ManagedPtrManagedPtr

`sub.ManagedPtrManagedPtr` should reuse the same normalization. Array-element subtraction stays
direct, and field-pointer subtraction becomes "same container, subtract field offsets":

```fsharp
match decomposeArithmeticTarget ptr1, decomposeArithmeticTarget ptr2 with
| ArrayTarget (arr1, idx1), ArrayTarget (arr2, idx2) when arr1 = arr2 ->
    (idx1 - idx2) |> nativeint |> Choice2Of2
| FieldTarget (container1, fieldName1), FieldTarget (container2, fieldName2) when container1 = container2 ->
    let containerValue = getFieldContainerValue state container1
    let offset1, _ = CliValueType.GetFieldLayout fieldName1 containerValue
    let offset2, _ = CliValueType.GetFieldLayout fieldName2 containerValue
    (offset1 - offset2) |> nativeint |> Choice2Of2
| _, _ -> failwith "TODO"
```

This preserves the new invariant while restoring the old semantic uniformity: `&obj.X`,
`&s.X`, `&arr[i].X`, and `&obj.Outer.Inner` all participate in pointer arithmetic through the same
decomposition path.

### 5. Unsafe.As with ObjectRef input

Currently handles `EvalStackValue.ObjectRef addr` by wrapping in `ManagedPointerSource.Heap addr`.
`Unsafe.As<TFrom, TTo>(ref TFrom)` takes a managed pointer, not an object reference. After
deconflation, if an ObjectRef arrives, either:
- It's dead code (upstream correctly produces ManagedPointer after the fix) — replace with an error.
- It's a real case that needs thought — investigate the C# that produces it.

### 6. Structural equality on ManagedPointerSource

The current type has `[<NoComparison>]` but NOT `[<NoEquality>]`. Structural equality is used in
`ceq` at `EvalStackValueComparisons.fs`. The new types must preserve this:
- `[<NoComparison>]` on `ByrefRoot`, `ByrefProjection`, `ManagedPointerSource`.
- Do NOT add `[<NoEquality>]`.
- `ByrefProjection list` comparison works correctly: F# lists support structural equality by default.
- `ByrefProjection.ReinterpretAs` contains `ConcreteType<ConcreteTypeHandle>` — the current type
  already has this in `InterpretedAsType` and works with structural equality (no `[<NoEquality>]`
  attribute), so the new type preserves this.

### 7. Projection chains and value-type fields only

The fold in `readManagedByref` expects `CliType.ValueType vt` when navigating `ByrefProjection.Field`.
This is correct: projection chains only make sense through value-type fields. If a field holds an
ObjectRef (reference type), you'd use `ldfld` to read it and then `ldflda` on the result — creating
a new `HeapObjectField` root, not extending the projection chain. The IL does not chain `ldflda`
through reference-type fields.

If the fold encounters a non-ValueType during projection, it means the managed pointer was
constructed from incorrect IL. The `failwith` is appropriate.

### 8. System.Threading.Monitor

`Monitor.ReliableEnter` currently does a 7-way match to write `true` to the out-byref parameter.
After the refactor:
```fsharp
writeManagedByref state outVar (CliType.ofBool true)
```

The lock object extraction simplifies from `| ObjectRef addr | ManagedPointer (Heap addr) ->` to
just `| ObjectRef addr ->`, with `NullObjectRef` handled separately.

## Verification

1. `nix develop -c dotnet build` — compiler catches all structural breakage.
2. `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`
   — full test suite including characterization tests.
3. `nix develop -c dotnet fantomas .` — reformat.
4. Grep verification (all should return zero hits):
   - `ManagedPointerSource\.Heap[^O]` (case removed; `HeapObjectField` is on `ByrefRoot`)
   - `ManagedPointerSource\.ArrayIndex`
   - `ManagedPointerSource\.Field`
   - `ManagedPointerSource\.InterpretedAsType`
   - `ManagedPointerSource\.LocalVariable`
   - `ManagedPointerSource\.Argument`
5. Grep for remaining conflation patterns (should return zero hits):
   - `ObjectRef addr\n.*ManagedPointer.*Heap addr` (the old interchangeable matching)
   - `ofCliType.*ObjectRef.*ManagedPointer` (the root cause conversion)

## What this plan does NOT do

- **Extend functionality.** The refactor preserves existing semantics. `failwith "TODO"` arms that
  existed before (e.g. `Argument` write-through) remain as `failwith "TODO"`.
- **Change CliType.** The `CliType` level already separates ObjectRef and RuntimePointer correctly.
  No changes needed there.
- **Fix all pointer arithmetic.** The `BinaryArithmetic` rewrite is mechanical; pointer arithmetic
  semantics remain the same.
- **Implement ReinterpretAs.** `Unsafe.As` is still `failwith "TODO"` for read-through and
  write-through. The projection representation supports it; implementation is future work.

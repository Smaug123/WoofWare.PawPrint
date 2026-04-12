# Problem statement

  ManagedPointerSource serves multiple roles: pointer to a local variable, pointer to an argument, pointer
  into the heap, pointer into an array, pointer to a field, null, and "interpreted as type". These have
  very different lifetime and aliasing semantics. The real CLR uses separate representations internally.
  Having them all in one DU is convenient now, but the dereferencePointer and setFieldValue functions
  already have large case-explosions that will get worse. Consider splitting heap pointers (which are
  GC-tracked, stable addresses) from stack pointers (which are frame-local and temporary).

# Detailed Implementation Plan: Refactor ManagedPointerSource

Executive Summary

This plan covers refactoring the recursive ManagedPointerSource discriminated union into a flat,
projection-based representation. The type has 196 occurrences across 13 files, all within the
WoofWare.PawPrint project (no occurrences in WoofWare.PawPrint.Domain, WoofWare.PawPrint.App, or
WoofWare.PawPrint.Test). The existing test suite does not reference ManagedPointerSource directly,
confirming this is a purely internal refactor.

Recommendation: Big-bang approach

Rationale:

1. The type is project-internal (not referenced from other projects or tests).
2. The F# compiler will catch every broken pattern match and construction site. With 196 occurrences
across 13 files this is large but entirely compiler-guided.
3. An incremental approach with old-to-new conversion shims would introduce temporary allocation
overhead (converting between representations at boundaries) and more total lines of code changed
across two or three PRs.
4. The FrameId plan in docs/plans/2026-04-11-frame-id.md established a precedent for mechanical,
compiler-guided type changes in this codebase.
5. The comprehensive test suite (33 C# source files compiled and executed on both the real CLR and
the PawPrint interpreter) will catch any semantic regression.

The main risk of big-bang is introducing a subtle semantic bug in the
readManagedByref/writeManagedByref implementation that happens not to be exercised by current tests.
Mitigations: the new core functions are small and can be thoroughly reviewed; most call sites
become simpler (just calling the new API instead of doing 7-way matches).

Step 0: Preparation

Before touching any code:
- Run dotnet build and dotnet test to confirm a green baseline.
- Create a branch: refactor/managed-pointer-source.

Step 1: Define the new types in BasicCliType.fs (lines 39-59)

Replace the existing ManagedPointerSource definition (lines 39-59) with the new types. All four
types must go in this exact location because:
- They depend on ThreadId, FrameId, ManagedHeapAddress (defined in AbstractMachineDomain.fs, which
compiles earlier).
- They depend on ConcreteType<ConcreteTypeHandle> (defined in ConcreteType.fs in the Domain
project).
- They must appear before UnsignedNativeIntSource (line 62) and NativeIntSource (line 67), which
reference ManagedPointerSource.

The new definitions:

[<NoComparison>]
type StackByrefRoot =
   | LocalVariable of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16
   | Argument of sourceThread : ThreadId * methodFrame : FrameId * whichVar : uint16

[<NoComparison>]
type HeapByrefRoot =
   | Object of ManagedHeapAddress
   | ArrayElement of arr : ManagedHeapAddress * index : int

[<NoComparison>]
type ByrefProjection =
   | Field of fieldName : string
   | ReinterpretAs of ConcreteType<ConcreteTypeHandle>

[<NoComparison>]
type ManagedPointerSource =
   | Null
   | Stack of StackByrefRoot * ByrefProjection list
   | Heap of HeapByrefRoot * ByrefProjection list

   override this.ToString () =
       let formatProjections projs =
           projs
           |> List.fold (fun acc proj ->
               match proj with
               | ByrefProjection.Field name -> $"<field %s{name} of {acc}>"
               | ByrefProjection.ReinterpretAs ty -> $"<{acc} as %s{ty.Namespace}.%s{ty.Name}>"
           ) ""
       match this with
       | ManagedPointerSource.Null -> "<null pointer>"
       | ManagedPointerSource.Stack (root, projs) ->
           let rootStr =
               match root with
               | StackByrefRoot.LocalVariable (source, method, var) ->
                   $"<variable %i{var} in method frame %O{method} of thread %O{source}>"
               | StackByrefRoot.Argument (source, method, var) ->
                   $"<argument %i{var} in method frame %O{method} of thread %O{source}>"
           projs |> List.fold (fun acc proj ->
               match proj with
               | ByrefProjection.Field name -> $"<field %s{name} of {acc}>"
               | ByrefProjection.ReinterpretAs ty -> $"<{acc} as %s{ty.Namespace}.%s{ty.Name}>"
           ) rootStr
       | ManagedPointerSource.Heap (root, projs) ->
           let rootStr =
               match root with
               | HeapByrefRoot.Object addr -> $"%O{addr}"
               | HeapByrefRoot.ArrayElement (arr, index) -> $"<index %i{index} of array %O{arr}>"
           projs |> List.fold (fun acc proj ->
               match proj with
               | ByrefProjection.Field name -> $"<field %s{name} of {acc}>"
               | ByrefProjection.ReinterpretAs ty -> $"<{acc} as %s{ty.Namespace}.%s{ty.Name}>"
           ) rootStr

Key naming decision: Heap case name is reused (was Heap of ManagedHeapAddress, now Heap of
HeapByrefRoot * ByrefProjection list). This minimizes churn at sites that already match
ManagedPointerSource.Heap. Null is preserved as-is.

Note on [<RequireQualifiedAccess>]: The current type does NOT have this attribute, so unqualified
pattern matching is allowed (see BinaryArithmetic.fs lines 24-38 which use bare LocalVariable,
Argument, etc.). The new sub-types (StackByrefRoot, HeapByrefRoot, ByrefProjection) should NOT have
[<RequireQualifiedAccess>] either, to match the codebase style. However, since the DU cases of the
outer type change names/shapes, all existing unqualified pattern matches in BinaryArithmetic.fs will
fail to compile and must be updated.

Step 2: Add helper functions in BasicCliType.fs

Add immediately after the ManagedPointerSource type definition (and before UnsignedNativeIntSource):

[<RequireQualifiedAccess>]
module ManagedPointerSource =
   let appendProjection (projection : ByrefProjection) (src : ManagedPointerSource) :
ManagedPointerSource =
       match src with
       | ManagedPointerSource.Null -> failwith "cannot project from null pointer"
       | ManagedPointerSource.Stack (root, projs) -> ManagedPointerSource.Stack (root, projs @
[projection])
       | ManagedPointerSource.Heap (root, projs) -> ManagedPointerSource.Heap (root, projs @
[projection])

This helper replaces the old construction pattern ManagedPointerSource.Field(src, name) and
ManagedPointerSource.InterpretedAsType(src, ty).

Performance note: projs @ [projection] is O(n) in the projection list length. In practice,
projection chains are very short (typically 0-2 elements: e.g., a field of a field). If this ever
becomes a concern, projections could be stored in reverse order and reversed on use, but this is
premature optimization.

Step 3: Add readManagedByref and writeManagedByref in IlMachineState.fs

These replace dereferencePointer (line 1682), getFieldValue (line 1447), and setFieldValue (line
1461). They should be placed in roughly the same region of the file.

readManagedByref (replaces dereferencePointer):

let readManagedByref (state : IlMachineState) (src : ManagedPointerSource) : CliType =
   match src with
   | ManagedPointerSource.Null -> failwith "TODO: throw NRE"
   | ManagedPointerSource.Stack (root, projs) ->
       let rootValue =
           match root with
           | StackByrefRoot.LocalVariable (sourceThread, methodFrame, whichVar) ->
               (getFrame sourceThread methodFrame state).LocalVariables.[int<uint16> whichVar]
           | StackByrefRoot.Argument (sourceThread, methodFrame, whichVar) ->
               (getFrame sourceThread methodFrame state).Arguments.[int<uint16> whichVar]
       projs |> List.fold (fun value proj ->
           match proj with
           | ByrefProjection.Field name ->
               match value with
               | CliType.ValueType vt -> CliValueType.DereferenceField name vt
               | v -> failwith $"could not find field {name} on object {v}"
           | ByrefProjection.ReinterpretAs ty ->
               failwith $"TODO: interpret as type
%s{ty.Assembly.Name}.%s{ty.Namespace}.%s{ty.Name}, object %O{value}"
       ) rootValue
   | ManagedPointerSource.Heap (root, projs) ->
       let rootValue =
           match root with
           | HeapByrefRoot.Object addr ->
               let result = ManagedHeap.get addr state.ManagedHeap
               CliType.ValueType result.Contents
           | HeapByrefRoot.ArrayElement (arr, index) ->
               getArrayValue arr index state
       projs |> List.fold (fun value proj ->
           match proj with
           | ByrefProjection.Field name ->
               match value with
               | CliType.ValueType vt -> CliValueType.DereferenceField name vt
               | v -> failwith $"could not find field {name} on object {v}"
           | ByrefProjection.ReinterpretAs ty ->
               failwith $"TODO: interpret as type
%s{ty.Assembly.Name}.%s{ty.Namespace}.%s{ty.Name}, object %O{value}"
       ) rootValue

writeManagedByref (replaces setFieldValue, and some setLocalVariable/setArrayValue call sites):

let writeManagedByref (state : IlMachineState) (src : ManagedPointerSource) (newValue : CliType) :
IlMachineState =
   match src with
   | ManagedPointerSource.Null -> failwith "TODO: throw NRE"
   | ManagedPointerSource.Stack (root, projs) ->
       match projs with
       | [] ->
           match root with
           | StackByrefRoot.LocalVariable (sourceThread, methodFrame, whichVar) ->
               state |> setLocalVariable sourceThread methodFrame whichVar newValue
           | StackByrefRoot.Argument (sourceThread, methodFrame, whichVar) ->
               failwith "todo: set argument"
       | _ ->
           // Read root, apply projections to navigate and write, reconstruct backwards
           let rootValue =
               match root with
               | StackByrefRoot.LocalVariable (t, f, v) -> (getFrame t f
state).LocalVariables.[int<uint16> v]
               | StackByrefRoot.Argument (t, f, v) -> (getFrame t f state).Arguments.[int<uint16>
v]
           let updatedRoot = applyProjectionsForWrite rootValue projs newValue
           match root with
           | StackByrefRoot.LocalVariable (t, f, v) -> state |> setLocalVariable t f v updatedRoot
           | StackByrefRoot.Argument (t, f, v) -> failwith "todo: set argument"
   | ManagedPointerSource.Heap (root, projs) ->
       match projs with
       | [] ->
           match root with
           | HeapByrefRoot.Object addr ->
               // ... write directly to heap object
           | HeapByrefRoot.ArrayElement (arr, index) ->
               state |> setArrayValue arr newValue index
       | _ ->
           // Read root, navigate projections, write back
           ...

applyProjectionsForWrite is the key new helper for writing through projection chains. It reads the
root, navigates forward through projections to find the target, writes the new value, then
reconstructs backward. For a chain like [Field "x"; Field "y"], it reads root, gets field "x", gets
field "y", replaces field "y" with the new value in the "x" struct, then replaces field "x" with the
updated struct in the root.

let private applyProjectionsForWrite (rootValue : CliType) (projs : ByrefProjection list) (newValue
: CliType) : CliType =
   match projs with
   | [] -> newValue
   | [ByrefProjection.Field name] -> CliType.withFieldSet name newValue rootValue
   | ByrefProjection.Field name :: rest ->
       let fieldValue = CliType.getField name rootValue
       let updatedField = applyProjectionsForWrite fieldValue rest newValue
       CliType.withFieldSet name updatedField rootValue
   | [ByrefProjection.ReinterpretAs _] -> failwith "TODO: write through reinterpret"
   | ByrefProjection.ReinterpretAs _ :: _ -> failwith "TODO: write through reinterpret chain"

Step 4: Mechanical rewrite of construction sites (all 13 files)

Apply these transformations systematically. The compiler will flag every broken site.

Direct constructor replacements:







┌──────────────────────────────────────────┬────────────────────────────────────────────────────────
─┐
│                   Old                    │                           New
│
├──────────────────────────────────────────┼────────────────────────────────────────────────────────
─┤
│ ManagedPointerSource.Null                │ ManagedPointerSource.Null (unchanged)
│
├──────────────────────────────────────────┼────────────────────────────────────────────────────────
─┤
│ ManagedPointerSource.LocalVariable(t, f, │ ManagedPointerSource.Stack(StackByrefRoot.LocalVariable
│
│  v)                                      │ (t, f, v), [])
│
├──────────────────────────────────────────┼────────────────────────────────────────────────────────
─┤
│ ManagedPointerSource.Argument(t, f, v)   │ ManagedPointerSource.Stack(StackByrefRoot.Argument(t,
│
│                                          │ f, v), [])
│
├──────────────────────────────────────────┼────────────────────────────────────────────────────────
─┤
│ ManagedPointerSource.Heap addr           │ ManagedPointerSource.Heap(HeapByrefRoot.Object addr,
│
│                                          │ [])
│
├──────────────────────────────────────────┼────────────────────────────────────────────────────────
─┤
│ ManagedPointerSource.ArrayIndex(arr, i)  │ ManagedPointerSource.Heap(HeapByrefRoot.ArrayElement(ar
│
│                                          │ r, i), [])
│
├──────────────────────────────────────────┼────────────────────────────────────────────────────────
─┤
│ ManagedPointerSource.Field(src, name)    │ ManagedPointerSource.appendProjection
│
│                                          │ (ByrefProjection.Field name) src
│
├──────────────────────────────────────────┼────────────────────────────────────────────────────────
─┤
│ ManagedPointerSource.InterpretedAsType(s │ ManagedPointerSource.appendProjection
│
│ rc, ty)                                  │ (ByrefProjection.ReinterpretAs ty) src
│
└──────────────────────────────────────────┴────────────────────────────────────────────────────────
─┘

Construction sites by file:

- UnaryConstIlOp.fs (3 construction sites):
 - Line 592: ManagedPointerSource.LocalVariable(...) in Ldloca -- becomes
ManagedPointerSource.Stack(StackByrefRoot.LocalVariable(...), [])
 - Lines 606, 616: ManagedPointerSource.Argument(...) in Ldarga -- becomes
ManagedPointerSource.Stack(StackByrefRoot.Argument(...), [])
- EvalStack.fs (3 construction sites):
 - Line 138: ManagedPointerSource.Null -- unchanged
 - Line 139: ManagedPointerSource.Heap i -- becomes ManagedPointerSource.Heap(HeapByrefRoot.Object
i, [])
 - Lines 232, 274: ManagedPointerSource.Heap ptr -- becomes
ManagedPointerSource.Heap(HeapByrefRoot.Object ptr, [])
- NullaryIlOp.fs (2 construction sites):
 - Line 56: ManagedPointerSource.Heap managedHeapAddress -- becomes
ManagedPointerSource.Heap(HeapByrefRoot.Object managedHeapAddress, [])
 - Line 327: ManagedPointerSource.Null -- unchanged
- UnaryMetadataIlOp.fs (2 construction sites):
 - Line 864: ManagedPointerSource.Heap addr (in Ldflda for ObjectRef) -- becomes
ManagedPointerSource.Heap(HeapByrefRoot.Object addr, [])
 - Line 893: ManagedPointerSource.Field(ptr, field.Name) -- becomes
ManagedPointerSource.appendProjection (ByrefProjection.Field field.Name) ptr
 - Line 488: ManagedPointerSource.ArrayIndex(arrAddr, index) -- becomes
ManagedPointerSource.Heap(HeapByrefRoot.ArrayElement(arrAddr, index), [])
- Intrinsics.fs (3 construction sites):
 - Line 590: ManagedPointerSource.InterpretedAsType(src, to_) -- becomes
ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs to_) src
 - Line 593: ManagedPointerSource.InterpretedAsType(ManagedPointerSource.Heap addr, to_) -- becomes
ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs to_)
(ManagedPointerSource.Heap(HeapByrefRoot.Object addr, []))
 - Line 649: ManagedPointerSource.ArrayIndex(addr, 0) -- becomes
ManagedPointerSource.Heap(HeapByrefRoot.ArrayElement(addr, 0), [])
- IlMachineStateExecution.fs (2 construction sites):
 - Lines 415, 438: ManagedPointerSource.Null -- unchanged
- BinaryArithmetic.fs (1 construction site):
 - Line 35: ManagedPointerSource.Field(src, ...) -- becomes ManagedPointerSource.appendProjection
(ByrefProjection.Field ...) src

Step 5: Mechanical rewrite of pattern match sites

Category A: Sites that only care about "is this null or not?" or "extract the heap address"

These are the simplest rewrites. Examples:
- ManagedPointerSource.Null stays as-is
- ManagedPointerSource.Heap addr becomes ManagedPointerSource.Heap(HeapByrefRoot.Object addr, []) or
ManagedPointerSource.Heap(HeapByrefRoot.Object addr, _) depending on whether projections should be
ignored

Files: UnaryConstIlOp.fs (lines 105, 127, 149, 171), NullaryIlOp.fs (lines 126-128, 156-158, 327,
778-780, 841), Intrinsics.fs (lines 165-167, 400-401, 412-413, 645, 651),
EvalStackValueComparisons.fs (lines 92-95, 160-166), IlMachineState.fs (lines 1504-1514).

Category B: Sites that do a 7-way match and can be replaced by readManagedByref/writeManagedByref

This is where the refactor pays off most. These sites currently have 7 arms (with many failwith
"todo" arms for Field and InterpretedAsType). They become a single call to the new API.

Key sites:
- IlMachineState.dereferencePointer (line 1682): Replace entirely with readManagedByref. Keep
dereferencePointer as an alias for backward compatibility, or remove if all callers are updated.
- IlMachineState.getFieldValue (line 1447): Becomes readManagedByref state
(ManagedPointerSource.appendProjection (ByrefProjection.Field fieldName) obj). Or remove entirely if
callers can use readManagedByref directly.
- IlMachineState.setFieldValue (line 1461): Becomes writeManagedByref state
(ManagedPointerSource.appendProjection (ByrefProjection.Field fieldName) obj) v. Or remove.
- NullaryIlOp.stind (line 82): The 7-way match on src becomes writeManagedByref state src
(EvalStackValue.toCliTypeCoerced varType valueToStore).
- NullaryIlOp.Stind_ref (line 1026): Same pattern.
- UnaryMetadataIlOp.Initobj (line 1148): The 7-way match on src becomes writeManagedByref state src
zeroOfType.
- System.Threading.Monitor.ReliableEnter (line 80): The write to outVar becomes writeManagedByref
state outVar (CliType.ofBool true).

Category C: Sites that do a 7-way match but need specific root information

These cannot just call readManagedByref because they need the root type for dispatch (e.g., getting
a heap address for sync block operations):

- UnaryMetadataIlOp.Ldfld (line 805): Needs to distinguish heap objects (for field lookup on
AllocatedNonArrayObject) from stack byrefs. However, this can be refactored to: read the
ManagedPointerSource, check if it has a field projection appended, and use readManagedByref. The
current code manually inlines the field dereference; with the new API it becomes:
| EvalStackValue.ManagedPointer src ->
   let value = readManagedByref state (ManagedPointerSource.appendProjection (ByrefProjection.Field
field.Name) src)
   IlMachineState.pushToEvalStack value thread state
- But there is a subtlety: for heap objects (ManagedPointerSource.Heap(HeapByrefRoot.Object addr,
[])), the current code goes through AllocatedNonArrayObject.DereferenceField, which operates on the
AllocatedNonArrayObject directly. The new readManagedByref for HeapByrefRoot.Object wraps in
CliType.ValueType result.Contents, then ByrefProjection.Field calls CliValueType.DereferenceField on
it. This should be semantically equivalent since AllocatedNonArrayObject.DereferenceField delegates
to CliValueType.DereferenceField on .Contents. Verify this equivalence carefully.
- BinaryArithmetic.addInt32ManagedPtr (line 22): Needs field layout information to compute offset
arithmetic. This is inherently projection-aware and cannot be simplified into a simple
readManagedByref call. The rewrite is:
match ptr with
| ManagedPointerSource.Null -> Choice2Of2 v
| ManagedPointerSource.Stack _ -> failwith "refusing to add to a stack address"
| ManagedPointerSource.Heap (_, projs) ->
   match List.tryLast projs with
   | Some (ByrefProjection.Field fieldName) ->
       // Navigate to parent, compute field offset, find target field
       let parentPtr = ... // ptr without the last projection
       let obj = readManagedByref state parentPtr
       let offset, _ = CliType.getFieldLayout fieldName obj
       match CliType.getFieldAt (offset + v) obj with
       | None -> failwith "TODO: couldn't identify field at offset"
       | Some field ->
           ManagedPointerSource.appendProjection (ByrefProjection.Field
(CliConcreteField.ToCliField(field).Name)) parentPtr
           |> Choice1Of2
   | _ -> failwith "refusing to add to a non-field pointer"
- BinaryArithmetic.sub.ManagedPtrManagedPtr (line 102): Needs to compare roots and compute offsets.
Match on the new structure:
| ManagedPointerSource.Heap (HeapByrefRoot.ArrayElement (arr1, idx1), []),
 ManagedPointerSource.Heap (HeapByrefRoot.ArrayElement (arr2, idx2), []) when arr1 = arr2 ->
   (idx1 - idx2) |> nativeint |> Choice2Of2
- And for Field projections, extract the last projection on each side, verify they share a parent,
compute offset difference.

Category D: EvalStackValueComparisons.ceq (line 150, 160-166)

The equality check var1 = var2 on ManagedPointerSource will still work because ManagedPointerSource
uses structural equality (no [<NoEquality>] attribute). The new sub-types also need structural
equality. This should work by default for F# discriminated unions -- verify that ByrefProjection
list comparison works correctly (it will, since F# lists support structural equality).

The specific pattern matches on ManagedPointerSource.Heap src comparing with ObjectRef (line 160)
become:
| ManagedPointerSource.Heap (HeapByrefRoot.Object src, []) -> src = var1

Category E: IlMachineStateExecution.getTypeOfObj (line 48-58)

The 7-way match here is mostly failwith "todo". In the new shape:
| ManagedPointerSource.Heap (HeapByrefRoot.Object addr, []) ->
   let o = ManagedHeap.get addr state.ManagedHeap
   state, o.ConcreteType
| _ -> failwith "todo"

Step 6: File-by-file change summary

Ordered by F# compilation order from .fsproj:





#: 1
File: BasicCliType.fs
Occurrences: 19
Changes: Replace type definition; add appendProjection helper; update ToString; update
 NativeIntSource.isZero (line 93); update CliNumericType.ToBytes (line 147); update
CliRuntimePointer
 (line 163, no change to wrapping type)
────────────────────────────────────────
#: 2
File: EvalStack.fs
Occurrences: 8
Changes: Update ofCliType construction (lines 138-139); update toCliTypeCoerced matches (lines
229-246,
 264, 268, 274)
────────────────────────────────────────
#: 3
File: EvalStackValueComparisons.fs
Occurrences: 10
Changes: Update cgtUn null checks (lines 92-95); update ceq pointer comparisons (lines 150, 157-166)
────────────────────────────────────────
#: 4
File: IlMachineState.fs
Occurrences: 28
Changes: Replace dereferencePointer with readManagedByref; replace getFieldValue/setFieldValue with
 writeManagedByref; update other match sites (lines 1504-1514)
────────────────────────────────────────
#: 5
File: BinaryArithmetic.fs
Occurrences: 22
Changes: Rewrite addInt32ManagedPtr, mulInt32ManagedPtr; update all ManagedPtrManagedPtr
implementations;
 update sub.ManagedPtrManagedPtr
────────────────────────────────────────
#: 6
File: Intrinsics.fs
Occurrences: 12
Changes: Update Unsafe.As construction (lines 589-594); update MemoryMarshal.GetArrayDataReference
(lines
 644-651); update addr-extraction patterns (lines 165-167, 400-401, 412-413)
────────────────────────────────────────
#: 7
File: IlMachineStateExecution.fs
Occurrences: 9
Changes: Update getTypeOfObj (lines 48-58); update ManagedPointerSource.Null construction (lines
415, 438)
────────────────────────────────────────
#: 8
File: NullaryIlOp.fs
Occurrences: 23
Changes: Rewrite stind and Stind_ref to use writeManagedByref; update addr-extraction patterns in
array
 ops and LdLen and Throw
────────────────────────────────────────
#: 9
File: UnaryMetadataIlOp.fs
Occurrences: 35
Changes: Rewrite Ldfld (line 805) to use readManagedByref; update Ldflda construction (lines 864,
893);
 rewrite Initobj (line 1148) to use writeManagedByref; update Ldelema construction (line 488)
────────────────────────────────────────
#: 10
File: UnaryConstIlOp.fs
Occurrences: 7
Changes: Update null checks in branch ops (lines 105, 127, 149, 171); update Ldloca/Ldarga
construction
 (lines 592, 606, 616)
────────────────────────────────────────
#: 11
File: System.Threading.Monitor.fs
Occurrences: 16
Changes: Update ReliableEnter (lines 42-93) and Exit (lines 103-107)
────────────────────────────────────────
#: 12
File: AbstractMachine.fs
Occurrences: 1
Changes: Update delegate invocation addr extraction (line 57)



Step 7: Verification

1. dotnet build -- the compiler will catch all remaining type errors.
2. dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal -- run all
tests.
3. dotnet fantomas . -- reformat.
4. Grep verification:
 - grep -r "ManagedPointerSource\.LocalVariable\|ManagedPointerSource\.Argument\|ManagedPointerSour
ce\.ArrayIndex\|ManagedPointerSource\.Field\|ManagedPointerSource\.InterpretedAsType" should return
zero hits (these case names no longer exist on ManagedPointerSource).
 - grep -rn "failwith.*todo" WoofWare.PawPrint/IlMachineState.fs should have fewer hits than
before, since the Field/InterpretedAsType arms that were all failwith "todo" are now handled by the
projection-based API.

Step 8: Post-refactor cleanup

After the type change is green:
1. Consider whether getFieldValue and setFieldValue should remain as thin wrappers or be inlined at
their (now fewer) call sites.
2. Consider whether dereferencePointer should remain as an alias for readManagedByref or be removed.
3. The IArithmeticOperation interface (lines 12-16 of BinaryArithmetic.fs) still takes
ManagedPointerSource -- this is fine, the type name hasn't changed.

Potential Pitfalls

1. AllocatedNonArrayObject.DereferenceField vs readManagedByref for heap objects: The current Ldfld
code for ObjectRef/Heap goes through AllocatedNonArrayObject.DereferenceField, which calls
CliValueType.DereferenceField name f.Contents. The new readManagedByref for HeapByrefRoot.Object
addr does ManagedHeap.get addr ... |> fun r -> CliType.ValueType r.Contents then applies
ByrefProjection.Field which calls CliValueType.DereferenceField. These are equivalent, but the
intermediate wrapping in CliType.ValueType means the Ldfld code path will now go through
readManagedByref which matches on CliType.ValueType vt -> CliValueType.DereferenceField name vt.
This is the same code path. No semantic change.
2. Structural equality of new type: ByrefProjection list uses F# list structural equality, which
compares elements in order. ByrefProjection.Field contains a string (structural equality works).
ByrefProjection.ReinterpretAs contains ConcreteType<ConcreteTypeHandle> -- verify this supports
structural equality. Looking at the existing code, ConcreteType is defined in the Domain project; if
it does not support equality, ManagedPointerSource equality (used in ceq at line 168: var1 = var2)
would break. However, the current ManagedPointerSource already contains
ConcreteType<ConcreteTypeHandle> in the InterpretedAsType variant and has [<NoComparison>] but not
[<NoEquality>], so equality already works. The new structure preserves this.
3. BinaryArithmetic unqualified patterns: The current code uses | LocalVariable ... (unqualified) to
match ManagedPointerSource. After the refactor, these case names no longer exist on
ManagedPointerSource. The compiler will flag every one. They must become |
ManagedPointerSource.Stack (StackByrefRoot.LocalVariable ..., []) etc. This is verbose but correct.
4. Write-through for nested value types: The writeManagedByref function for projection chains must
reconstruct the entire chain from leaf to root. The helper applyProjectionsForWrite is recursive and
works correctly for value types. For reference types on the heap, the write goes through
ManagedHeap.set, which replaces the entire AllocatedNonArrayObject. This is correct because the heap
object is mutable at the ManagedHeap level.

Critical Files for Implementation

- /Users/patrick/Documents/GitHub/WoofWare.PawPrint2/WoofWare.PawPrint/BasicCliType.fs
- /Users/patrick/Documents/GitHub/WoofWare.PawPrint2/WoofWare.PawPrint/IlMachineState.fs
- /Users/patrick/Documents/GitHub/WoofWare.PawPrint2/WoofWare.PawPrint/BinaryArithmetic.fs
- /Users/patrick/Documents/GitHub/WoofWare.PawPrint2/WoofWare.PawPrint/UnaryMetadataIlOp.fs
- /Users/patrick/Documents/GitHub/WoofWare.PawPrint2/WoofWare.PawPrint/NullaryIlOp.fs



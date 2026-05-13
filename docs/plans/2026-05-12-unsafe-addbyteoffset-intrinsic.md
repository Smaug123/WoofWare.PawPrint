# Plan: implement `Unsafe.AddByteOffset(&, IntPtr)` JIT intrinsic

Date: 2026-05-12
Author: Claude
Status: Proposed
Branch: `unsafe-addbyteoffset-intrinsic` (off `rawdata-array-projection`)

## Context

The previous PR (`rawdata-array-projection`) taught
`RuntimeFieldProjection.tryProjectRawDataFieldAddress` to project
`RawData::Data` on arrays as
`Byref(ArrayElement(arr, 0), [ReinterpretAs byte; ByteOffset (-nint)])`
so that the canonical `+sizeof(nint)` skip used by `CastCache.TableData`
collapses cleanly to `&array[0]`.

With that in place, `NullDereferenceTest.cs` advances one step further
and trips the next blocker:

```
System.Exception : TODO: implement JIT intrinsic
                   System.Private.CoreLib System.Runtime.CompilerServices.Unsafe.AddByteOffset(&, System.IntPtr),
                   or add it to safeIntrinsics after reviewing its IL
   at WoofWare.PawPrint.IlMachineStateExecution.callMethod
       (IlMachineStateExecution.fs:769)
```

The reaching IL is from `CastCache.TableData`
(`src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/CastCache.cs:104-109`):

```csharp
private static ref int TableData(int[] table)
{
    // element 0 is used for embedded aux data
    return ref Unsafe.As<byte, int>(
        ref Unsafe.AddByteOffset(ref table.GetRawData(), (nint)sizeof(nint)));
}
```

`table.GetRawData()` lowers to `ldflda RawData::Data`, which our previous
PR projects as `[ReinterpretAs byte; ByteOffset -8]` over
`ArrayElement(table, 0)`. The next step calls
`Unsafe.AddByteOffset<byte>(ref byte source, IntPtr byteOffset)` with
`byteOffset = sizeof(nint) = 8` — and that intrinsic is not implemented.

`Unsafe.AddByteOffset`'s CoreCLR IL body
(`src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs:655-671`)
is just `throw new PlatformNotSupportedException()` decorated with
`[Intrinsic]` and the documentation block

```text
// ldarg.0
// ldarg.1
// add
// ret
```

i.e. CoreCLR replaces it at JIT time with raw byref + native-int
addition. PawPrint cannot execute the IL body (which would throw); we
must implement the intrinsic.

## Surface area

`Unsafe.cs` declares two `[Intrinsic]` overloads
(`Unsafe.cs:210-224` and `Unsafe.cs:661-671`):

```csharp
public static ref T AddByteOffset<T>(ref T source, nuint byteOffset)
public static ref T AddByteOffset<T>(ref T source, IntPtr byteOffset)
```

The `nuint` overload's non-CORECLR body just delegates to the `IntPtr`
overload after a `(void*)` cast; the `IntPtr` overload is the primitive.
On CoreCLR both surface as `[Intrinsic]` and either may be reached
directly from generated code. We must handle both.

Both have the same semantics:

- Input: a managed byref `ref T source`, a native-sized signed offset in
  bytes.
- Output: a managed byref `ref T` advanced by `byteOffset` bytes.
- The `T` view on the resulting byref is the same `T` as the input;
  changing the type view requires a separate `Unsafe.As`.

This is identical in shape to the existing `Unsafe.Add<T>(ref T, IntPtr)`
handler in `Intrinsics.fs:1800-1847`, except the offset is in bytes (not
in `sizeof(T)` units).

## Existing primitives

`ManagedPointerSource.addByteOffsetUnderReinterpret`
(`ManagedPointerSource.fs:510-520`) is exactly the operation we need:

```fsharp
let addByteOffsetUnderReinterpret
    (context : ByteOffsetNormalisationContext)
    (reinterpretAs : ConcreteType<ConcreteTypeHandle>)
    (byteOffset : int)
    (src : ManagedPointerSource)
    : ManagedPointerSource
    =
    src
    |> appendProjection (ByrefProjection.ReinterpretAs reinterpretAs)
    |> appendProjection (ByrefProjection.ByteOffset byteOffset)
    |> normaliseByteOffset context
```

It anchors the byte cursor under a `ReinterpretAs T` (where T is the
caller-visible type view), appends the byte offset, then runs the
canonical normalisation (which folds whole-cell offsets into the
array/string root and collapses ByteOffset pairs).

The interaction with the `RawData::Data` projection is:

- Input: `Byref(ArrayElement(arr, 0), [ReinterpretAs byte; ByteOffset -8])`
- Append `ReinterpretAs byte`: the existing collapse rule in
  `appendProjection` (`ManagedPointerSource.fs:390-396`) preserves the
  trailing `ByteOffset`, leaving `[ReinterpretAs byte; ByteOffset -8]`
  unchanged.
- Append `ByteOffset 8`: the pair-collapse rule
  (`ManagedPointerSource.fs:399-403`) detects `8 = -(-8)` and strips
  both, leaving `[ReinterpretAs byte]`.
- `normaliseByteOffset` is a no-op on the resulting shape.

Output: `Byref(ArrayElement(arr, 0), [ReinterpretAs byte])` — a clean
`ref byte` at element 0. The follow-up `Unsafe.As<byte, int>` then
replaces the trailing `ReinterpretAs byte` with `ReinterpretAs int`,
yielding `&table[0]` as `ref int` for the `HashShift`/`TableMask`/
`VictimCounter` accessors.

## Plan

1. Add a new intrinsic handler in `Intrinsics.fs`, immediately after
   the existing `Unsafe.Add` case (around line 1847):

   ```fsharp
   | "System.Private.CoreLib", "Unsafe", "AddByteOffset" ->
       // CoreCLR replaces this with raw byref + native-int addition; the
       // managed IL body throws PlatformNotSupportedException so we cannot
       // fall through to the standard call path. Both overloads
       // (IntPtr and UIntPtr) have the same semantics: advance the byref
       // by `byteOffset` bytes, preserving the static T view.
       // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L661
       let t =
           match Seq.toList methodToCall.Generics with
           | [ t ] -> t
           | _ -> failwith "bad generics Unsafe.AddByteOffset"

       match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
       | [ ConcreteByref tFromParam ; ConcreteIntPtr state.ConcreteTypes ],
         MethodReturnType.Returns (ConcreteByref tFromRet)
       | [ ConcreteByref tFromParam ; ConcreteUIntPtr state.ConcreteTypes ],
         MethodReturnType.Returns (ConcreteByref tFromRet) when tFromParam = t && tFromRet = t -> ()
       | _ ->
           failwith
               $"TODO: Unsafe.AddByteOffset: only the (ref T, IntPtr) and (ref T, UIntPtr) overloads are implemented; got params %A{methodToCall.Signature.ParameterTypes}"

       let offset, state = IlMachineState.popEvalStack currentThread state
       let src, state = IlMachineState.popEvalStack currentThread state

       let offset : int =
           match offset with
           | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
               if i < int64<int> System.Int32.MinValue || i > int64<int> System.Int32.MaxValue then
                   failwith
                       $"TODO: Unsafe.AddByteOffset: native-int byte offset %d{i} does not fit in Int32"
               int32<int64> i
           | EvalStackValue.Int32 i -> i
           | _ ->
               failwith
                   $"TODO: Unsafe.AddByteOffset: expected Verbatim NativeInt or Int32 byte offset, got %O{offset}"

       let srcPtr =
           match src with
           | EvalStackValue.ManagedPointer p -> p
           | _ ->
               failwith
                   $"TODO: Unsafe.AddByteOffset on non-ManagedPointer source byref: %O{src}"

       let tConcrete =
           match AllConcreteTypes.lookup t state.ConcreteTypes with
           | Some c -> c
           | None -> failwith $"Unsafe.AddByteOffset: T not concretised: %O{t}"

       // We use addByteOffsetUnderReinterpret rather than a bare
       // `addByteOffsetToByteView` because the source byref may not yet
       // carry a trailing `ReinterpretAs`/`ByteOffset` tail (e.g. a
       // freshly-loaded `ref int` with empty projections). Appending the
       // reinterpret first anchors the byte cursor; existing trailing
       // reinterprets collapse via `appendProjection`'s same-reinterpret
       // rule, so the no-op `ReinterpretAs T` over an already-T view is
       // idempotent.
       let arrElementSize =
           match srcPtr with
           | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, _), _) ->
               let obj = state.ManagedHeap.Arrays.[arr]
               if obj.Length = 0 then 0 else CliType.sizeOf obj.Elements.[0]
           | _ -> 0

       let normalisation =
           match srcPtr with
           | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, _), _) ->
               ByteOffsetNormalisationContext.withArrayElementSize arr arrElementSize
           | _ ->
               ByteOffsetNormalisationContext.fixedStrideRootsOnly

       let ptr =
           ManagedPointerSource.addByteOffsetUnderReinterpret normalisation tConcrete offset srcPtr

       state
       |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) currentThread
       |> IlMachineState.advanceProgramCounter currentThread
       |> Some
   ```

   Notes:
   - `ConcreteIntPtr` / `ConcreteUIntPtr` are active patterns in scope
     for `Unsafe.Add`; reuse them.
   - The offset accepts `Int32` (e.g. JIT-narrowed widening) defensively,
     but the IL we observe pushes `NativeInt (Verbatim ...)` from
     `conv.i` after `sizeof + ldc.i4`.
   - The normalisation context needs `arrElementSize` for the
     `ArrayElement` root so whole-cell byte offsets fold into the
     element index. For non-array roots, `fixedStrideRootsOnly` is the
     correct context (it normalises `HeapValue` etc. on a fixed stride).
   - `tConcrete` is the concrete view to reinterpret under. Looking up
     the handle is what `Unsafe.Add` does already (via
     `cliTypeZeroOfHandle`); for `AddByteOffset` we only need the
     concrete type, not the zero value, so a direct
     `AllConcreteTypes.lookup` is sufficient.

2. The handler does not need a `safeIntrinsics` entry: the managed IL
   throws, so we want our handler to fire instead of falling through.

3. The Unsafe namespace is `System.Runtime.CompilerServices.Unsafe`
   (declared in `System.Private.CoreLib`); the existing `Unsafe.Add`
   case uses `"System.Private.CoreLib", "Unsafe", "AddByteOffset"` —
   verify that the type-name matching strips the namespace prefix the
   same way (it does; the `Unsafe.Add` case is `"Unsafe", "Add"` not
   `"System.Runtime.CompilerServices.Unsafe", "Add"`).

## Tests

Add a focused unit test in `TestMethodTableProjection.fs`, alongside the
new `RawData data projects array as byte byref before element 0` test
from the previous PR:

```fsharp
[<Test>]
let ``Unsafe.AddByteOffset over RawData::Data on array collapses to &array[0]`` () : unit =
    // The CastCache.TableData composition: project RawData::Data on an
    // int[], advance by sizeof(nint) bytes via Unsafe.AddByteOffset,
    // reinterpret as int. Result should be a clean `ref int` at
    // `&array[0]`.
    ...
```

This test exercises the full collapse chain through the
`ManagedPointerSource` primitives, without invoking the full intrinsic
dispatch (which is exercised end-to-end when `NullDereferenceTest.cs`
clears).

For end-to-end coverage, `NullDereferenceTest.cs` should advance past
this intrinsic and hit the next blocker (whatever lies inside
`CastCache.TryGet` after the `TableData` call). Per AGENTS.md
incremental policy, leave `NullDereferenceTest.cs` in `unimplemented`
with a refreshed comment if a downstream blocker appears.

## Validation

1. Build + fantomas clean.
2. Run the new focused test (step 3 below) — should pass.
3. Run `dotnet test --filter Name~NullDereferenceTest` — observe whether
   it now passes or trips the next downstream blocker. Either outcome
   is acceptable; if a new blocker appears, refresh the comment in
   `TestPureCases.fs` and keep the test in `unimplemented`.
4. Run the full suite (687+ tests) — no regressions.
5. Commit on branch `unsafe-addbyteoffset-intrinsic` bundling the plan
   + implementation. Push and request review per `CLAUDE.md`.

## Risk

Low. The handler is a direct translation of an existing primitive
(`addByteOffsetUnderReinterpret`), and the collapse rules in
`appendProjection` are already exercised by the `RawData::Data` array
projection tests. The most likely failure mode is mishandling an
overload signature variant (e.g. the `nuint` overload's offset arrives
as `EvalStackValue.NativeInt` with a different tag); the test plan
above includes both `IntPtr` and `UIntPtr` overloads to flush that out.

namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeMarshal =
    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "GetLastPInvokeError",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 state.Kernel.LastPInvokeError) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "GetLastSystemError",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 state.Kernel.LastSystemError) ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "SetLastPInvokeError",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "Marshal.SetLastPInvokeError" instruction.Arguments.[0]

            state.MapKernel (fun kernel ->
                { kernel with
                    LastPInvokeError = error
                }
            )
            |> NativeHandlerResult.completed
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "SetLastSystemError",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void ->
            let error =
                NativeCall.int32Argument "Marshal.SetLastSystemError" instruction.Arguments.[0]

            state.MapKernel (fun kernel ->
                { kernel with
                    LastSystemError = error
                }
            )
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "MarshalNative_SizeOfHelper",
          "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            qCallGenerics.IsEmpty
            ->
            let operation = "MarshalNative_SizeOfHelper"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandle =
                NativeCall.qCallTypeHandleToConcreteTypeHandle operation state qCallHandle

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes typeHandle

            let throwIfNotMarshalable =
                match instruction.Arguments.[1] |> EvalStackValue.ofCliType with
                | EvalStackValue.Int32 0 -> false
                | EvalStackValue.Int32 _ -> true
                | other -> failwith $"%s{operation}: expected throwIfNotMarshalable as Int32, got %O{other}"

            match CliType.TryComputeMarshalSize state.ConcreteTypes state._LoadedAssemblies ctx.BaseClassTypes zero with
            | Result.Error (MarshalSizeError.NotMarshalable _) when throwIfNotMarshalable ->
                // CoreCLR's `MarshalNative_SizeOfHelper` (marshalnative.cpp:150) throws
                // `ArgumentException` (resource `IDS_CANNOT_MARSHAL`) for types it can't
                // marshal as unmanaged structures when `throwIfNotMarshalable` is set.
                // Mirror that with a guest exception so the caller's `try/catch` can handle it.
                NativeHandlerResult.raiseException ctx.BaseClassTypes.ArgumentException state
                |> Some
            | Result.Error (MarshalSizeError.NotMarshalable reason) ->
                // `throwIfNotMarshalable=false` path: CoreCLR falls through to
                // `MethodTable::GetNativeSize` and returns whatever the type loader recorded.
                // PawPrint doesn't compute that value yet, so surface a host failure with a
                // clear TODO until a real caller forces us to model it.
                failwith
                    $"TODO %s{operation}: throwIfNotMarshalable=false fall-through to GetNativeSize is not implemented; type rejected because %s{reason}"
            | Result.Error (MarshalSizeError.NotImplemented reason) ->
                // PawPrint hasn't implemented this marshalling case; CoreCLR would compute a
                // size successfully. Surface as a host TODO so the missing case is visible.
                failwith
                    $"TODO %s{operation}: unimplemented marshalling case (throwIfNotMarshalable=%b{throwIfNotMarshalable}): %s{reason}"
            | Result.Ok size ->
                let state =
                    IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size.Size)) ctx.Thread state

                NativeHandlerResult.completed state |> Some
        | "MarshalNative_TryGetStructMarshalStub",
          "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePointer (ConcreteFunctionPointer _)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UIntPtr) ],
          // The CoreLib declaration is `[return: MarshalAs(UnmanagedType.Bool)] bool`, which
          // the QCall PInvoke stub presents to us as an Int32 return (Win32 BOOL is 4 bytes).
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MarshalNative_TryGetStructMarshalStub"

            let methodTableArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let typeHandle = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let stubOutPtr =
                NativeCall.managedPointerOfPointerArgument operation "structMarshalStub" instruction.Arguments.[1]

            let sizeOutPtr =
                NativeCall.managedPointerOfPointerArgument operation "size" instruction.Arguments.[2]

            // CoreCLR's `MarshalNative_TryGetStructMarshalStub` (marshalnative.cpp:99-145)
            // has three branches: blittable (memmove fast path, *stub = NULL, *size = native
            // size, return TRUE), has-layout-non-blittable (synthesised IL stub, return TRUE),
            // and no-layout (return FALSE so managed Marshal throws ArgumentException).
            // This implementation handles the no-layout arm (AutoLayout types, which covers
            // `System.Object` and ordinary classes without `[StructLayout]`, as well as value
            // types explicitly marked `[StructLayout(LayoutKind.Auto)]`), and the first arm
            // for the strict subset we are confident matches CoreCLR exactly: structs whose
            // fields are recursively plain numeric (Int8..Float64), excluding host-known
            // field-only special cases (DateTime, Decimal) that CoreCLR's `MarshalInfo`
            // diverts to stub synthesis (`MARSHAL_TYPE_DATE`, `NFT_DECIMAL`). Anything else —
            // enums, [MarshalAs] descriptors, Bool/Char/ObjectRef fields,
            // has-layout-non-blittable structs, etc. — surfaces a host TODO. Each future
            // widening wants its own motivating PawPrint test before being added to the
            // classifier.

            if CliValueType.IsAutoLayoutHandle state.ConcreteTypes state._LoadedAssemblies typeHandle then
                // No-layout branch: write *stub = NULL, *size = 0, return FALSE so the
                // managed `Marshal.StructureToPtr` / `PtrToStructureHelper` / `DestroyStructure`
                // wrappers throw `ArgumentException` (resource `Argument_MustHaveLayoutOrBeBlittable`).
                let zeroNativeInt =
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

                let state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state stubOutPtr zeroNativeInt

                let state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state sizeOutPtr zeroNativeInt

                let state =
                    IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) ctx.Thread state

                NativeHandlerResult.completed state |> Some
            else

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes typeHandle

            // The classifier is split into two functions to encode the top-level-vs-field
            // distinction that CoreCLR's `MarshalInfo` makes. CoreCLR walks fields with
            // `IsFieldBlittable`, which short-circuits DateTime to `MARSHAL_TYPE_DATE`
            // (mlinfo.cpp:1747) and Decimal to marshal-stub synthesis (`NFT_DECIMAL` in
            // fieldmarshaler.cpp); neither of those host-known types is byte-image compatible
            // with its native form *when used as a field*, but their standalone byte images
            // can coincide with the native form (Decimal's standalone is byte-identical;
            // DateTime is filtered earlier by the AutoLayout gate). Top-level entry walks the
            // outer struct's fields via `isBlittableField`; the field walker rejects the
            // host-known special cases and recurses into nested structs via itself.
            let rec isBlittableField (t : CliType) : bool =
                match t with
                // `NativeInt` cells carry provenance under PawPrint (e.g.
                // `TypeHandlePtr` from `typeof(T).TypeHandle.Value`). CoreCLR
                // memmoves the integer-width bits regardless, but PawPrint's
                // byte model rejects non-`Verbatim` provenance because
                // `CliNumericType.ToBytes` cannot serialise it. We accept
                // `IntPtr`/`UIntPtr` here because the blittable arm returns a
                // null stub, instructing CoreLib to call
                // `SpanHelpers.Memmove(ref byte, ref byte, nuint)` — which
                // PawPrint intercepts and routes through `CellAwareCopy.copy`,
                // preserving whole-cell provenance when both endpoints anchor
                // on cell-aware roots. The hazard that remains is value-level:
                // a struct holding a non-`Verbatim` `IntPtr` marshalled to
                // `AllocHGlobal`'d native memory (a byte-only endpoint) still
                // falls back to the byte walk and surfaces the
                // `validateByteAddressableCell` failure there, not here.
                | CliType.Numeric (CliNumericType.NativeInt _) -> true
                | CliType.Numeric _ -> true
                | CliType.Bool _
                | CliType.Char _
                | CliType.ObjectRef _
                | CliType.RuntimePointer _ -> false
                | CliType.ValueType vt ->
                    // DateTime is structurally a single `ulong _dateData` and would otherwise
                    // qualify as strictly numeric, but CoreCLR's `MarshalInfo` (mlinfo.cpp:1747)
                    // special-cases DateTime fields as `MARSHAL_TYPE_DATE`: 8 bytes of OADate
                    // (`dt.ToOADate()` as a little-endian IEEE-754 double), NOT the managed
                    // `_dateData` byte image. The memmove fast path would silently emit the
                    // wrong bytes, so reject here and let the outer arm surface the existing
                    // TODO failwith. Implementing the OADate conversion belongs in a future
                    // PR that synthesises the has-layout-non-blittable IL stub.
                    let isDateTime =
                        CliValueType.IsHostKnownDateTime
                            state.ConcreteTypes
                            state._LoadedAssemblies
                            ctx.BaseClassTypes
                            vt

                    // Decimal is structurally four `Int32` fields (`flags`, `hi`, `lo`, `mid`)
                    // and would otherwise recurse to true, but CoreCLR's `MarshalInfo` routes
                    // Decimal fields through marshal-stub synthesis (`NFT_DECIMAL` in
                    // fieldmarshaler.cpp): managed `Decimal` is 16 bytes with 4-byte field
                    // alignment, native `DECIMAL` is 16 bytes with 8-byte alignment (its
                    // `Lo64` union member is `ULONGLONG`). The outer struct's managed layout
                    // therefore positions Decimal at a different offset than the native
                    // layout — `{ int x; decimal d; }` is 20 bytes managed, 24 bytes native.
                    // Memmoving would write into native padding. Reject here so the outer
                    // arm surfaces the TODO failwith; real handling needs the Decimal
                    // marshal stub and the matching 8-byte-aligned native layout.
                    let isDecimal =
                        CliValueType.IsHostKnownDecimal
                            state.ConcreteTypes
                            state._LoadedAssemblies
                            ctx.BaseClassTypes
                            vt

                    if isDateTime || isDecimal then
                        false
                    else
                        match vt._Storage with
                        // RawBytes-backed value types are not the typical struct-with-fields
                        // shape; conservatively reject so we don't quietly accept primitive
                        // wrappers whose CoreCLR marshal size diverges from the byte image.
                        | CliValueTypeStorage.RawBytes _ -> false
                        | CliValueTypeStorage.Fields storage ->
                            storage.Fields |> List.forall (fun field -> isBlittableField field.Contents)

            let isStructStrictlyNumericBlittable (t : CliType) : bool =
                match t with
                | CliType.ValueType vt ->
                    // Top-level: walk fields via the field-level classifier. Host-known
                    // field-only rejections (Decimal) do not apply here because the outer
                    // type's *own* declared type is what we're classifying. Top-level
                    // DateTime is filtered earlier by `IsAutoLayoutHandle`; if it ever
                    // reached us we'd want the same answer the field walker gives, so we
                    // intentionally don't short-circuit it.
                    match vt._Storage with
                    | CliValueTypeStorage.RawBytes _ -> false
                    | CliValueTypeStorage.Fields storage ->
                        storage.Fields |> List.forall (fun field -> isBlittableField field.Contents)
                | _ ->
                    // Top-level primitive (e.g. `Marshal.StructureToPtr<int>`): defer to the
                    // field walker. Primitives are unconditionally blittable; Bool/Char/etc.
                    // are not — same semantics either way.
                    isBlittableField t

            if isStructStrictlyNumericBlittable zero then
                // The eventual `*structMarshalStub` we write here is null: the blittable path
                // tells CoreLib to take the `SpanHelpers.Memmove` fast path
                // (marshalnative.cpp:99-145).
                let zeroNativeInt =
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

                // For the strictly-numeric subset, CoreCLR's marshal size and PawPrint's
                // managed CLI size coincide: each field's managed width equals its native
                // width, sequential layout uses natural alignment, and no `[MarshalAs]`
                // resizing is in play.
                let size = CliType.SizeOf zero

                let state =
                    IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state stubOutPtr zeroNativeInt

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        sizeOutPtr
                        (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 size.Size))))

                let state =
                    IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) ctx.Thread state

                NativeHandlerResult.completed state |> Some
            else
                failwith
                    $"TODO %s{operation}: only strictly-numeric blittable structs are supported by this QCall today; type %O{typeHandle} has fields outside that allowlist (see comment for the deferred cases)"
        | _ -> None

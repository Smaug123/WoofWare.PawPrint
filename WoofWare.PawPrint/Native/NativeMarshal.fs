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
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim state.Kernel.LastPInvokeError))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          "GetLastSystemError",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.Int32 (Int32Source.Verbatim state.Kernel.LastSystemError))
                ctx.Thread
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
                | EvalStackValue.Int32 (Int32Source.Verbatim 0) -> false
                | EvalStackValue.Int32 (Int32Source.Verbatim _) -> true
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
            // All three are implemented, but the middle one only for the shapes
            // `StructMarshalStub.tryComputePlan` admits.
            //
            // - No-layout: AutoLayout types, which covers `System.Object` and ordinary classes
            //   without `[StructLayout]`, as well as value types explicitly marked
            //   `[StructLayout(LayoutKind.Auto)]`.
            // - Blittable: the strict subset we are confident matches CoreCLR exactly — structs
            //   whose fields are recursively plain numeric (Int8..Float64), excluding the
            //   host-known field-only special cases (DateTime, Decimal) that CoreCLR's
            //   `MarshalInfo` diverts to stub synthesis (`MARSHAL_TYPE_DATE`, `NFT_DECIMAL`).
            // - Has-layout-non-blittable: a function pointer to a synthesised method carrying
            //   `RuntimeBehaviour.StructMarshalStub`, which `AbstractMachine` dispatches like any
            //   other runtime-provided method. Today that means a struct whose only non-blittable
            //   fields are `DateTime`.
            //
            // Everything else — `[MarshalAs]` descriptors, Bool/Char/ObjectRef fields, Decimal,
            // nested composites needing a recursive plan, and reference types (which reach us as
            // `CliType.ObjectRef` and so classify non-blittable, though CoreCLR would memmove a
            // sequential class) — surfaces a host TODO. Each future widening wants its own
            // motivating PawPrint test before being added to the classifier or the plan.

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

            // The classifier lives in `StructMarshalStub` so that this arm and the stub itself
            // ask the same question. It encodes the top-level-vs-field distinction CoreCLR's
            // `MarshalInfo` makes: CoreCLR walks fields with `IsFieldBlittable`, which
            // short-circuits DateTime to `MARSHAL_TYPE_DATE` (mlinfo.cpp:1747) and Decimal to
            // marshal-stub synthesis (`NFT_DECIMAL` in fieldmarshaler.cpp); neither of those
            // host-known types is byte-image compatible with its native form *when used as a
            // field*, but their standalone byte images can coincide with the native form
            // (Decimal's standalone is byte-identical; DateTime is filtered earlier by the
            // AutoLayout gate).
            let isStructStrictlyNumericBlittable (t : CliType) : bool =
                StructMarshalStub.isStructStrictlyNumericBlittable
                    state.ConcreteTypes
                    state._LoadedAssemblies
                    ctx.BaseClassTypes
                    t

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

            // Has-layout-non-blittable branch (marshalnative.cpp:118): CoreCLR synthesises an IL
            // stub, writes its entry address, writes *size = 0 (CoreLib ignores the size once the
            // stub is non-null) and returns TRUE. PawPrint writes a `StructMarshalStub` pointer
            // carrying the type's identity; `calli` on it runs `StructMarshalStub.executeStubCall`.
            //
            // The plan is computed *here*, and discarded, purely so an unsupported field shape is
            // reported at the QCall — where the type is named and the guest has not yet committed
            // to the stub path — rather than at the `calli`, which is several BCL frames away.
            match
                StructMarshalStub.tryComputePlan state.ConcreteTypes state._LoadedAssemblies ctx.BaseClassTypes zero
            with
            // The two error cases are kept apart because they call for different eventual
            // handling, and flattening them to a string would destroy the distinction the sibling
            // `MarshalNative_SizeOfHelper` arm above relies on. Both still fail the host today:
            // CoreCLR reports an unmarshalable *field* by throwing from stub synthesis
            // (`CreateStructMarshalILStub`), and which exception reaches the guest is not
            // something we should guess — `Marshal.StructureToPtr`'s own `ArgumentException` is
            // reachable only via the no-layout arm returning FALSE, which is a different
            // rejection. Establishing what CoreCLR actually throws here wants a differential
            // test, and that is its own change; until then, say which kind of refusal this is.
            | Result.Error (MarshalSizeError.NotMarshalable reason) ->
                failwith
                    $"TODO %s{operation}: type %O{typeHandle} has layout, but CoreCLR would reject it as unmarshalable too: %s{reason}. PawPrint does not yet model the guest-visible exception CoreCLR raises for this"
            | Result.Error (MarshalSizeError.NotImplemented reason) ->
                failwith
                    $"TODO %s{operation}: type %O{typeHandle} has layout but is not blittable, and PawPrint has not implemented its marshalling: %s{reason}"
            | Result.Ok _plan ->

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    stubOutPtr
                    (CliType.Numeric (
                        CliNumericType.NativeInt (
                            NativeIntSource.FunctionPointer (
                                FunctionPointerTarget.Managed (
                                    StructMarshalStub.synthesise operation ctx.BaseClassTypes state typeHandle
                                )
                            )
                        )
                    ))

            // Exactly as CoreCLR does: the size is left at zero on this arm, because CoreLib only
            // consults it on the blittable path.
            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    sizeOutPtr
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None

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
                (EvalStackValue.Int32 (
                    Int32Source.Verbatim (EmulatedKernel.lastPInvokeErrorFor ctx.Thread state.Kernel)
                ))
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

            state.MapKernel (EmulatedKernel.withLastPInvokeError ctx.Thread error)
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None

    /// CoreCLR's `IDS_CANNOT_MARSHAL` (mscorrc.rc:387), which `MarshalNative_SizeOfHelper` and
    /// `MarshalNative_OffsetOf` both raise as an `ArgumentException` naming the rejected type as
    /// `TypeString::AppendType` spells it with its default `FormatNamespace`.
    let private raiseCannotMarshal
        (operation : string)
        (ctx : NativeCallContext)
        (typeHandle : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : NativeHandlerResult
        =
        let name =
            NativeRuntimeTypeHelpers.runtimeTypeHandleName
                operation
                state
                NativeRuntimeTypeHelpers.formatNamespaceFlag
                typeHandle

        NativeHandlerResult.raiseExceptionWithMessage
            ctx.BaseClassTypes.ArgumentException
            (Some
                $"Type '%s{name}' cannot be marshaled as an unmanaged structure; no meaningful size or offset can be computed.")
            state

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
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
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
                raiseCannotMarshal operation ctx (RuntimeTypeHandleTarget.Closed typeHandle) state
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
        | "MarshalNative_OffsetOf",
          "System.Private.CoreLib",
          "System.Runtime.InteropServices",
          "Marshal",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            // `Marshal.OffsetOf(Type, string)` (Marshal.CoreCLR.cs:41) has already done every
            // check that depends on the *name*: a null type or name, and a field that is not an
            // instance field of the type, are all refused in managed code. What reaches us is the
            // `FieldDesc` reflection found, which may belong to a base class of the type the
            // guest named.
            let operation = "MarshalNative_OffsetOf"

            let fieldHandle =
                // The QCall's PRECONDITION is `pFD != NULL`, and the managed wrapper passes an
                // `RtFieldInfo`'s own handle, so a null one is a bug.
                match NativeCall.fieldHandleIdOfRuntimeFieldHandleInternal operation instruction.Arguments.[0] with
                | None -> failwith $"%s{operation}: null FieldDesc"
                | Some fieldHandleId ->
                    match FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles with
                    | Some fieldHandle -> fieldHandle
                    | None -> failwith $"%s{operation}: field-registry handle %d{fieldHandleId} is not allocated"

            // CoreCLR answers for `pFD->GetApproxEnclosingMethodTable()`: the type that declares
            // the field, not the one the guest named, so an inherited field is placed within its
            // base class's layout.
            match fieldHandle.GetDeclaringTypeHandle () with
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ as declaringType ->
                // The typical instantiation `G<T>` never has a layout: the type loader grants one
                // only to a type containing no generic variables (methodtablebuilder.cpp:12707),
                // so `IsStructMarshalable` fails whatever the fields are.
                raiseCannotMarshal operation ctx declaringType state |> Some
            | RuntimeTypeHandleTarget.Closed declaringType ->
                let sharedInstantiation =
                    match AllConcreteTypes.lookup declaringType state.ConcreteTypes with
                    | None ->
                        failwith $"%s{operation}: declaring type %O{declaringType} is not a registered concrete type"
                    | Some concrete ->
                        concrete.Generics
                        |> Seq.exists (
                            IlMachineRuntimeMetadata.isSharedTypeArgument
                                ctx.BaseClassTypes
                                state
                                $"%s{operation}: the declaring type %s{AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes declaringType}"
                        )

                if sharedInstantiation then
                    // The approximate enclosing MethodTable of an instantiation shared over
                    // `System.__Canon` is the canonical one, and a field of type `T` there is a
                    // `__Canon`, which has no native form. Measured on real .NET: `S<string>` is
                    // refused, naming `S<__Canon>`, when `S<T>` holds a `T` -- even one marked
                    // `[MarshalAs(ByValTStr)]`, which `S<string>`'s own layout would accept -- and
                    // answered when it does not.
                    failwith
                        $"TODO: %s{operation}: %s{AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes declaringType} is an instantiation shared over System.__Canon, whose native layout CoreCLR computes for the canonical form; PawPrint does not model canonical forms"

                let zero, state =
                    IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes declaringType

                match
                    CliType.TryComputeMarshalFieldOffset
                        state.ConcreteTypes
                        state._LoadedAssemblies
                        ctx.BaseClassTypes
                        declaringType
                        zero
                        (fieldHandle.GetFieldDefinitionHandle ())
                with
                | Result.Error (MarshalSizeError.NotMarshalable _) ->
                    raiseCannotMarshal operation ctx (RuntimeTypeHandleTarget.Closed declaringType) state
                    |> Some
                | Result.Error (MarshalSizeError.NotImplemented reason) ->
                    failwith
                        $"TODO %s{operation}: unimplemented marshalling case for %s{AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes declaringType}: %s{reason}"
                | Result.Ok offset ->
                    state
                    |> IlMachineState.pushToEvalStack
                        (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 offset))))
                        ctx.Thread
                    |> NativeHandlerResult.completed
                    |> Some
            | RuntimeTypeHandleTarget.OpenConstructed _
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _
            | RuntimeTypeHandleTarget.DynamicMethodsClass _
            | RuntimeTypeHandleTarget.Composite _
            | RuntimeTypeHandleTarget.FunctionPointer _ as other ->
                // Reflection mints a field handle against a closed type or an open generic
                // definition only; nothing else declares fields.
                failwith $"%s{operation}: a FieldDesc's declaring type cannot be %O{other}"
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
            //   whose fields are recursively plain numeric (Int8..Float64) or UTF-16 `char`s,
            //   excluding the host-known field-only special cases (DateTime, Decimal) that
            //   CoreCLR's `MarshalInfo` diverts to stub synthesis (`MARSHAL_TYPE_DATE`,
            //   `MARSHAL_TYPE_DECIMAL`).
            // - Has-layout-non-blittable: a function pointer to a synthesised method carrying
            //   `RuntimeBehaviour.StructMarshalStub`, which `AbstractMachine` dispatches like any
            //   other runtime-provided method. Today that means a struct whose only non-blittable
            //   fields are `DateTime`, `Decimal`, `bool` or ANSI `char`.
            //
            // Everything else — `[MarshalAs]` descriptors on other fields, ObjectRef fields,
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
            // short-circuits DateTime to `MARSHAL_TYPE_DATE` (mlinfo.cpp:1747) and rejects a
            // Decimal field outright (fieldmarshaler.cpp:266). Neither host-known type may be
            // memmoved *as a field*, though a standalone Decimal is its own native layout, and a
            // standalone DateTime is filtered earlier by the AutoLayout gate.
            let isBlittableStruct (t : CliType) : bool =
                StructMarshalStub.isBlittableStruct state.ConcreteTypes state._LoadedAssemblies ctx.BaseClassTypes t

            if isBlittableStruct zero then
                // The eventual `*structMarshalStub` we write here is null: the blittable path
                // tells CoreLib to take the `SpanHelpers.Memmove` fast path
                // (marshalnative.cpp:99-145).
                let zeroNativeInt =
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

                // For a blittable struct, CoreCLR's marshal size and PawPrint's managed CLI size
                // coincide: each field's managed width equals its native width (a blittable
                // `char` is a two-byte UTF-16 code unit either way), and sequential layout uses
                // natural alignment in both.
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

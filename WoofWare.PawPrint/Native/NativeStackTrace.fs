namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeStackTrace =

    /// In/out on entry to the QCall: the caller's `NumFramesRequested`, and on return the number of
    /// frames captured. Named rather than inlined because it is both read and written below.
    ///
    /// `StackFrameHelper` is sealed and derives directly from `Object`, so this and every other
    /// field named in this file is its own and resolves against the object's own concrete type.
    [<Literal>]
    let private frameCountField = "iFrameCount"

    let private concretizeCorelibType
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle
        =
        DumpedAssembly.typeInfoToTypeDefn' ctx.BaseClassTypes state._LoadedAssemblies typeInfo
        |> IlMachineState.concretizeType
            ctx.LoggerFactory
            ctx.BaseClassTypes
            state
            ctx.BaseClassTypes.Corelib.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty

    /// Allocate a one-dimensional array of `elementHandle` holding `values`, and return its
    /// address as an `ObjectRef` ready to be stored in a `StackFrameHelper` field.
    let private allocateFilledArray
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (elementHandle : ConcreteTypeHandle)
        (values : CliType list)
        : IlMachineState * CliType
        =
        let elementZero, state =
            IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes elementHandle

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero elementHandle)
                (fun () -> elementZero)
                values.Length
                state

        let state =
            ((state, 0), values)
            ||> List.fold (fun (state, index) value ->
                IlMachineState.setArrayValue arrayAddr value index state, index + 1
            )
            |> fst

        state, CliType.ObjectRef (Some arrayAddr)

    /// The frames a capture should report.
    ///
    /// `NumFramesRequested` bounds only the current-thread walk: CoreCLR consults it in
    /// `GetStackFrames`' stack-walk callback (debugdebugger.cpp:242) and never in
    /// `GetStackFramesFromException`, whose comment records that "for StackTraces from an
    /// Exception, the EE always captures all frames" (StackFrameHelper.cs:78-80).
    let private framesToReport
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (exceptionAddr : ManagedHeapAddress option)
        (numFramesRequested : int)
        : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list
        =
        match exceptionAddr with
        | Some addr -> IlMachineState.frozenStackTraceFrames ctx.BaseClassTypes addr state
        | None ->
            let frames = StackFrameCapture.ofThread state.ThreadState.[ctx.Thread]

            if numFramesRequested > 0 then
                frames |> List.truncate numFramesRequested
            else
                frames

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
        | "StackTrace_GetStackFramesInternal",
          "System.Private.CoreLib",
          "System.Diagnostics",
          "StackTrace",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", helperGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", exceptionGenerics) ],
          MethodReturnType.Void when helperGenerics.IsEmpty && exceptionGenerics.IsEmpty ->
            // CoreCLR debugdebugger.cpp:287. Fills a caller-allocated `StackFrameHelper` in place:
            // `iFrameCount` arrives as the caller's `NumFramesRequested` and leaves as the number
            // of frames captured, and the array fields are assigned only when that number is
            // non-zero.
            //
            // `fNeedFileInfo` is deliberately unread. It selects whether CoreCLR consults a PDB
            // reader to fill `rgFilename`/`rgiLineNumber`/`rgiColumnNumber`; PawPrint has no
            // loaded PE image to read symbols from, so it reports every frame's method token as 0,
            // which is CoreLib's own signal to skip source lookup for that frame
            // (StackFrameHelper.cs:119). Those three arrays are therefore left null, which their
            // getters tolerate — `GetFilename` reads `rgFilename?[i]`, and
            // `GetLineNumber`/`GetColumnNumber` answer 0 for a null array.
            let operation = "StackTrace_GetStackFramesInternal"

            if instruction.Arguments.Length <> 3 then
                failwith
                    $"%s{operation}: expected three native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let helperPtr =
                NativeCall.objectHandleOnStackTarget operation state "stackFrameHelper" instruction.Arguments.[0]

            let helperAddr =
                match IlMachineState.readManagedByref ctx.BaseClassTypes state helperPtr with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    // CoreCLR would dereference a null `STACKFRAMEHELPERREF`. Every managed caller
                    // constructs the helper immediately before calling, so a null here is an
                    // interpreter bug rather than a guest error.
                    failwith
                        $"%s{operation}: ObjectHandleOnStack pointed to a null StackFrameHelper; every caller allocates one before calling, so this is an interpreter bug"
                | other -> failwith $"%s{operation}: expected ObjectRef in ObjectHandleOnStack, got %O{other}"

            let exceptionPtr =
                NativeCall.objectHandleOnStackTarget operation state "exception" instruction.Arguments.[2]

            // Null is the current-thread request, not an error: `new StackTrace()` and
            // `Environment.StackTrace` both pass it (CaptureStackTrace's `e` parameter).
            let exceptionAddr =
                match IlMachineState.readManagedByref ctx.BaseClassTypes state exceptionPtr with
                | CliType.ObjectRef addr -> addr
                | other ->
                    failwith $"%s{operation}: expected ObjectRef in the exception ObjectHandleOnStack, got %O{other}"

            let numFramesRequested =
                let field =
                    IlMachineState.requiredOwnInstanceFieldId
                        state
                        (ManagedHeap.get helperAddr state.ManagedHeap).ConcreteType
                        frameCountField

                match
                    AllocatedNonArrayObject.DereferenceFieldById field (ManagedHeap.get helperAddr state.ManagedHeap)
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected Int32 in StackFrameHelper.iFrameCount, got %O{other}"

            let frames = framesToReport ctx state exceptionAddr numFramesRequested

            let state =
                IlMachineState.setOwnInstanceField
                    helperAddr
                    frameCountField
                    (CliType.Numeric (CliNumericType.Int32 frames.Length))
                    state

            match frames with
            | [] ->
                // CoreCLR allocates no array at all in this case (debugdebugger.cpp:331-334), so
                // the helper's fields stay as its constructor left them: null. An exception that
                // has never been thrown takes this path, and `StackTrace` handles it — `GetFrames`
                // answers `Array.Empty` and `GetFrame` answers null.
                NativeHandlerResult.completed state |> Some
            | _ :: _ ->

            let state, int32Handle = concretizeCorelibType ctx state ctx.BaseClassTypes.Int32
            let state, intPtrHandle = concretizeCorelibType ctx state ctx.BaseClassTypes.IntPtr

            // The method handle of each frame, as CoreCLR's `rgMethodHandle` carries it: an
            // `IntPtr` per frame, read back by managed code as
            // `new RuntimeMethodHandleInternal(mh)`. `MethodHandlePtr` is the `IntPtr`-shaped
            // spelling of a registry id, which is what a value that has travelled through an
            // `IntPtr` array cell decodes to.
            let state, methodHandles =
                ((state, []), frames)
                ||> List.fold (fun (state, acc) frame ->
                    let id, registry =
                        MethodHandleRegistry.getOrAllocateDefinitionId
                            state.ConcreteTypes
                            frame.Method
                            state.MethodHandles

                    let state =
                        { state with
                            MethodHandles = registry
                        }

                    state,
                    CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodHandlePtr id))
                    :: acc
                )

            let methodHandles = List.rev methodHandles

            let state, methodHandleArray =
                allocateFilledArray ctx state intPtrHandle methodHandles

            // PawPrint runs no native code, so no frame has a native offset. `-1` is CoreLib's own
            // word for that (`StackFrame.OFFSET_UNKNOWN`, StackFrame.cs:133), which
            // `StackFrame.ToString` renders as "<offset unknown>". `StackTrace.ToString` reads only
            // the IL offset, so the ordinary rendering is unaffected.
            let state, nativeOffsetArray =
                frames
                |> List.map (fun _ -> CliType.Numeric (CliNumericType.Int32 -1))
                |> allocateFilledArray ctx state int32Handle

            let state, ilOffsetArray =
                frames
                |> List.map (fun frame -> CliType.Numeric (CliNumericType.Int32 frame.IlOffset))
                |> allocateFilledArray ctx state int32Handle

            // Zero for every frame: CoreLib reads a non-zero token as "ask the portable-PDB reader
            // about this frame", and PawPrint has no symbols to answer with.
            let state, methodTokenArray =
                frames
                |> List.map (fun _ -> CliType.Numeric (CliNumericType.Int32 0))
                |> allocateFilledArray ctx state int32Handle

            let state =
                state
                |> IlMachineState.setOwnInstanceField helperAddr "rgMethodHandle" methodHandleArray
                |> IlMachineState.setOwnInstanceField helperAddr "rgiOffset" nativeOffsetArray
                |> IlMachineState.setOwnInstanceField helperAddr "rgiILOffset" ilOffsetArray
                |> IlMachineState.setOwnInstanceField helperAddr "rgiMethodToken" methodTokenArray

            // Allocated only if some frame carries the flag, matching CoreCLR's optimisation
            // (debugdebugger.cpp:400-415): `IsLastFrameFromForeignExceptionStackTrace` reads a
            // null array as false for every frame, so an all-false array and no array are
            // indistinguishable to the guest, and CoreCLR spends nothing on the common case.
            if
                frames
                |> List.exists (fun frame -> frame.IsLastFrameFromForeignExceptionStackTrace)
            then
                let state, booleanHandle =
                    concretizeCorelibType ctx state ctx.BaseClassTypes.Boolean

                let state, foreignArray =
                    frames
                    |> List.map (fun frame -> CliType.ofBool frame.IsLastFrameFromForeignExceptionStackTrace)
                    |> allocateFilledArray ctx state booleanHandle

                IlMachineState.setOwnInstanceField
                    helperAddr
                    "rgiLastFrameFromForeignExceptionStackTrace"
                    foreignArray
                    state
                |> NativeHandlerResult.completed
                |> Some
            else
                NativeHandlerResult.completed state |> Some
        | _ -> None

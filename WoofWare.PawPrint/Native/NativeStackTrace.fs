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

    /// The registry id naming `frame`'s method, as `rgMethodHandle` carries it.
    ///
    /// A dynamic method is *not* metadata-backed but is still perfectly nameable: its
    /// `DynamicMethodHandle` already carries a registry id, minted when `Reflection.Emit` built it,
    /// and `MethodHandleRegistry` maps that id to a `MethodHandle.FromDynamic`. So the id is read
    /// off the frame rather than minted, exactly as CoreCLR writes the frame's existing
    /// `DynamicMethodDesc*` for an LCG method. Such frames really do appear in traces — an
    /// exception thrown out of a `DynamicMethod` carries one.
    ///
    /// The other synthesised kinds have no handle of any sort and are refused. That refusal is
    /// deliberate rather than an omission: writing `IntPtr.Zero` would make `GetMethodBase` answer
    /// null, and `CalculateFramesToSkip` counts a null-method frame as skippable (its namespace
    /// tests sit inside `if (mb != null)` while the `iRetVal++` after them is unconditional,
    /// StackTrace.CoreCLR.cs:26-41), so the frame would vanish from the trace silently rather than
    /// visibly.
    let private methodHandleIdOfFrame
        (operation : string)
        (state : IlMachineState)
        (frame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState * int64
        =
        match frame.Method.SynthesisedKind with
        | None ->
            let id, registry =
                MethodHandleRegistry.getOrAllocateDefinitionId state.ConcreteTypes frame.Method state.MethodHandles

            { state with
                MethodHandles = registry
            },
            id
        | Some (SynthesisedMethod.DynamicMethod handle) -> state, handle.GetRegistryId ()
        | Some SynthesisedMethod.EntryPointPlaceholder ->
            // The frame the entry thread carries while startup runs class initialisers, before
            // `Main` is installed. It is shaped like the entry point but deliberately carries no
            // MethodDef handle, so that nothing resolves its IL offsets against the real `Main`'s
            // debug information and reports source lines for code that has not executed
            // (`SynthesisedMethod.EntryPointPlaceholder`).
            //
            // Real .NET has no equivalent moment — its class initialisers are lazy, so the closest
            // scenario, an entry type whose initialiser runs from `Main`'s first static access,
            // does have a real `Main` frame and reports it (measured). Answering that here means
            // naming the placeholder as `Main`, which reintroduces exactly the hazard that
            // docstring exists to prevent: the placeholder's own body is a bare `ret`, so its
            // offset 0 is a real position in `Main` that a consumer could map to a source line.
            // Deciding between those is not this handler's to make, so it says so instead.
            failwith
                $"%s{operation}: a captured frame is the entry-point placeholder for %s{frame.Method.Name}, which carries no method handle by construction. This is reachable from a class initialiser that captures a stack trace during startup, before Main is installed. Deciding what such a frame should report — the entry point's own handle, or no frame at all — needs the question in SynthesisedMethod.EntryPointPlaceholder's docstring settled first."
        | Some kind ->
            failwith
                $"%s{operation}: a captured frame runs the synthesised method %s{frame.Method.Name} (%O{kind}), which has no MethodDef row and no registry id to name it. Reporting a null handle instead is not an option: CalculateFramesToSkip counts a null-method frame as skippable, so the frame would silently vanish from the guest's trace."

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
                    let state, id = methodHandleIdOfFrame operation state frame

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

            // A frame whose method has no IL body has no IL offset either, and reports
            // `OFFSET_UNKNOWN`. CoreCLR distinguishes the two ways an offset can be missing
            // (`InitPass2`, debugdebugger.cpp:1543-1607): a *valid* jitted method whose debug info
            // yields no mapping gets 0, but a frame with no managed code information at all falls
            // to `bRes = false` and then to `(DWORD)-1`. A runtime-provided method is the second
            // case — `MethodState.IlOpIndex` for one is the synthetic 0 it was created with, not a
            // position in a body it does not have — so reporting 0 would present that placeholder
            // as a real offset into the first instruction.
            //
            // This is not hypothetical: the innermost frame of every current-thread capture is the
            // P/Invoke stub for this very QCall, and PawPrint keeps frames for InternalCall and
            // QCall methods too (`ExceptionDispatching` deliberately does not suppress them,
            // because a real trace does name them).
            //
            // Normalised here rather than in `StackFrameCapture`, because it is a property of what
            // `rgiILOffset` means: `ExceptionStackFrame.IlOffset` is documented as a byte position
            // within a method's IL, and -1 is not one. The frozen traces the exception branch
            // returns are shared with `renderExceptionStackTrace`, which must keep reading that
            // field as the position it claims to be.
            let state, ilOffsetArray =
                frames
                |> List.map (fun frame ->
                    match frame.Method.Body with
                    | MethodBody.Il _ -> CliType.Numeric (CliNumericType.Int32 frame.IlOffset)
                    | _ -> CliType.Numeric (CliNumericType.Int32 -1)
                )
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
                // Explicitly null rather than merely left alone, which is what CoreCLR writes here
                // (debugdebugger.cpp:416-418). It matters if a helper is ever reused: a previous
                // capture's flags would otherwise survive into this one, as an array that is both
                // stale and possibly the wrong length. No CoreLib caller reuses one — every one
                // allocates a helper immediately before calling — so this is about the handler's
                // answer being a function of its arguments rather than of what came before.
                IlMachineState.setOwnInstanceField
                    helperAddr
                    "rgiLastFrameFromForeignExceptionStackTrace"
                    (CliType.ObjectRef None)
                    state
                |> NativeHandlerResult.completed
                |> Some
        | _ -> None

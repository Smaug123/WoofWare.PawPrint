namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// The <c>DelegateBindingFlags</c> bitfield (<c>comdelegate.h</c>:109-117, mirrored managed-side
/// at <c>Delegate.CoreCLR.cs</c>:561-570) that the delegate-binding QCalls take as their last
/// argument. It is how a caller narrows what shape of binding it is willing to accept, which is
/// what makes the same routine serve `Delegate.CreateDelegate`'s six overloads.
/// </summary>
[<RequireQualifiedAccess>]
module DelegateBindingFlags =
    let staticMethodOnly = 0x00000001
    let instanceMethodOnly = 0x00000002
    let openDelegateOnly = 0x00000004
    let closedDelegateOnly = 0x00000008
    let neverCloseOverNull = 0x00000010

    /// Only consulted by `BindToMethodName`, which is the overload that has a name to match
    /// case-insensitively. Nothing reaching `BindToMethodInfo` can set it, because that entry
    /// point is handed a specific method rather than a name.
    let caselessMatching = 0x00000020

    /// Co/contravariant matching. The only flag any caller that can reach
    /// `Delegate_BindToMethodInfo` with a `DynamicMethod` sets, and the only one the handler
    /// accepts — see the enumeration of callers there, and `NativeDelegate.isCompatible`'s remarks
    /// for why the other filters are absent rather than transcribed.
    ///
    /// A *strict* comparison is nonetheless reachable even so, because
    /// `isLocationAssignable` suppresses relaxation itself for a byref: strictness is not only
    /// something a caller can ask for.
    let relaxedSignature = 0x00000040

    let private all =
        staticMethodOnly
        ||| instanceMethodOnly
        ||| openDelegateOnly
        ||| closedDelegateOnly
        ||| neverCloseOverNull
        ||| caselessMatching
        ||| relaxedSignature

    /// Refuse a bit CoreCLR does not define, rather than silently ignoring it. An undefined bit
    /// means either the enum grew upstream or the argument was unmarshalled wrongly, and both are
    /// things to find out about at the point they happen.
    let requireKnown (operation : string) (flags : int) : unit =
        let unknown = flags &&& ~~~all

        if unknown <> 0 then
            failwith
                $"%s{operation}: DelegateBindingFlags 0x%08x{flags} carries undefined bits 0x%08x{unknown}; comdelegate.h defines only 0x01..0x40"

/// <summary>
/// Whether a candidate target method can back a given delegate type, and if so whether the
/// resulting delegate is open or closed. This is <c>COMDelegate::IsMethodDescCompatible</c>
/// (comdelegate.cpp:2544).
/// </summary>
[<RequireQualifiedAccess>]
type DelegateBindingShape =
    /// The delegate's `Invoke` supplies every argument the target takes, so nothing is bound at
    /// creation time.
    | Open
    /// `Invoke` supplies one fewer argument than the target takes, so the target's first argument
    /// is bound at creation time — to the `target` object for a closed delegate over a static
    /// method, or to the receiver for a closed delegate over an instance method.
    | Closed

/// <summary>
/// The QCalls behind <c>Delegate.CreateDelegate</c> and <c>Delegate.Method</c>. Two are
/// implemented: <c>Delegate_BindToMethodInfo</c>, and only for a target minted by
/// <c>Reflection.Emit</c>; and <c>Delegate_FindMethodHandle</c>, for a delegate over an ordinary
/// metadata method.
/// </summary>
[<RequireQualifiedAccess>]
module NativeDelegate =

    /// <summary>
    /// <c>IsLocationAssignable</c> (comdelegate.cpp:2367): may a value of type
    /// <paramref name="fromHandle" /> be passed where a <paramref name="toHandle" /> is expected,
    /// for the purposes of delegate signature matching?
    /// </summary>
    /// <param name="fromHandleIsBoxed">
    /// True when the value is known to have arrived boxed — which is the case for the bound first
    /// argument of a closed delegate, since it was handed to `CreateDelegate` as an `object`.
    /// CoreCLR skips the objref-ness check then, because the boxing has already happened.
    /// </param>
    /// <remarks>
    /// <para>
    /// Two branches of CoreCLR's version are absent because they cannot be reached with the
    /// arguments this QCall gets. Both sides here are <c>ConcreteTypeHandle</c>s, which are closed
    /// by construction, so the whole generic-variable half of the function (comdelegate.cpp:
    /// 2399-2489, the <c>ConstrainedAsObjRef</c>/<c>ConstrainedAsValueType</c> table) is dead: a
    /// delegate type reaching <c>CreateDelegate</c> is a runtime type with its instantiation
    /// already substituted, and a dynamic method's signature cannot spell a variable at all.
    /// </para>
    /// <para>
    /// The enum arm at the end is <em>not</em> dead, and it is the reason this function exists
    /// rather than a bare assignability call: <c>Func&lt;DayOfWeek, int&gt;</c> over a dynamic
    /// method taking an <c>int</c> is a legal binding in CoreCLR, decided entirely by that arm
    /// (the two types have the same verifier element type, and one of them is an enum). An
    /// implementation that stopped at "identical, or castable" would reject it.
    /// </para>
    /// </remarks>
    let private isLocationAssignable
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (fromHandle : ConcreteTypeHandle)
        (toHandle : ConcreteTypeHandle)
        (relaxedMatch : bool)
        (fromHandleIsBoxed : bool)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        // The verifier element type: for an enum, its underlying integer's, which is exactly what
        // makes the enum arm below able to see through the enum. CoreCLR spells this
        // `TypeHandle::GetVerifierCorElementType`.
        let verifierCorElementType (state : IlMachineState) (handle : ConcreteTypeHandle) : IlMachineState * int32 =
            let state, isEnum =
                IlMachineState.isEnumValueType loggerFactory baseClassTypes state handle

            if not isEnum then
                state,
                NativeRuntimeTypeHelpers.corElementType
                    operation
                    baseClassTypes
                    state
                    (RuntimeTypeHandleTarget.Closed handle)
            else

            match IlMachineState.enumUnderlyingHandle loggerFactory baseClassTypes state handle with
            | Some (state, underlying) ->
                state,
                NativeRuntimeTypeHelpers.corElementType
                    operation
                    baseClassTypes
                    state
                    (RuntimeTypeHandleTarget.Closed underlying)
            | None ->
                failwith
                    $"%s{operation}: %O{handle} derives from System.Enum but has no single `value__` instance field to take an underlying type from"

        // Reached when the types are not identical and relaxed matching either was not asked for
        // or did not admit the cast. CoreCLR's comment: "they are not compatible yet enums can go
        // into each other if their underlying element type is the same".
        let enumArm (state : IlMachineState) : IlMachineState * bool =
            let state, fromElement = verifierCorElementType state fromHandle
            let state, toElement = verifierCorElementType state toHandle

            if fromElement <> toElement then
                state, false
            else

            let state, fromIsEnum =
                IlMachineState.isEnumValueType loggerFactory baseClassTypes state fromHandle

            let state, toIsEnum =
                IlMachineState.isEnumValueType loggerFactory baseClassTypes state toHandle

            state, (fromIsEnum || toIsEnum)

        if fromHandle = toHandle then
            state, true
        else

        // CoreCLR spells this as "byref parameters can never be allowed relaxed matching since type
        // safety will always be violated in one of the two directions (in or out)", and implements
        // it by clearing `relaxedMatch` when the source is a byref. Stated here as a direct
        // refusal instead, which is the same answer: with relaxation off the only remaining path
        // is the enum arm, every byref has verifier element type BYREF and none is an enum, so two
        // distinct byrefs fail on enum-ness and a byref against a non-byref fails on element type.
        // (`isConcreteTypeAssignableTo` already refuses a byref structurally, so today clearing
        // the flag would change nothing; the guard is against assignability ever learning byref
        // variance, at which point the objref-ness check below would silently admit `string&`
        // where an `object&` was wanted.)
        //
        // No test kills this, and none can today: reaching it from the target side needs a dynamic
        // method with a byref parameter, which needs `typeof(int).MakeByRefType()`, and
        // `RuntimeTypeHandle_MakeByRef` is unimplemented (measured). The delegate-side direction is
        // reachable and `DynamicMethodDelegateBinding.cs` exercises it, but the enum arm gives the
        // same answer there.
        let eitherIsByref =
            let isByref (handle : ConcreteTypeHandle) : bool =
                match handle with
                | ConcreteTypeHandle.Byref _ -> true
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ -> false

            isByref fromHandle || isByref toHandle

        if eitherIsByref then
            state, false
        elif not relaxedMatch then
            enumArm state
        else

        let state, canCast =
            IlMachineState.isConcreteTypeAssignableTo loggerFactory baseClassTypes state fromHandle toHandle

        if not canCast then
            enumArm state
        elif fromHandleIsBoxed then
            state, true
        else

        // "Check that the 'objrefness' of source and destination matches": without boxing, a value
        // type does not implicitly become an `object` at the call, whatever `CanCastTo` says.
        let fromIsObjRef =
            IlMachineState.isReferenceTypeHandle baseClassTypes operation state fromHandle

        let toIsObjRef =
            IlMachineState.isReferenceTypeHandle baseClassTypes operation state toHandle

        state, (fromIsObjRef = toIsObjRef)

    /// Concretise a `TypeDefn` decoded out of a dynamic method's signature blob. The blob's token
    /// universe is the scope assembly's (see `MethodSignatureDecoding`), and neither a type nor a
    /// method instantiation can be in scope: a dynamic method is never generic, and it is declared
    /// on the synthetic per-module class rather than on any generic type.
    let private concretiseSignatureType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (scopeAssemblyName : System.Reflection.AssemblyName)
        (state : IlMachineState)
        (typeDefn : TypeDefn)
        : IlMachineState * ConcreteTypeHandle
        =
        IlMachineState.concretizeType
            loggerFactory
            baseClassTypes
            state
            scopeAssemblyName
            ImmutableArray.Empty
            ImmutableArray.Empty
            typeDefn

    /// The `Invoke` method of a delegate type, fully concretised. CoreCLR reaches it through
    /// `COMDelegate::FindDelegateInvokeMethod` (comdelegate.cpp:2516), which reads the slot the
    /// `DelegateEEClass` caches; PawPrint has no such cache and looks the method up by name, which
    /// is the same thing given that the runtime is what synthesises `Invoke` in the first place
    /// and gives every delegate type exactly one.
    let private delegateInvokeMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (delegateType : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let concreteType, typeInfo =
            IlMachineState.tryGetConcreteTypeInfo state delegateType
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: the delegate's type %O{delegateType} has no TypeDef row"
            )

        let invoke =
            typeInfo.Methods
            |> List.filter (fun method -> method.Name = "Invoke" && not method.IsStatic)
            |> function
                | [ single ] -> single
                | [] ->
                    // CoreCLR raises `MissingMethodException("Invoke")` here
                    // (comdelegate.cpp:2530). Unreachable from this QCall: `CreateDelegate`'s
                    // callers all check `rtType.IsDelegate()` first, and a type whose base is
                    // `MulticastDelegate` got its `Invoke` from the runtime.
                    failwith
                        $"%s{operation}: delegate type %s{typeInfo.Namespace}.%s{typeInfo.Name} declares no instance method named Invoke"
                | several ->
                    failwith
                        $"%s{operation}: delegate type %s{typeInfo.Namespace}.%s{typeInfo.Name} declares %d{several.Length} instance methods named Invoke; a delegate type has exactly one"

        let state, concretised, _declaringHandle =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                concreteType.Generics
                invoke
                ImmutableArray.Empty
                state

        state, concretised

    /// <summary>
    /// <c>COMDelegate::IsMethodDescCompatible</c> (comdelegate.cpp:2544), specialised to a target
    /// method minted by <c>Reflection.Emit</c>.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Being specialised to a dynamic method removes three of CoreCLR's cases outright. A
    /// <c>DynamicMethod</c> is always static (<c>DynamicMethod</c>'s constructors set
    /// <c>mdStatic</c> unconditionally), so every "is the target an instance method" branch takes
    /// its static side; it is never virtual, so the open path's virtual sub-branch is dead; and it
    /// is never a generic method definition, which <c>Delegate_BindToMethodInfo</c> rejects with
    /// <c>ArgumentException(Arg_DlgtTargMeth)</c> before ever reaching here
    /// (comdelegate.cpp:1137-1139).
    /// </para>
    /// <para>
    /// It also removes four of the five flag filters, which is why this takes a bare
    /// <c>relaxedMatch</c> rather than the bitfield. The caller has already established that the
    /// flags are exactly <c>DBF_RelaxedSignature</c> — see the handler for the enumeration of
    /// callers that makes that exhaustive — so <c>DBF_StaticMethodOnly</c>,
    /// <c>DBF_InstanceMethodOnly</c>, <c>DBF_OpenDelegateOnly</c>, <c>DBF_ClosedDelegateOnly</c>
    /// and <c>DBF_NeverCloseOverNull</c> are all unset on every reachable call. Whoever makes a
    /// second caller reachable should add their arms back with the tests that exercise them.
    /// </para>
    /// </remarks>
    let private isCompatible
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (relaxed : bool)
        /// The runtime type of the object supplied as the bound first argument, if one was.
        (firstArgType : ConcreteTypeHandle option)
        (invokeSignature : TypeMethodSignature<ConcreteTypeHandle>)
        (targetSignature : TypeMethodSignature<ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * DelegateBindingShape option
        =
        // "Check that there is no vararg mismatch." A vararg dynamic method is not constructible
        // (`DynamicMethod`'s constructors pass `CallingConventions.Standard`), and neither is a
        // vararg delegate type from any language PawPrint's tests compile, so no test can
        // exercise this check.
        if
            invokeSignature.Header.Get.CallingConvention
            <> targetSignature.Header.Get.CallingConvention
        then
            state, None
        else

        // A `DynamicMethod` is static, so its total argument count is its fixed count with no
        // implicit `this`. Invoke supplies `numFixedInvokeArgs`; the difference decides the shape.
        let numFixedInvokeArgs = invokeSignature.ParameterTypes.Length
        let numTotalTargetArgs = targetSignature.ParameterTypes.Length

        let shape =
            if numTotalTargetArgs = numFixedInvokeArgs then
                Some DelegateBindingShape.Open
            elif numTotalTargetArgs = numFixedInvokeArgs + 1 then
                Some DelegateBindingShape.Closed
            else
                None

        match shape with
        | None -> state, None
        | Some shape ->

        let isOpen = (shape = DelegateBindingShape.Open)

        // "If, on the other hand, we're looking at an open delegate but the caller has provided a
        // target it's also not a match."
        //
        // The shape comes from the *arity*, and the supplied target is then checked against it —
        // not the other way round. A closed delegate over a null first argument is a real and
        // reachable shape (`CreateDelegate(t, null)` on a dynamic method taking one more argument
        // than the delegate), so "target is null" must not be read as "must be open".
        if isOpen && firstArgType.IsSome then
            state, None
        else

        // "There is one edge case for an open static delegate which takes no arguments. In that
        // case we're nearly done, just compare the return types."
        let state, argumentsMatch =
            if numTotalTargetArgs = 0 then
                state, true
            else

            // Where the first argument's type comes from differs on each side. On the invoke side
            // it is the first `Invoke` parameter when open, and the *runtime type of the bound
            // object* when closed — CoreCLR reads `refFirstArg->GetTypeHandle()`
            // (comdelegate.cpp:1156), not any declared type. On the target side a dynamic method
            // is static, so it is always the first declared parameter.
            let firstTargetArg = targetSignature.ParameterTypes.Head

            let state, firstArgOk =
                match shape with
                | DelegateBindingShape.Open ->
                    let firstInvokeArg = invokeSignature.ParameterTypes.Head

                    isLocationAssignable
                        loggerFactory
                        baseClassTypes
                        operation
                        firstInvokeArg
                        firstTargetArg
                        relaxed
                        false
                        state
                | DelegateBindingShape.Closed ->
                    // "Delegates closed over static methods have a further constraint: the first
                    // argument of the target must be an object reference type (otherwise the
                    // argument shuffling logic could get complicated)."
                    if not (IlMachineState.isReferenceTypeHandle baseClassTypes operation state firstTargetArg) then
                        state, false
                    else

                    match firstArgType with
                    // Closed over null: "we don't have enough type information for the match but
                    // it doesn't matter because the null matches all object reference types, which
                    // our first arg must be in this case" — and the line above has just
                    // established that it is.
                    | None -> state, true
                    | Some firstArgType ->
                        // `fromHandleIsBoxed` is true: the argument reached `CreateDelegate` as an
                        // `object`, so whatever boxing was needed has already happened.
                        isLocationAssignable
                            loggerFactory
                            baseClassTypes
                            operation
                            firstArgType
                            firstTargetArg
                            relaxed
                            true
                            state

            if not firstArgOk then
                state, false
            else

            // "Loop over the remaining fixed args, the list should be one to one at this point."
            let remainingInvokeArgs =
                match shape with
                | DelegateBindingShape.Open -> invokeSignature.ParameterTypes.Tail
                | DelegateBindingShape.Closed -> invokeSignature.ParameterTypes

            let remainingTargetArgs = targetSignature.ParameterTypes.Tail

            if remainingInvokeArgs.Length <> remainingTargetArgs.Length then
                failwith
                    $"%s{operation}: internal error: after the first argument, the delegate has %d{remainingInvokeArgs.Length} parameter(s) and the target %d{remainingTargetArgs.Length}; the arity classification above guarantees these are equal"
            else

            ((state, true), List.zip remainingInvokeArgs remainingTargetArgs)
            ||> List.fold (fun (state, soFar) (invokeArg, targetArg) ->
                if not soFar then
                    state, false
                else
                    isLocationAssignable loggerFactory baseClassTypes operation invokeArg targetArg relaxed false state
            )

        if not argumentsMatch then
            state, None
        else

        // "remember that the assignment is in the other direction here, from callee to caller, so
        // switch the order of the arguments to IsLocationAssignable". So a target returning
        // `string` can back a delegate returning `object`, and not the reverse.
        let state, returnMatches =
            match targetSignature.ReturnType, invokeSignature.ReturnType with
            | MethodReturnType.Void, MethodReturnType.Void -> state, true
            | MethodReturnType.Void, MethodReturnType.Returns _
            | MethodReturnType.Returns _, MethodReturnType.Void -> state, false
            | MethodReturnType.Returns targetReturn, MethodReturnType.Returns invokeReturn ->
                isLocationAssignable
                    loggerFactory
                    baseClassTypes
                    operation
                    targetReturn
                    invokeReturn
                    relaxed
                    false
                    state

        if returnMatches then state, Some shape else state, None

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
        | "Delegate_BindToMethodInfo",
          "System.Private.CoreLib",
          "System",
          "Delegate",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              delegateHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              targetHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System",
                                              "RuntimeMethodHandleInternal",
                                              methodHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              typeHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "DelegateBindingFlags", flagsGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            delegateHandleGenerics.IsEmpty
            && targetHandleGenerics.IsEmpty
            && methodHandleGenerics.IsEmpty
            && typeHandleGenerics.IsEmpty
            && flagsGenerics.IsEmpty
            ->
            // `Delegate_BindToMethodInfo` (comdelegate.cpp:1110): decide whether `method` can back
            // the delegate `d`, and if so point `d` at it. The BOOL it returns is a *semantic*
            // answer — the managed caller turns FALSE into `ArgumentException(Arg_DlgtTargMeth)`
            // or a null return, depending on the overload (Delegate.CoreCLR.cs:391, 399-403). So
            // FALSE must be reserved for genuine incompatibility: anything PawPrint has not
            // implemented crashes the host instead, because reporting it as FALSE would hand the
            // guest a specific, wrong, catchable answer.
            let operation = "Delegate_BindToMethodInfo"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let readObjectHandle (argIndex : int) (argName : string) : ManagedHeapAddress option =
                NativeCall.objectHandleOnStackTarget operation state argName instruction.Arguments.[argIndex]
                |> IlMachineState.readManagedByref ctx.BaseClassTypes state
                |> CliType.unwrapPrimitiveLikeDeep
                |> function
                    | CliType.ObjectRef target -> target
                    | other -> failwith $"%s{operation}: expected %s{argName} to be an object reference, got %O{other}"

            let delegateAddr =
                readObjectHandle 0 "d"
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: the delegate is null, but every caller allocates it with Delegate.InternalAlloc immediately before reaching this QCall"
                )

            let targetAddr = readObjectHandle 1 "target"

            // CoreCLR asserts this (comdelegate.cpp:1134, "Assert to track down VS#458689"): a
            // delegate closed over itself would be a cycle the invocation path cannot unpick.
            if targetAddr = Some delegateAddr then
                failwith
                    $"%s{operation}: the delegate is its own bound target, which CoreCLR asserts against (comdelegate.cpp:1134)"

            let flags =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[4] with
                | CliType.Numeric (CliNumericType.Int32 flags) -> flags
                | other -> failwith $"%s{operation}: expected DelegateBindingFlags as an Int32, got %O{other}"

            DelegateBindingFlags.requireKnown operation flags

            // Every reachable caller passes exactly `RelaxedSignature`.
            // `Delegate.CreateDelegateNoSecurityCheck` — the only
            // route `DynamicMethod.CreateDelegate` takes — passes exactly that
            // (Delegate.CoreCLR.cs:387-391). The two public `Delegate.CreateDelegate(Type,
            // MethodInfo, ...)` overloads pass `RelaxedSignature` or `OpenDelegateOnly |
            // RelaxedSignature`, but they first require `method is RuntimeMethodInfo`
            // (Delegate.CoreCLR.cs:304, 339), and a `DynamicMethod` is not one — so they cannot
            // deliver a `FromDynamic` handle here at all, and the `FromMetadata` arm below refuses
            // them anyway. `BindToMethodName`'s flag sets belong to a different QCall.
            if flags <> DelegateBindingFlags.relaxedSignature then
                failwith
                    $"TODO: %s{operation} was passed DelegateBindingFlags 0x%08x{flags}; PawPrint implements only DBF_RelaxedSignature (0x%08x{DelegateBindingFlags.relaxedSignature}), which is what every caller that can reach this QCall with a Reflection.Emit method passes"

            let methodHandle =
                match NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation instruction.Arguments.[2] with
                | None ->
                    failwith
                        $"%s{operation}: the method handle is the null sentinel, but Delegate.CreateDelegateNoSecurityCheck rejects a null handle with ArgumentNullException before reaching this QCall"
                | Some id ->
                    MethodHandleRegistry.resolveMethodFromId id state.MethodHandles
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: method-registry id %d{id} names no method in this registry"
                    )

            let declaringTypeTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (EvalStackValue.ofCliType instruction.Arguments.[3])

            match methodHandle with
            | MethodHandle.FromMetadata identity ->
                // Deliberately a host crash and not FALSE, per the note above. This is reachable:
                // `Delegate.CreateDelegate(type, someMethodInfo)` takes exactly this path
                // (Delegate.CoreCLR.cs:395-403).
                failwith
                    $"TODO: %s{operation} was asked to bind a delegate to the metadata method %O{identity.GetMethodDefinitionHandle ()} in %s{identity.GetAssemblyFullName ()}; PawPrint implements this QCall only for a method minted by Reflection.Emit, which is what DynamicMethod.CreateDelegate reaches it with"
            | MethodHandle.FromDynamic dynamicHandle ->

            let definition =
                MethodHandleRegistry.resolveDynamicMethod dynamicHandle state.MethodHandles
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: %O{dynamicHandle} is not registered in the method-handle registry"
                )

            let scopeAssemblyFullName = definition.GetScopeAssemblyFullName ()

            // `methodType` is the declaring type the managed caller read off the same handle
            // (`RuntimeMethodHandle.GetDeclaringType`, Delegate.CoreCLR.cs:389). CoreCLR uses it as
            // the exact instantiation to interpret the target's signature against; a dynamic
            // method has no instantiation, so nothing here needs it. Checked rather than dropped:
            // an argument that is unmarshalled and then ignored is where a mismatch between the
            // handle and the type hides, and this is the one place both are in hand.
            match declaringTypeTarget with
            | RuntimeTypeHandleTarget.DynamicMethodsClass declaringScope when declaringScope = scopeAssemblyFullName ->
                ()
            | other ->
                failwith
                    $"%s{operation}: %O{dynamicHandle} is scoped to %s{scopeAssemblyFullName}, but the methodType argument names %O{other}; these come from the same handle and must agree"

            let scopeAssembly =
                state.LoadedAssembly' scopeAssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: the scope assembly %s{scopeAssemblyFullName} is not loaded"
                )

            // The blob `ModuleHandle_GetDynamicMethod` recorded verbatim. `SignatureHelper`
            // spells any type that is not a primitive, string or object as
            // `ELEMENT_TYPE_INTERNAL`, which the decoder refuses by name, so a dynamic method
            // with a `MyClass` or enum parameter dies at this line rather than being declared
            // incompatible — a separate gap from the compatibility rules below.
            let targetSignature =
                MethodSignatureDecoding.decode
                    scopeAssembly.Name
                    (scopeAssembly.PeReader.GetMetadataReader ())
                    (definition.GetSignature () |> Seq.toArray)
                |> TypeMethodSignature.make

            let state, targetSignature =
                targetSignature
                |> IlMachineState.concretizeMethodSignature
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    scopeAssembly.Name
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            let delegateType = ManagedHeap.getObjectConcreteType delegateAddr state.ManagedHeap

            let state, invokeMethod =
                delegateInvokeMethod ctx.LoggerFactory ctx.BaseClassTypes operation delegateType state

            // The bound argument's type is the *runtime* type of the object supplied, which is
            // what CoreCLR's `refFirstArg->GetTypeHandle()` reads.
            let firstArgType =
                targetAddr
                |> Option.map (fun addr -> ManagedHeap.getObjectConcreteType addr state.ManagedHeap)

            let state, shape =
                isCompatible
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    operation
                    (flags &&& DelegateBindingFlags.relaxedSignature <> 0)
                    firstArgType
                    invokeMethod.Signature
                    targetSignature
                    state

            let state =
                match shape with
                | None -> state
                | Some shape ->
                    // `COMDelegate::BindToMethod` (comdelegate.cpp:1184), reduced to what a
                    // dynamic method needs. Three of its branches cannot fire here.
                    // `NeedsWrapperDelegate` is ARM32-only and instance-virtual-only
                    // (comdelegate.cpp:2053); the open path's virtualisation sub-branch needs a
                    // virtual target; and the `SetMethodBase` tail fires only for a collectible
                    // `LoaderAllocator`, which PawPrint does not model. (`_methodBase`
                    // still ends up holding the `DynamicMethod` — `DynamicMethod.CreateDelegate`
                    // assigns it in managed code straight after this QCall returns, via
                    // `StoreDynamicMethod`, which is why `d.Method` works without anything here.)
                    //
                    // What is left is the field write, and it is where PawPrint's delegate
                    // representation diverges from CoreCLR's: see docs/divergences.md. CoreCLR's
                    // open path stores the delegate itself in `_target`, a shuffle thunk in
                    // `_methodPtr` and the real code address in `_methodPtrAux`. PawPrint has no
                    // shuffle thunks, and `IlMachineRuntimeMetadata.executeDelegateConstructor`
                    // already puts the target in `_target` and the method in `_methodPtr` for
                    // *every* delegate; this follows that convention.
                    let delegateTypeHandle =
                        AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes ctx.BaseClassTypes.DelegateType

                    let delegateField (fieldName : string) : FieldId =
                        FieldIdentity.requiredOwnInstanceField ctx.BaseClassTypes.DelegateType fieldName
                        |> FieldIdentity.fieldId delegateTypeHandle

                    // `_target` is the supplied object for both shapes, because an open binding
                    // cannot have one: `isCompatible` refuses `isOpen && firstArgType.IsSome`, and
                    // `firstArgType` is exactly `targetAddr` mapped. So this is a postcondition to
                    // assert rather than a case to branch on — CoreCLR asserts the same thing at
                    // the top of its open path, `_ASSERTE(pRefFirstArg == NULL || *pRefFirstArg ==
                    // NULL)` (comdelegate.cpp:1215) — and the assertion fails loudly if the guard
                    // above is ever weakened.
                    match shape, targetAddr with
                    | DelegateBindingShape.Open, Some _ ->
                        failwith
                            $"%s{operation}: internal error: classified the binding as open but a target object was supplied; the compatibility check rejects that combination"
                    | DelegateBindingShape.Open, None
                    | DelegateBindingShape.Closed, _ -> ()

                    let heap =
                        state.ManagedHeap
                        |> ManagedHeap.setFieldById
                            delegateAddr
                            (delegateField "_target")
                            (CliType.ObjectRef targetAddr)
                        |> ManagedHeap.setFieldById
                            delegateAddr
                            (delegateField "_methodPtr")
                            (CliType.Numeric (
                                CliNumericType.NativeInt (
                                    NativeIntSource.FunctionPointer (FunctionPointerTarget.Dynamic dynamicHandle)
                                )
                            ))

                    { state with
                        ManagedHeap = heap
                    }

            // The QCall's managed declaration is `[return: MarshalAs(UnmanagedType.Bool)] bool`,
            // so the interop stub receives an Int32 and normalises it; hand back the C `BOOL`.
            let result = if shape.IsSome then 1 else 0

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 result)) ctx.Thread state

            NativeHandlerResult.completed state |> Some

        | "Delegate_FindMethodHandle",
          "System.Private.CoreLib",
          "System",
          "Delegate",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              delegateHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              resultHandleGenerics) ],
          MethodReturnType.Void when delegateHandleGenerics.IsEmpty && resultHandleGenerics.IsEmpty ->
            // `Delegate_FindMethodHandle` (comdelegate.cpp:2122): which method does this delegate
            // point at? `Delegate.GetMethodImpl` asks whenever its `_methodBase` cache is empty,
            // and turns the `IRuntimeMethodInfo` written back here into the `MethodInfo` that
            // `Delegate.Method` hands the guest (Delegate.CoreCLR.cs:159-219).
            let operation = "Delegate_FindMethodHandle"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let delegateAddr =
                NativeCall.objectHandleOnStackTarget operation state "d" instruction.Arguments.[0]
                |> IlMachineState.readManagedByref ctx.BaseClassTypes state
                |> CliType.unwrapPrimitiveLikeDeep
                |> function
                    | CliType.ObjectRef (Some target) -> target
                    | other ->
                        // `Delegate.FindMethodHandle` hands its own `this` to the QCall
                        // (Delegate.CoreCLR.cs:516-521), so the null receiver was already rejected
                        // by the call that reached it.
                        failwith $"%s{operation}: expected the delegate to be an object reference, got %O{other}"

            let result =
                NativeCall.objectHandleOnStackTarget operation state "retMethodInfo" instruction.Arguments.[1]

            let delegateObject = ManagedHeap.get delegateAddr state.ManagedHeap

            let nativeIntField
                (declaringType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
                (fieldName : string)
                : NativeIntSource
                =
                let declaringHandle =
                    AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes declaringType

                let value =
                    FieldIdentity.requiredOwnInstanceField declaringType fieldName
                    |> FieldIdentity.fieldId declaringHandle
                    |> fun fieldId -> AllocatedNonArrayObject.DereferenceFieldById fieldId delegateObject
                    // These fields are typed `IntPtr` (primitive-like); unwrap to the inner NativeInt.
                    |> CliType.unwrapPrimitiveLikeDeep

                match value with
                | CliType.Numeric (CliNumericType.NativeInt src) -> src
                | other -> failwith $"%s{operation}: expected %s{fieldName} to be a native int, got %O{other}"

            // `COMDelegate::GetMethodDesc` (comdelegate.cpp:1815) dispatches on four fields, of
            // which PawPrint populates one. The two guards below are what let the remaining arm
            // read `_methodPtr` honestly: each names a shape whose target method is somewhere
            // else, so falling through to `_methodPtr` would answer a different method rather
            // than fail.
            //
            // `_invocationCount` is declared on `MulticastDelegate`, not on `Delegate`. Every
            // delegate type's immediate base is `MulticastDelegate` -- that is exactly what makes
            // a type a delegate, and `ActivationInfo.classify` spells the same rule -- so the
            // field is present on anything that can reach here. A type deriving straight from
            // `System.Delegate` is legal IL but rejected at type load by CoreCLR; here it would
            // fail loudly in the field lookup rather than answer wrongly.
            let invocationCount =
                nativeIntField ctx.BaseClassTypes.MulticastDelegateType "_invocationCount"

            // Nonzero means an unmanaged function pointer delegate (`_invocationCount == -1`) or
            // an open *virtual* delegate, whose target `MethodDesc` CoreCLR reads out of
            // `_invocationCount` itself (`GetMethodDescForOpenVirtualDelegate`,
            // comdelegate.cpp:1802). Those are the only two shapes that reach this QCall with a
            // count: an ordinary multicast delegate is answered in managed code from the last
            // invocation-list entry, and a wrapper delegate is unwrapped there too
            // (MulticastDelegate.CoreCLR.cs:499-513), so neither gets this far.
            if not (NativeIntSource.isZero invocationCount) then
                failwith
                    $"TODO: %s{operation} was handed a delegate whose _invocationCount is %O{invocationCount}; PawPrint builds no unmanaged-function-pointer or open-virtual delegate, and the target of such a delegate is not the method named in _methodPtr (issue #959)"

            // `_methodPtrAux` is CoreCLR's open-delegate slot: it holds the target's real code
            // address while `_methodPtr` holds a shuffle thunk. PawPrint writes no shuffle thunks
            // and leaves it zero, naming the target in `_methodPtr` for open and closed alike --
            // see docs/divergences.md, "An open delegate stores no shuffle thunk". Zero today
            // because nothing writes it; the managed writer in `MulticastDelegate` does exist
            // (`NewMulticastDelegate`, MulticastDelegate.CoreCLR.cs:168-190, sets it from
            // `GetInvokeMethod`), and is blocked only on that InternalCall, so this guard is what
            // stops #959 turning `Delegate.Method` into a silently wrong answer.
            let methodPtrAux = nativeIntField ctx.BaseClassTypes.DelegateType "_methodPtrAux"

            if not (NativeIntSource.isZero methodPtrAux) then
                failwith
                    $"TODO: %s{operation} was handed a delegate whose _methodPtrAux is %O{methodPtrAux}; PawPrint leaves that field zero and names the target in _methodPtr for open and closed delegates alike (issue #959)"

            let methodPtr =
                match nativeIntField ctx.BaseClassTypes.DelegateType "_methodPtr" with
                | NativeIntSource.FunctionPointer target -> target
                | other -> failwith $"%s{operation}: expected _methodPtr to hold a function pointer, got %O{other}"

            let method =
                match methodPtr with
                | FunctionPointerTarget.Managed method -> method
                | FunctionPointerTarget.Dynamic handle ->
                    // Not reachable, and measured rather than argued:
                    // `DynamicMethod.CreateDelegate` calls `d.StoreDynamicMethod(this)` right
                    // after binding (DynamicMethod.CoreCLR.cs:60), which fills `_methodBase`, and
                    // `Delegate.GetMethodImpl` returns that without consulting the runtime.
                    // `sourcesImpure/DelegateMethodOnDynamicMethod.cs` is what keeps it that way.
                    // Serving it would need a stub over a method with no MethodDef row, which
                    // `MethodHandleRegistry.getOrAllocateStub` cannot mint.
                    failwith
                        $"TODO: %s{operation} was handed a delegate bound to %O{handle}, a method minted by Reflection.Emit; DynamicMethod.CreateDelegate caches that MethodInfo in _methodBase, so Delegate.Method answers from there and never reaches this QCall"
                | FunctionPointerTarget.RuntimeAllocator ->
                    // The JIT's `newobj` helper: it lives in `ActivatorCache._pfnAllocator` and is
                    // reached by `calli`, and nothing stores it in a delegate.
                    failwith
                        $"TODO: %s{operation} was handed a delegate whose _methodPtr is the runtime's newobj allocation helper, which has no MethodInfo to report"

            // A null `_target` on an *instance* target arises two ways here, and PawPrint's
            // representation cannot tell them apart: a legal open instance delegate, where
            // `Invoke` supplies the receiver and CoreCLR records the target in `_methodPtrAux`
            // (which PawPrint does not write -- docs/divergences.md, "An open delegate stores no
            // shuffle thunk"); and an illegal delegate closed over a null receiver, which
            // CoreCLR's `CtorClosed` refuses with `ArgumentException(Arg_DlgtNullInst)`
            // (MulticastDelegate.CoreCLR.cs:552-556) and `executeDelegateConstructor` does not.
            //
            // Neither is refused for its own sake -- the method this handler resolves is right in
            // both cases. What is refused is handing it back when CoreLib will then fault on it:
            // `Delegate.GetMethodImpl` dereferences `_target` to walk the base chain whenever the
            // target is an instance method on a *generic* declaring type
            // (Delegate.CoreCLR.cs:189), because a zero `_methodPtrAux` sends it down the closed
            // branch. Measured: the guest gets a NullReferenceException where real .NET returns a
            // MethodInfo. Off a non-generic declaring type that branch is never entered and the
            // answer is correct, so those are served.
            //
            // No test reaches this: the illegal shape is parked as
            // `sourcesPure/DelegateOverNullInstanceReceiver.cs`, and the legal open one needs
            // either raw `ldnull; ldftn; newobj` IL, which the C# harness cannot emit, or
            // `Delegate.CreateDelegate(Type, MethodInfo)`, which `Delegate_BindToMethodInfo`
            // refuses first.
            let targetIsNull =
                let delegateTypeHandle =
                    AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes ctx.BaseClassTypes.DelegateType

                FieldIdentity.requiredOwnInstanceField ctx.BaseClassTypes.DelegateType "_target"
                |> FieldIdentity.fieldId delegateTypeHandle
                |> fun fieldId -> AllocatedNonArrayObject.DereferenceFieldById fieldId delegateObject
                |> CliType.unwrapPrimitiveLikeDeep
                |> function
                    | CliType.ObjectRef target -> target.IsNone
                    | other -> failwith $"%s{operation}: expected _target to be an object reference, got %O{other}"

            // A concretised declaring type is closed, so a non-empty instantiation is exactly
            // what `RuntimeType.IsGenericType` answers true to.
            let declaringTypeIsGeneric = not method.DeclaringTypeGenerics.IsEmpty

            if not method.IsStatic && targetIsNull && declaringTypeIsGeneric then
                failwith
                    $"TODO: %s{operation} was handed a delegate over the instance method %s{method.Name} on a generic declaring type, with a null _target; Delegate.GetMethodImpl would dereference _target to walk its base chain, because PawPrint leaves the _methodPtrAux that would send it down the open-delegate branch at zero"

            // CoreCLR follows `GetMethodDesc` with
            // `FindOrCreateAssociatedMethodDescForReflection`, whose whole job is to replace a
            // *shared* (`__Canon`) `MethodDesc` -- or an unboxing or instantiating stub -- with
            // the exact one reflection is allowed to expose. Nothing to do here:
            // `FunctionPointerTarget.Managed` carries a fully concretised method, and PawPrint has
            // no shared method representation for it to have been one of.
            //
            // The stub itself is deduplicated rather than freshly allocated per call as CoreCLR's
            // `AllocateStubMethodInfo` does. That is unobservable: the stub is consumed by
            // `RuntimeType.GetMethodBase` and never reaches the guest, and sharing it with the
            // `ldtoken` path is what makes `someDelegate.Method` and `GetMethod(...)` agree.
            let runtimeMethodInfoStubType =
                AllConcreteTypes.getRequiredNonGenericHandle
                    state.ConcreteTypes
                    ctx.BaseClassTypes.RuntimeMethodInfoStub

            let stubAddress, registry, state =
                MethodHandleRegistry.getOrAllocateStub
                    ctx.BaseClassTypes
                    state.ConcreteTypes
                    state
                    (fun fields state -> IlMachineState.allocateManagedObject runtimeMethodInfoStubType fields state)
                    method
                    state.MethodHandles

            let state =
                { state with
                    MethodHandles = registry
                }

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    result
                    (CliType.ObjectRef (Some stubAddress))

            NativeHandlerResult.completed state |> Some

        | _ -> None

namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// CoreCLR's constant fold of `typeof(X).IsValueType`, performed by the interpreter at the
/// `ldtoken` that begins it.
///
/// The JIT never runs `Type.get_IsValueType`'s body for a literal `typeof` receiver. Its importer
/// sees the getter's receiver as the `GetTypeFromHandle` helper call over a class handle -- a
/// literal token, or the generic-dictionary lookup that `typeof(T)` in shared code compiles to --
/// and replaces the getter call with `TypeHandle::IsValueType` of that handle (`impIntrinsic`,
/// importercalls.cpp:4044-4077; `gtIsTypeof` and `gtGetHelperArgClassHandle`, gentree.cpp).
/// `IsPrimitive`, `IsEnum`, `IsByRefLike` and `IsGenericType` share that arm; only `IsValueType`
/// is folded here, because it is the one that is hot: `Dictionary<TKey, TValue>` asks it on every
/// lookup and insert, and interpreted it is a forty-step chain through
/// `RuntimeType.IsValueTypeImpl` and the MethodTable projection.
///
/// The answer is `IntrinsicHelpers.isValueTypeHandleAsCoreClr`, which is the flag that projection
/// reads, so a folded question and an interpreted one cannot disagree.
[<RequireQualifiedAccess>]
module internal TypeofIntrinsicFold =

    /// Whether `method` is declared on CoreLib's `System.Type`. The method's own name is checked
    /// by the caller, which has it before resolving anything.
    let private isDeclaredOnCorelibSystemType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<'typeGen, 'methodGen, 'methodVars>)
        : bool
        =
        let corelib = baseClassTypes.Corelib

        method.DeclaringAssemblyFullName = corelib.DefinitionFullName
        && (
            match method.TryDeclaringType with
            | None -> false
            | Some declaringType ->
                let typeInfo = corelib.TypeDefs.[declaringType.Definition.Get]
                TypeInfo.fullName (fun h -> corelib.TypeDefs.[h]) typeInfo = "System.Type"
        )

    /// Whether `op` is a `call` -- or, for `allowVirtual`, a `callvirt` -- whose token names the
    /// CoreLib `System.Type` method called `name`.
    ///
    /// The token belongs to the same body as the executing `ldtoken`, so it indexes `activeAssy`'s
    /// tables: a body's tokens are decoded against one module. A `DynamicMethod` body's operands
    /// name scope entries instead, and are not folded.
    ///
    /// The row's own name is read before anything is resolved, so the common `typeof(X)` that
    /// feeds some other call costs two dictionary reads and no resolution. A `MemberReference`
    /// whose name matches is resolved through the memoised `resolveMember`, which the `call` itself
    /// would have done: on a miss the state carries that resolution whether or not the fold fires.
    let private callsCorelibSystemTypeMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (activeAssy : DumpedAssembly)
        (allowVirtual : bool)
        (name : string)
        (op : IlOp)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let isCallOfRightKind (kind : UnaryMetadataTokenIlOp) : bool =
            match kind with
            | UnaryMetadataTokenIlOp.Call -> true
            | UnaryMetadataTokenIlOp.Callvirt -> allowVirtual
            | _ -> false

        match op with
        | IlOp.UnaryMetadataToken (kind, MetadataOperand.FromMetadata sourced) when isCallOfRightKind kind ->
            match sourced.Token with
            | MetadataToken.MethodDef h ->
                match activeAssy.Methods.TryGetValue h with
                | true, method when method.Name = name -> state, isDeclaredOnCorelibSystemType baseClassTypes method
                | _ -> state, false
            | MetadataToken.MemberReference h ->
                match activeAssy.Members.TryGetValue h with
                | true, row when row.PrettyName = name ->
                    let state, _, resolved, _ =
                        IlMachineState.resolveMember loggerFactory baseClassTypes thread activeAssy h state

                    match resolved with
                    | Choice1Of2 method -> state, isDeclaredOnCorelibSystemType baseClassTypes method
                    | Choice2Of2 _field -> state, false
                | _ -> state, false
            | _ -> state, false
        | _ -> state, false

    /// Fold `ldtoken X; call Type::GetTypeFromHandle; call[virt] Type::get_IsValueType`, when the
    /// executing instruction is that `ldtoken` and `handle` is the closed type it named.
    ///
    /// Returns `true` with the getter's answer pushed and the program counter moved past the getter
    /// call; or `false` with nothing pushed and the program counter where it was, when the next two
    /// instructions are not that pair. Either way the state may carry a token resolution the pair
    /// would have performed itself.
    ///
    /// Sound however control reaches the three instructions: this runs only when the `ldtoken` is
    /// the one being executed, and what it pushes and where it lands are what executing the three
    /// in sequence would have pushed and landed. A branch into the `call` or the getter from
    /// elsewhere executes those instructions as normal. The `RuntimeType` that `GetTypeFromHandle`
    /// would have materialised is not materialised, as under the JIT.
    let tryFoldIsValueType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (activeAssy : DumpedAssembly)
        (currentMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (handle : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let instructions =
            match currentMethod.Body with
            | MethodBody.Il instructions -> instructions
            | other ->
                failwith $"BUG: an ldtoken is executing in %O{currentMethod}, which has no IL body (Body=%A{other})"

        let ldtokenOffset = state.ThreadState.[thread].MethodState.IlOpIndex
        let ldtokenOp = instructions.Locations.[ldtokenOffset]
        let callOffset = ldtokenOffset + IlOp.NumberOfBytes ldtokenOp

        match Map.tryFind callOffset instructions.Locations with
        | None -> state, false
        | Some callOp ->

        let state, isGetTypeFromHandle =
            callsCorelibSystemTypeMethod
                loggerFactory
                baseClassTypes
                thread
                activeAssy
                false
                "GetTypeFromHandle"
                callOp
                state

        if not isGetTypeFromHandle then
            state, false
        else

        let getterOffset = callOffset + IlOp.NumberOfBytes callOp

        match Map.tryFind getterOffset instructions.Locations with
        | None -> state, false
        | Some getterOp ->

        let state, isGetter =
            callsCorelibSystemTypeMethod
                loggerFactory
                baseClassTypes
                thread
                activeAssy
                true
                "get_IsValueType"
                getterOp
                state

        if not isGetter then
            state, false
        else

        let answer =
            IntrinsicHelpers.isValueTypeHandleAsCoreClr baseClassTypes state "typeof(X).IsValueType fold" handle

        let resumeAt = getterOffset + IlOp.NumberOfBytes getterOp

        let state =
            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool answer) thread
            |> IlMachineState.jumpProgramCounter thread (resumeAt - ldtokenOffset)

        state, true

namespace WoofWare.PawPrint.Analysis

open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

/// Why part of a method's behaviour is hidden from the analysis. Each is a place where an exception
/// the analysis cannot name may arise, which is what makes <c>Escapes.Unknown</c> true.
[<RequireQualifiedAccess>]
type Opacity =
    /// A <c>callvirt</c> to a method that may be overridden: the method named may not be the one
    /// that runs.
    | VirtualCall
    /// <c>calli</c>, or a call through a dynamic method's scope: no metadata names the target.
    | IndirectCall
    /// The method is <c>InternalCall</c>, <c>PInvoke</c> or runtime-provided: its behaviour is not IL.
    | NativeBody
    /// The method is abstract.
    | AbstractBody
    /// The method is an <c>[Intrinsic]</c> whose IL is not its semantics: a placeholder the JIT
    /// expands, or a body the VM replaces.
    | IntrinsicExpansion
    /// A MemberRef whose target turns on how a type variable of this method is instantiated.
    | DependsOnInstantiation
    /// A <c>throw</c> whose operand's type the analysis does not know.
    | UntypedThrow
    /// A <c>rethrow</c>: what it raises is what the enclosing handler caught.
    | Rethrow
    /// An instruction <c>OpcodeFaults</c> declines to classify.
    | UnmodelledOpcode

/// What one method body does by itself: the exceptions it raises, the places it cannot see
/// through, and the methods it calls, each at the IL offset where it happens, so that the body's
/// own exception clauses can be applied to all three alike.
type internal LocalFacts =
    {
        Raises : (int * ThrownType) list
        Opaque : (int * Opacity) list
        Calls : (int * MethodKey) list
        Regions : ExceptionRegion list
    }

/// What a call instruction's token names.
[<RequireQualifiedAccess>]
type internal CallTarget =
    | Method of MethodKey
    | ArrayAccessor of ArrayAccessor
    | Missing
    | DependsOnInstantiation

/// An escape analysis in progress: the assemblies loaded so far and every answer computed so far.
/// Immutable; each query returns the state to ask the next one of.
type EscapeAnalysisState =
    private
        {
            LoggerFactory : ILoggerFactory
            RuntimeDirs : string list
            Context : TypeConcretization.ConcretizationContext<DumpedAssembly>
            Facts : Map<MethodKey, LocalFacts>
            Summaries : Map<MethodKey, Escapes>
            /// Each type definition's base type, as far as it has been asked; `None` at the root.
            Bases : Map<ResolvedTypeIdentity, ResolvedTypeIdentity option>
        }

/// <summary>
/// Which exceptions can escape a method, computed from its IL and that of everything it calls,
/// across assemblies, without running any of it.
/// </summary>
/// <remarks>
/// The answer is an over-approximation: every exception a run of the method can let escape is in
/// it, either named in <c>Escapes.Types</c> or covered by <c>Escapes.Unknown</c>. That includes
/// the faults the runtime raises by itself (<c>OpcodeFaults</c>), resource exhaustion among them,
/// so almost every method can escape <c>StackOverflowException</c>; a report that wants to drop
/// those does so knowingly.
///
/// A method's summary is computed once and shared by every instantiation of it: a generic
/// method's IL is the same for all of them, and the calls whose target an instantiation decides
/// are opaque.
/// </remarks>
[<RequireQualifiedAccess>]
module EscapeAnalysis =

    /// Begin an analysis over the assemblies `context` has loaded, loading any others it needs from
    /// `runtimeDirs`.
    let create
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (context : TypeConcretization.ConcretizationContext<DumpedAssembly>)
        : EscapeAnalysisState
        =
        {
            LoggerFactory = loggerFactory
            RuntimeDirs = List.ofSeq runtimeDirs
            Context = context
            Facts = Map.empty
            Summaries = Map.empty
            Bases = Map.empty
        }

    let private assemblyOf (state : EscapeAnalysisState) (fullName : string) : DumpedAssembly =
        state.Context.LoadedAssemblies.ByDefinitionName fullName

    let private withAssemblies (state : EscapeAnalysisState) (assemblies : LoadedAssemblies) : EscapeAnalysisState =
        { state with
            Context =
                { state.Context with
                    LoadedAssemblies = assemblies
                }
        }

    /// The CoreLib type an `OpcodeFault` names.
    let private faultType (state : EscapeAnalysisState) (fault : OpcodeFault) : ResolvedTypeIdentity =
        let qualified = OpcodeFault.typeName fault
        let dot = qualified.LastIndexOf '.'
        let ns, name = qualified.Substring (0, dot), qualified.Substring (dot + 1)

        match state.Context.BaseTypes.Corelib.TryGetTopLevelTypeDef ns name with
        | Some ty -> ty.Identity
        | None -> failwith $"CoreLib declares no %s{qualified}, which OpcodeFaults names"

    /// The CoreLib exception type of this name, which the runtime raises by itself.
    let private corelibException (state : EscapeAnalysisState) (name : string) : ResolvedTypeIdentity =
        match state.Context.BaseTypes.Corelib.TryGetTopLevelTypeDef "System" name with
        | Some ty -> ty.Identity
        | None -> failwith $"CoreLib declares no System.%s{name}"

    let private resolveTypeRef
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (typeRef : TypeRef)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match
            TypeResolution.resolveTypeRefIdentity
                state.LoggerFactory
                state.RuntimeDirs
                assembly
                typeRef
                state.Context.LoadedAssemblies
        with
        | assemblies, Ok identity -> withAssemblies state assemblies, Some identity
        | assemblies, Error _ -> withAssemblies state assemblies, None

    /// The type definition a type spelling names, looking through custom modifiers and
    /// instantiations. `None` for a spelling that names no definition: a type variable, an array, a
    /// pointer.
    let private nominalIdentity
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericInstantiation (root, _) ->
            match TypeDefn.stripCustomModifiers root with
            | TypeDefn.FromDefinition (identity, _) -> state, Some identity
            | TypeDefn.FromReference (typeRef, _) -> resolveTypeRef state assembly typeRef
            | _ -> state, None
        | TypeDefn.FromDefinition (identity, _) -> state, Some identity
        | TypeDefn.FromReference (typeRef, _) -> resolveTypeRef state assembly typeRef
        | TypeDefn.PrimitiveType primitive ->
            state, Some (BaseClassTypes.ofPrimitive state.Context.BaseTypes primitive).Identity
        | _ -> state, None

    let private definitionOf
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let assembly = assemblyOf state identity.AssemblyFullName
        assembly, assembly.TypeDefs.[identity.TypeDefinition.Get]

    let private baseOf
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match state.Bases.TryFind identity with
        | Some known -> state, known
        | None ->
            let assembly, ty = definitionOf state identity

            let state, result =
                match ty.BaseType with
                | None -> state, None
                | Some (BaseTypeInfo.TypeDef handle) -> state, Some assembly.TypeDefs.[handle].Identity
                | Some (BaseTypeInfo.TypeRef handle) -> resolveTypeRef state assembly assembly.TypeRefs.[handle]
                | Some (BaseTypeInfo.TypeSpec handle) ->
                    nominalIdentity state assembly assembly.TypeSpecs.[handle].Signature

            { state with
                Bases = state.Bases.Add (identity, result)
            },
            result

    /// Is `ty`, or some type it derives from, `ancestor`? Answered on type definitions, which is
    /// exact for a non-generic ancestor: every instantiation of a type shares its definition's base
    /// chain.
    let private derivesFrom
        (state : EscapeAnalysisState)
        (ty : ResolvedTypeIdentity)
        (ancestor : ResolvedTypeIdentity)
        : EscapeAnalysisState * bool
        =
        // Metadata could make a chain cyclic; CoreCLR would refuse to load it.
        let rec go (state : EscapeAnalysisState) (current : ResolvedTypeIdentity) (depth : int) =
            if current = ancestor then
                state, true
            elif depth > 256 then
                failwith $"The base chain of %O{ty} is more than 256 types long, which suggests a cycle"
            else
                match baseOf state current with
                | state, Some parent -> go state parent (depth + 1)
                | state, None -> state, false

        go state ty 0

    /// The type a `catch` clause names, when it is one this analysis can compare against: a TypeDef
    /// or TypeRef. A TypeSpec names an instantiation, and deciding whether an exception is one needs
    /// its arguments, so such a clause is treated as catching nothing, which over-reports.
    let private catchType
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match token with
        | MetadataToken.TypeDefinition handle -> state, Some assembly.TypeDefs.[handle].Identity
        | MetadataToken.TypeReference handle -> resolveTypeRef state assembly assembly.TypeRefs.[handle]
        | _ -> state, None

    /// Does an exception raised at `offset` get past this body's handlers? `thrown` is `None` for
    /// one the analysis cannot name, which only a clause catching everything stops. A `finally` or
    /// `fault` never stops one, and a `filter` may decline, so neither counts.
    let private escapesHandlers
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (regions : ExceptionRegion list)
        (offset : int)
        (thrown : ThrownType option)
        : EscapeAnalysisState * bool
        =
        let catchesEverything (caught : ResolvedTypeIdentity) : bool =
            caught = state.Context.BaseTypes.Object.Identity
            || caught = state.Context.BaseTypes.Exception.Identity

        let rec go (state : EscapeAnalysisState) (regions : ExceptionRegion list) =
            match regions with
            | [] -> state, true
            | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, o) :: rest when
                offset >= o.TryOffset && offset < o.TryOffset + o.TryLength
                ->
                match catchType state assembly token with
                | state, Some caught when catchesEverything caught -> state, false
                | state, Some caught ->
                    match thrown with
                    | Some (ThrownType.Exactly ty)
                    | Some (ThrownType.SubtypeOf ty) ->
                        match derivesFrom state ty caught with
                        | state, true -> state, false
                        | state, false -> go state rest
                    | None -> go state rest
                | state, None -> go state rest
            | _ :: rest -> go state rest

        go state regions

    /// How far a branch instruction jumps from the end of itself, or `None` if it is not a branch.
    /// Exhaustive with no wildcard, so that a branch opcode added to `UnaryConstIlOp` must be
    /// classified here rather than silently read as falling through.
    let private branchDelta (op : UnaryConstIlOp) : int option =
        match op with
        | UnaryConstIlOp.Br d
        | UnaryConstIlOp.Brfalse d
        | UnaryConstIlOp.Brtrue d
        | UnaryConstIlOp.Beq d
        | UnaryConstIlOp.Blt d
        | UnaryConstIlOp.Ble d
        | UnaryConstIlOp.Bgt d
        | UnaryConstIlOp.Bge d
        | UnaryConstIlOp.Bne_un d
        | UnaryConstIlOp.Bge_un d
        | UnaryConstIlOp.Bgt_un d
        | UnaryConstIlOp.Ble_un d
        | UnaryConstIlOp.Blt_un d
        | UnaryConstIlOp.Leave d -> Some (int d)
        | UnaryConstIlOp.Br_s d
        | UnaryConstIlOp.Brfalse_s d
        | UnaryConstIlOp.Brtrue_s d
        | UnaryConstIlOp.Beq_s d
        | UnaryConstIlOp.Blt_s d
        | UnaryConstIlOp.Ble_s d
        | UnaryConstIlOp.Bgt_s d
        | UnaryConstIlOp.Bge_s d
        | UnaryConstIlOp.Bne_un_s d
        | UnaryConstIlOp.Bge_un_s d
        | UnaryConstIlOp.Bgt_un_s d
        | UnaryConstIlOp.Ble_un_s d
        | UnaryConstIlOp.Blt_un_s d
        | UnaryConstIlOp.Leave_s d -> Some (int d)
        | UnaryConstIlOp.Stloc _
        | UnaryConstIlOp.Stloc_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Ldloc_s _
        | UnaryConstIlOp.Ldloca_s _
        | UnaryConstIlOp.Ldarga _
        | UnaryConstIlOp.Ldarg_s _
        | UnaryConstIlOp.Ldarga_s _
        | UnaryConstIlOp.Starg_s _
        | UnaryConstIlOp.Starg _
        | UnaryConstIlOp.Unaligned _
        | UnaryConstIlOp.Ldloc _
        | UnaryConstIlOp.Ldloca _
        | UnaryConstIlOp.Ldarg _ -> None

    /// What a call instruction's metadata token names.
    let rec private callTarget
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * CallTarget
        =
        match token with
        | MetadataToken.MethodDef handle -> state, CallTarget.Method (MethodKey.make assembly handle)
        | MetadataToken.MethodSpecification handle -> callTarget state assembly assembly.MethodSpecs.[handle].Method
        | MetadataToken.MemberReference handle ->
            let ctx, target =
                MethodReferenceResolution.resolve state.LoggerFactory state.RuntimeDirs state.Context assembly handle

            let state =
                { state with
                    Context = ctx
                }

            match target with
            | MethodReferenceTarget.Defined (declaring, method) ->
                state, CallTarget.Method (MethodKey.make declaring method)
            | MethodReferenceTarget.ArrayMethod (_, accessor) -> state, CallTarget.ArrayAccessor accessor
            | MethodReferenceTarget.Missing -> state, CallTarget.Missing
            | MethodReferenceTarget.DependsOnInstantiation -> state, CallTarget.DependsOnInstantiation
        | other -> failwith $"A call in %s{assembly.DefinitionFullName} names %O{other}, which is not a method"

    let private methodOf
        (state : EscapeAnalysisState)
        (key : MethodKey)
        : DumpedAssembly * MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        let assembly = assemblyOf state key.AssemblyFullName
        assembly, assembly.Methods.[key.Method.Get]

    let private declaringTypeOf (state : EscapeAnalysisState) (key : MethodKey) : ResolvedTypeIdentity =
        let _, method = methodOf state key
        method.RequiredDeclaringType.Identity

    /// Can the method a `callvirt` names be overridden, so that some other method may run?
    let private isOverridable (state : EscapeAnalysisState) (key : MethodKey) : bool =
        let _, method = methodOf state key

        if not method.IsVirtual || method.IsFinal then
            false
        else
            let _, declaring = definitionOf state (declaringTypeOf state key)
            not (declaring.TypeAttributes.HasFlag TypeAttributes.Sealed)

    let private hasTypeInitializer (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) : bool =
        let _, ty = definitionOf state identity
        ty.Methods |> List.exists (fun m -> m.Name = ".cctor" && m.IsStatic)

    /// The runtime's own exceptions from one of the methods it supplies on an array type: those of
    /// the instruction each stands in for, `ldelem`, `stelem.ref`, `ldelema` and `newarr`.
    let private arrayAccessorFaults (accessor : ArrayAccessor) : OpcodeFault list =
        match accessor with
        | ArrayAccessor.Get -> [ OpcodeFault.NullReference ; OpcodeFault.IndexOutOfRange ]
        | ArrayAccessor.Set ->
            [
                OpcodeFault.NullReference
                OpcodeFault.IndexOutOfRange
                OpcodeFault.ArrayTypeMismatch
            ]
        | ArrayAccessor.Address ->
            [
                OpcodeFault.NullReference
                OpcodeFault.IndexOutOfRange
                OpcodeFault.ArrayTypeMismatch
            ]
        | ArrayAccessor.Constructor _ -> [ OpcodeFault.Overflow ; OpcodeFault.OutOfMemory ]

    /// The static type of the value a call returns, as the call site's own signature spells it.
    let rec private returnTypeOfCall
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        let ofReturn (ret : MethodReturnType<TypeDefn>) =
            match ret with
            | MethodReturnType.Void -> state, None
            | MethodReturnType.Returns ty -> nominalIdentity state assembly ty

        match token with
        | MetadataToken.MethodDef handle -> ofReturn assembly.Methods.[handle].Signature.ReturnType
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Signature with
            | MemberSignature.Method signature -> ofReturn signature.ReturnType
            | MemberSignature.Field _ -> state, None
        | MetadataToken.MethodSpecification handle ->
            returnTypeOfCall state assembly assembly.MethodSpecs.[handle].Method
        | _ -> state, None

    let private opaqueFromEntry (reason : Opacity) : LocalFacts =
        {
            Raises = []
            Opaque = [ 0, reason ]
            Calls = []
            Regions = []
        }

    /// What one body does by itself.
    let private factsOf (state : EscapeAnalysisState) (key : MethodKey) : EscapeAnalysisState * LocalFacts =
        let assembly, method = methodOf state key

        let intrinsicWithoutSemantics =
            IntrinsicBody.isIntrinsic assembly key.Method.Get
            && match IntrinsicBody.classify assembly key.Method.Get with
               | IntrinsicBody.OwnIl -> false
               | _ -> true

        if intrinsicWithoutSemantics then
            state, opaqueFromEntry Opacity.IntrinsicExpansion
        else

        match method.Body with
        | MethodBody.Abstract -> state, opaqueFromEntry Opacity.AbstractBody
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _ -> state, opaqueFromEntry Opacity.NativeBody
        | MethodBody.Il body ->

        let ops = body.Instructions |> Array.ofList

        // Offsets some branch or handler can land on: a `throw` at one of these may be reached
        // from somewhere other than the instruction above it, so that instruction is not
        // necessarily what produced its operand.
        let entered =
            let fromBranches =
                ops
                |> Seq.collect (fun (op, offset) ->
                    let width = IlOp.NumberOfBytes op

                    match op with
                    | IlOp.UnaryConst c ->
                        match branchDelta c with
                        | Some delta -> [ offset + width + delta ]
                        | None -> []
                    | IlOp.Switch deltas -> deltas |> Seq.map (fun d -> offset + width + d) |> List.ofSeq
                    | _ -> []
                )

            let fromHandlers =
                body.ExceptionRegions
                |> Seq.collect (fun region ->
                    match region with
                    | ExceptionRegion.Filter (filterOffset, o) -> [ filterOffset ; o.HandlerOffset ]
                    | ExceptionRegion.Catch (_, o)
                    | ExceptionRegion.Finally o
                    | ExceptionRegion.Fault o -> [ o.HandlerOffset ]
                )

            Seq.append fromBranches fromHandlers |> Set.ofSeq

        // Inside a type initializer, touching the type it initializes cannot trigger it: the CLI
        // lets the initializing thread straight through (ECMA-335 I.8.9.5).
        let initializing =
            if method.Name = ".cctor" && method.IsStatic then
                Some method.RequiredDeclaringType.Identity
            else
                None

        // Whether a `TypeInitializationException` from this instruction is impossible: the type it
        // touches has no initializer to fail, or is the one this body is initializing. A
        // `callvirt` names where dispatch starts rather than where it lands, so it is never pruned.
        let typeInitializationImpossible (state : EscapeAnalysisState) (op : IlOp) : EscapeAnalysisState * bool =
            let owner (state : EscapeAnalysisState) : EscapeAnalysisState * ResolvedTypeIdentity option =
                match op with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp),
                                           MetadataOperand.FromMetadata token) ->
                    match callTarget state assembly token.Token with
                    | state, CallTarget.Method callee -> state, Some (declaringTypeOf state callee)
                    | state, _ -> state, None
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Ldsfld | UnaryMetadataTokenIlOp.Stsfld | UnaryMetadataTokenIlOp.Ldsflda),
                                           MetadataOperand.FromMetadata token) ->
                    match token.Token with
                    | MetadataToken.FieldDefinition handle ->
                        state, Some assembly.Fields.[handle].DeclaringType.Identity
                    | MetadataToken.MemberReference handle ->
                        match assembly.Members.[handle].Parent with
                        | MetadataToken.TypeDefinition parent -> state, Some assembly.TypeDefs.[parent].Identity
                        | MetadataToken.TypeReference parent -> resolveTypeRef state assembly assembly.TypeRefs.[parent]
                        | MetadataToken.TypeSpecification parent ->
                            nominalIdentity state assembly assembly.TypeSpecs.[parent].Signature
                        | _ -> state, None
                    | _ -> state, None
                | _ -> state, None

            match owner state with
            | state, Some owner -> state, (Some owner = initializing || not (hasTypeInitializer state owner))
            | state, None -> state, false

        let folder
            (
                state : EscapeAnalysisState,
                raises : (int * ThrownType) list,
                opaque : (int * Opacity) list,
                calls : (int * MethodKey) list
            )
            (index : int)
            =
            let op, offset = ops.[index]

            // 1. What the instruction raises by itself.
            let state, raises, opaque =
                match OpcodeFaults.ofIlOp op with
                | OpcodeFaults.Unmodelled ->
                    let reason =
                        match op with
                        | IlOp.Nullary NullaryIlOp.Rethrow -> Opacity.Rethrow
                        | _ -> Opacity.UnmodelledOpcode

                    state, raises, (offset, reason) :: opaque
                | OpcodeFaults.Raises faults ->
                    let state, raises =
                        ((state, raises), faults)
                        ||> List.fold (fun (state, raises) fault ->
                            let state, impossible =
                                if fault = OpcodeFault.TypeInitialization then
                                    typeInitializationImpossible state op
                                else
                                    state, false

                            if impossible then
                                state, raises
                            else
                                state, (offset, ThrownType.Exactly (faultType state fault)) :: raises
                        )

                    state, raises, opaque

            // 2. What the instruction throws explicitly.
            let state, raises, opaque =
                match op with
                | IlOp.Nullary NullaryIlOp.Throw ->
                    let previous =
                        if index > 0 && not (entered.Contains offset) then
                            Some (fst ops.[index - 1])
                        else
                            None

                    let state, thrown =
                        match previous with
                        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj,
                                                         MetadataOperand.FromMetadata token)) ->
                            match callTarget state assembly token.Token with
                            | state, CallTarget.Method constructor ->
                                state, Some (ThrownType.Exactly (declaringTypeOf state constructor))
                            | state, _ -> state, None
                        | Some (IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt),
                                                         MetadataOperand.FromMetadata token)) ->
                            match returnTypeOfCall state assembly token.Token with
                            | state, Some ty -> state, Some (ThrownType.SubtypeOf ty)
                            | state, None -> state, None
                        | _ -> state, None

                    match thrown with
                    | Some thrown -> state, (offset, thrown) :: raises, opaque
                    | None -> state, raises, (offset, Opacity.UntypedThrow) :: opaque
                | _ -> state, raises, opaque

            // 3. What the instruction calls.
            match op with
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, _) ->
                state, raises, (offset, Opacity.IndirectCall) :: opaque, calls
            | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp) as call,
                                       operand) ->
                match operand with
                | MetadataOperand.FromDynamicScope _ -> state, raises, (offset, Opacity.IndirectCall) :: opaque, calls
                | MetadataOperand.FromMetadata token ->
                    match callTarget state assembly token.Token with
                    | state, CallTarget.Method callee ->
                        if call = UnaryMetadataTokenIlOp.Callvirt && isOverridable state callee then
                            state, raises, (offset, Opacity.VirtualCall) :: opaque, calls
                        else
                            state, raises, opaque, (offset, callee) :: calls
                    | state, CallTarget.ArrayAccessor accessor ->
                        let raised =
                            arrayAccessorFaults accessor
                            |> List.map (fun fault -> offset, ThrownType.Exactly (faultType state fault))

                        state, raised @ raises, opaque, calls
                    | state, CallTarget.Missing ->
                        state,
                        (offset, ThrownType.Exactly (corelibException state "MissingMethodException"))
                        :: raises,
                        opaque,
                        calls
                    | state, CallTarget.DependsOnInstantiation ->
                        state, raises, (offset, Opacity.DependsOnInstantiation) :: opaque, calls
            | _ -> state, raises, opaque, calls

        let state, raises, opaque, calls =
            ((state, [], [], []), [ 0 .. ops.Length - 1 ]) ||> List.fold folder

        state,
        {
            Raises = List.rev raises
            Opaque = List.rev opaque
            Calls = List.rev calls
            Regions = List.ofSeq body.ExceptionRegions
        }

    /// The full name of a type the analysis has loaded, for reporting.
    let typeName (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) : string =
        let assembly, ty = definitionOf state identity
        TypeInfo.fullName (fun handle -> assembly.TypeDefs.[handle]) ty

    /// <summary>
    /// What may escape `method`, and the state to ask the next question of.
    /// </summary>
    /// <remarks>
    /// Computes the summary of every method `method` can reach through calls whose target the
    /// analysis can name and has not summarised already, loading whatever assemblies they live in,
    /// and remembers them all.
    /// </remarks>
    let escapes (state : EscapeAnalysisState) (method : MethodKey) : EscapeAnalysisState * Escapes =
        match state.Summaries.TryFind method with
        | Some known -> state, known
        | None ->

        // Every method reachable from `method` that has no summary yet, with its local facts.
        let rec discover (state : EscapeAnalysisState) (pending : MethodKey list) (found : MethodKey list) =
            match pending with
            | [] -> state, found
            | key :: rest when state.Summaries.ContainsKey key || state.Facts.ContainsKey key ->
                discover state rest found
            | key :: rest ->
                let state, facts = factsOf state key

                let state =
                    { state with
                        Facts = state.Facts.Add (key, facts)
                    }

                discover state ((facts.Calls |> List.map snd) @ rest) (key :: found)

        let state, reachable = discover state [ method ] []

        // Whether a raise at `offset` of `key`'s body gets out of it.
        let escapesAt (state : EscapeAnalysisState) (key : MethodKey) (offset : int) (thrown : ThrownType option) =
            let facts = state.Facts.[key]
            escapesHandlers state (assemblyOf state key.AssemblyFullName) facts.Regions offset thrown

        let seedOf (state : EscapeAnalysisState) (key : MethodKey) : EscapeAnalysisState * Escapes =
            let facts = state.Facts.[key]

            let state, types =
                ((state, Set.empty), facts.Raises)
                ||> List.fold (fun (state, types) (offset, thrown) ->
                    match escapesAt state key offset (Some thrown) with
                    | state, true -> state, Set.add thrown types
                    | state, false -> state, types
                )

            let state, unknown =
                ((state, false), facts.Opaque)
                ||> List.fold (fun (state, unknown) (offset, _) ->
                    if unknown then
                        state, true
                    else
                        escapesAt state key offset None
                )

            state,
            {
                Types = types
                Unknown = unknown
            }

        let state, seeds =
            ((state, Map.empty), reachable)
            ||> List.fold (fun (state, seeds) key ->
                let state, seed = seedOf state key
                state, Map.add key seed seeds
            )

        // Iterate to the least fixed point: each method lets escape its own seed and whatever its
        // callees let escape past its handlers.
        let rec iterate (state : EscapeAnalysisState) (current : Map<MethodKey, Escapes>) =
            let summaryOf (key : MethodKey) : Escapes =
                match state.Summaries.TryFind key with
                | Some known -> known
                | None -> current.[key]

            let state, next =
                ((state, current), reachable)
                ||> List.fold (fun (state, next) key ->
                    let facts = state.Facts.[key]

                    let state, escaping =
                        ((state, seeds.[key]), facts.Calls)
                        ||> List.fold (fun (state, acc) (offset, callee) ->
                            let calleeEscapes = summaryOf callee

                            let state, types =
                                ((state, acc.Types), calleeEscapes.Types)
                                ||> Set.fold (fun (state, types) thrown ->
                                    if types.Contains thrown then
                                        state, types
                                    else
                                        match escapesAt state key offset (Some thrown) with
                                        | state, true -> state, Set.add thrown types
                                        | state, false -> state, types
                                )

                            let state, unknown =
                                if acc.Unknown || not calleeEscapes.Unknown then
                                    state, acc.Unknown
                                else
                                    escapesAt state key offset None

                            state,
                            {
                                Types = types
                                Unknown = unknown
                            }
                        )

                    state, Map.add key escaping next
                )

            if next = current then
                state, current
            else
                iterate state next

        let state, solved = iterate state seeds

        let state =
            { state with
                Summaries =
                    (state.Summaries, solved)
                    ||> Map.fold (fun acc key value -> Map.add key value acc)
            }

        state, state.Summaries.[method]

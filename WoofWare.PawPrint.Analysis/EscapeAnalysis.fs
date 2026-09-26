namespace WoofWare.PawPrint.Analysis

open System.Collections.Immutable
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
        /// What binding the tokens the body names and the types of its locals can throw: a member
        /// or type the assembly it is looked for in does not have. The JIT binds them before the
        /// body runs, so none of the body's own handlers can catch these.
        BindingFailures : Set<ThrownType>
    }

/// What a call instruction's token names.
[<RequireQualifiedAccess>]
type internal CallTarget =
    | Method of MethodKey
    | ArrayAccessor of arrayType : TypeDefn * ArrayAccessor
    | Missing
    | TypeMissing
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
/// That holds for assemblies that agree with each other. A member or type that an assembly names
/// and the loaded one it is looked for in lacks is reported, as the exception binding it throws,
/// and signals that they do not. A member that has become inaccessible to its caller, or a generic
/// instantiation that a constraint added since rejects, is not checked, and the exception it causes
/// is not reported.
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

    /// The CoreLib type of this namespace and name.
    let private corelibType (state : EscapeAnalysisState) (ns : string) (name : string) : ResolvedTypeIdentity =
        match state.Context.BaseTypes.Corelib.TryGetTopLevelTypeDef ns name with
        | Some ty -> ty.Identity
        | None -> failwith $"CoreLib declares no %s{ns}.%s{name}"

    /// The CoreLib exception type of this name, which the runtime raises by itself.
    let private corelibException (state : EscapeAnalysisState) (name : string) : ResolvedTypeIdentity =
        corelibType state "System" name

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
    /// one the analysis cannot name. A `finally` or `fault` never stops one, and a `filter` may
    /// decline, so neither counts.
    ///
    /// A clause sees a thrown object that is not an exception as `RuntimeWrappedException` if
    /// `assembly` wraps such throws, and as itself if not; an unknown one may be such an object, so
    /// only a clause catching everything it could be seen as stops it.
    let private escapesHandlers
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (regions : ExceptionRegion list)
        (offset : int)
        (thrown : ThrownType option)
        : EscapeAnalysisState * bool
        =
        let objectType = state.Context.BaseTypes.Object.Identity
        let exceptionType = state.Context.BaseTypes.Exception.Identity
        let wraps = lazy (RuntimeCompatibility.wrapsNonExceptionThrows assembly)

        // Does a clause catching `caught` stop what was thrown?
        let stops (state : EscapeAnalysisState) (caught : ResolvedTypeIdentity) : EscapeAnalysisState * bool =
            if caught = objectType then
                state, true
            else

            match thrown with
            | None -> state, caught = exceptionType && wraps.Force ()
            | Some (ThrownType.Exactly ty)
            | Some (ThrownType.SubtypeOf ty) ->

            match derivesFrom state ty exceptionType with
            | state, true -> derivesFrom state ty caught
            | state, false ->
                // What is thrown is not an exception, or, below `object` or an interface, may be
                // either; each possibility must be caught.
                let mayBeException =
                    match thrown with
                    | Some (ThrownType.SubtypeOf _) ->
                        ty = objectType
                        || (snd (definitionOf state ty)).TypeAttributes.HasFlag TypeAttributes.Interface
                    | _ -> false

                let state, nonExceptionStopped =
                    if wraps.Force () then
                        derivesFrom
                            state
                            (corelibType state "System.Runtime.CompilerServices" "RuntimeWrappedException")
                            caught
                    else
                        derivesFrom state ty caught

                state, nonExceptionStopped && (not mayBeException || caught = exceptionType)

        let rec go (state : EscapeAnalysisState) (regions : ExceptionRegion list) =
            match regions with
            | [] -> state, true
            | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, o) :: rest when
                offset >= o.TryOffset && offset < o.TryOffset + o.TryLength
                ->
                match catchType state assembly token with
                | state, Some caught ->
                    match stops state caught with
                    | state, true -> state, false
                    | state, false -> go state rest
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
            | MethodReferenceTarget.ArrayMethod (arrayType, accessor) ->
                state, CallTarget.ArrayAccessor (arrayType, accessor)
            | MethodReferenceTarget.Missing -> state, CallTarget.Missing
            | MethodReferenceTarget.ParentTypeMissing _ -> state, CallTarget.TypeMissing
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

    /// A static virtual method, which only a `constrained.` call reaches, and which the type that
    /// prefix names decides.
    let private isStaticVirtual (state : EscapeAnalysisState) (key : MethodKey) : bool =
        let _, method = methodOf state key
        method.IsStatic && method.IsVirtual

    /// The runtime's own exceptions from one of the methods it supplies on an array type: those of
    /// the instruction each stands in for, `ldelem`, `stelem.ref`, `ldelema` and `newarr`, and for a
    /// multidimensional array's constructor taking lower bounds, `ArgumentOutOfRangeException` for
    /// bounds whose upper end overflows.
    let private arrayAccessorRaises
        (state : EscapeAnalysisState)
        (arrayType : TypeDefn)
        (accessor : ArrayAccessor)
        : ThrownType list
        =
        let faults =
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

        let lowerBounds =
            match arrayType, accessor with
            | TypeDefn.Array (_, rank), ArrayAccessor.Constructor arity when arity = 2 * rank ->
                [ ThrownType.Exactly (corelibException state "ArgumentOutOfRangeException") ]
            | _ -> []

        (faults |> List.map (fun fault -> ThrownType.Exactly (faultType state fault)))
        @ lowerBounds

    /// Does every type reference in a spelling name a type? One that does not makes binding the
    /// token that spells it throw `TypeLoadException`. Custom modifiers and function pointer
    /// signatures are not bound.
    let rec private spellingBinds
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * bool
        =
        match spelling with
        | TypeDefn.FromReference (typeRef, _) ->
            match resolveTypeRef state assembly typeRef with
            | state, Some _ -> state, true
            | state, None -> state, false
        | TypeDefn.GenericInstantiation (root, arguments) ->
            ((state, true), Seq.append [ root ] arguments)
            ||> Seq.fold (fun (state, soFar) spelling ->
                if soFar then
                    spellingBinds state assembly spelling
                else
                    state, false
            )
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> spellingBinds state assembly element
        | TypeDefn.Modified modified -> spellingBinds state assembly modified.Unmodified
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> state, true

    /// Bind the metadata token an instruction names, as the JIT does before the body runs: what it
    /// names when that is a method, and what binding it can throw.
    let rec private bindToken
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * CallTarget option * ThrownType list
        =
        let typeLoad () =
            [ ThrownType.Exactly (corelibException state "TypeLoadException") ]

        match token with
        | MetadataToken.MethodDef _ ->
            let state, target = callTarget state assembly token
            state, Some target, []
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]
            let state, target, failures = bindToken state assembly spec.Method

            let state, argumentsBind =
                ((state, true), spec.Signature)
                ||> Seq.fold (fun (state, soFar) argument ->
                    if soFar then
                        spellingBinds state assembly argument
                    else
                        state, false
                )

            state, target, (if argumentsBind then failures else typeLoad () @ failures)
        | MetadataToken.MemberReference handle ->
            // Resolving the member reads only the parent's definition, but binding the reference
            // loads the parent type itself, with every type argument it spells.
            let state, parentBinds =
                match assembly.Members.[handle].Parent with
                | MetadataToken.TypeSpecification parent ->
                    spellingBinds state assembly assembly.TypeSpecs.[parent].Signature
                | _ -> state, true

            let state, target, failures =
                match assembly.Members.[handle].Signature with
                | MemberSignature.Method _ ->
                    match callTarget state assembly token with
                    | state, CallTarget.Missing ->
                        state,
                        Some CallTarget.Missing,
                        [ ThrownType.Exactly (corelibException state "MissingMethodException") ]
                    | state, CallTarget.TypeMissing -> state, Some CallTarget.TypeMissing, typeLoad ()
                    | state, target -> state, Some target, []
                | MemberSignature.Field _ ->
                    let assemblies, target =
                        FieldReferenceResolution.resolve
                            state.LoggerFactory
                            state.RuntimeDirs
                            state.Context.BaseTypes
                            state.Context.LoadedAssemblies
                            assembly
                            handle

                    let state = withAssemblies state assemblies

                    match target with
                    | FieldReferenceTarget.Missing ->
                        state, None, [ ThrownType.Exactly (corelibException state "MissingFieldException") ]
                    | FieldReferenceTarget.ParentTypeMissing _ -> state, None, typeLoad ()
                    | FieldReferenceTarget.Defined _
                    | FieldReferenceTarget.DependsOnInstantiation -> state, None, []

            state, target, (if parentBinds then failures else typeLoad () @ failures)
        | MetadataToken.TypeReference handle ->
            match resolveTypeRef state assembly assembly.TypeRefs.[handle] with
            | state, Some _ -> state, None, []
            | state, None -> state, None, typeLoad ()
        | MetadataToken.TypeSpecification handle ->
            match spellingBinds state assembly assembly.TypeSpecs.[handle].Signature with
            | state, true -> state, None, []
            | state, false -> state, None, typeLoad ()
        | _ -> state, None, []

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
            BindingFailures = Set.empty
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
        // lets the initializing thread straight through (ECMA-335 I.8.9.5). Only for a non-generic
        // type, though: each instantiation of a generic one has its own statics and initializer, and
        // a definition cannot tell `G<int>` from the `G<string>` being initialized.
        let initializing =
            if
                method.Name = ".cctor"
                && method.IsStatic
                && method.RequiredDeclaringType.Generics.IsEmpty
            then
                Some method.RequiredDeclaringType.Identity
            else
                None

        // Whether a `TypeInitializationException` from this instruction is impossible: the type it
        // touches has no initializer to fail, or is the one this body is initializing. A
        // `callvirt` names where dispatch starts rather than where it lands, so it is never pruned.
        let typeInitializationImpossible
            (state : EscapeAnalysisState)
            (op : IlOp)
            (methodTarget : CallTarget option)
            : EscapeAnalysisState * bool
            =
            let owner (state : EscapeAnalysisState) : EscapeAnalysisState * ResolvedTypeIdentity option =
                match op with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp),
                                           _) ->
                    match methodTarget with
                    | Some (CallTarget.Method callee) -> state, Some (declaringTypeOf state callee)
                    | _ -> state, None
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
                calls : (int * MethodKey) list,
                bindingFailures : Set<ThrownType>
            )
            (index : int)
            =
            let op, offset = ops.[index]

            // 0. Bind the token the instruction names, which the JIT does before the body runs.
            let state, methodTarget, bindingFailures =
                match op with
                | IlOp.UnaryMetadataToken (_, MetadataOperand.FromMetadata token) ->
                    let state, target, failures = bindToken state assembly token.Token
                    state, target, Set.union bindingFailures (Set.ofList failures)
                | _ -> state, None, bindingFailures

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
                                    typeInitializationImpossible state op methodTarget
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
            let state, raises, opaque, calls =
                match op with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, _) ->
                    state, raises, (offset, Opacity.IndirectCall) :: opaque, calls
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp) as call,
                                           operand) ->
                    match operand, methodTarget with
                    | MetadataOperand.FromDynamicScope _, _ ->
                        state, raises, (offset, Opacity.IndirectCall) :: opaque, calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.Method callee) ->
                        // A static virtual is dispatched on the type a `constrained.` prefix names,
                        // which is how the only legal call to one is written.
                        if
                            (call = UnaryMetadataTokenIlOp.Callvirt && isOverridable state callee)
                            || isStaticVirtual state callee
                        then
                            state, raises, (offset, Opacity.VirtualCall) :: opaque, calls
                        else
                            state, raises, opaque, (offset, callee) :: calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.ArrayAccessor (arrayType, accessor)) ->
                        let raised =
                            arrayAccessorRaises state arrayType accessor
                            |> List.map (fun thrown -> offset, thrown)

                        state, raised @ raises, opaque, calls
                    | MetadataOperand.FromMetadata _, Some CallTarget.DependsOnInstantiation ->
                        state, raises, (offset, Opacity.DependsOnInstantiation) :: opaque, calls
                    // Binding the token fails, which step 0 recorded; there is nothing to call.
                    | MetadataOperand.FromMetadata _, Some CallTarget.Missing
                    | MetadataOperand.FromMetadata _, Some CallTarget.TypeMissing -> state, raises, opaque, calls
                    | MetadataOperand.FromMetadata token, None ->
                        failwith
                            $"A call in %s{assembly.DefinitionFullName} names %O{token.Token}, which is not a method"
                | _ -> state, raises, opaque, calls

            state, raises, opaque, calls, bindingFailures

        // The JIT loads the type of every local before the body runs, as it binds every token.
        let state, localFailures =
            ((state, Set.empty), Option.defaultValue ImmutableArray.Empty body.LocalVars)
            ||> Seq.fold (fun (state, failures) local ->
                match spellingBinds state assembly local with
                | state, true -> state, failures
                | state, false ->
                    state, Set.add (ThrownType.Exactly (corelibException state "TypeLoadException")) failures
            )

        let state, raises, opaque, calls, bindingFailures =
            ((state, [], [], [], localFailures), [ 0 .. ops.Length - 1 ])
            ||> List.fold folder

        state,
        {
            Raises = List.rev raises
            Opaque = List.rev opaque
            Calls = List.rev calls
            Regions = List.ofSeq body.ExceptionRegions
            BindingFailures = bindingFailures
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

            // Binding failures happen before the body runs, so none of its handlers apply.
            let state, types =
                ((state, facts.BindingFailures), facts.Raises)
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

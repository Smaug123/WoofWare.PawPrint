namespace WoofWare.PawPrint.Analysis

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

/// Why part of a method's behaviour is hidden from the analysis. Each is a place where an exception
/// the analysis cannot name may arise, which is what makes <c>Escapes.Unknown</c> true.
[<RequireQualifiedAccess>]
type Opacity =
    /// A <c>callvirt</c> to a method that may be overridden: the method named may not be the one
    /// that runs. That includes a <c>constrained.</c> call whose type does not decide it: an
    /// instance method's on a class that may be derived from, or a type variable of a definition
    /// analysed for every instantiation at once.
    | VirtualCall
    /// <c>calli</c>, or a call through a dynamic method's scope: no metadata names the target.
    | IndirectCall
    /// <c>ldvirtftn</c> of an interface method, which on an object implementing
    /// <c>IDynamicInterfaceCastable</c> asks that object's <c>GetInterfaceImplementation</c>.
    | InterfaceMethodPointer
    /// The method is <c>InternalCall</c>, <c>PInvoke</c> or runtime-provided: its behaviour is not IL.
    | NativeBody
    /// The method is abstract.
    | AbstractBody
    /// Something CoreCLR runs for an <c>[Intrinsic]</c> in place of its IL that the analysis does not
    /// model: the JIT's expansion of the method's call to itself where the JIT's tables do not say
    /// what the instruction raises (<c>HardwareInstruction.contract</c>) or the expansion is not
    /// recognised, or a body the VM substitutes that is not transcribed.
    | IntrinsicExpansion
    /// A MemberRef whose target turns on how a type variable of this method is instantiated.
    | DependsOnInstantiation
    /// A <c>throw</c> whose operand's type the analysis does not know.
    | UntypedThrow
    /// A <c>rethrow</c>: what it raises is what the enclosing handler caught.
    | Rethrow
    /// An instruction <c>OpcodeFaults</c> declines to classify.
    | UnmodelledOpcode

/// Something that happens outside a method's body, so that none of its own handlers can catch it.
[<RequireQualifiedAccess>]
type internal OutsideBodyFact =
    | Raises of ThrownType
    /// What happens turns on something the analysis cannot see, such as how a type variable of the
    /// method is instantiated when the JIT binds a member of it.
    | Opaque of Opacity

/// How a call's token instantiates the method it names, as the calling body spells it.
[<RequireQualifiedAccess>]
type internal CalleeSpelling =
    /// The callee has no type variables, so every call reaches the same method.
    | Fixed
    /// The token names a generic definition rather than an instantiation of it: a MethodDef of a
    /// generic type's method, or a MemberRef whose parent is a generic type named without
    /// arguments. Nothing says which instantiation runs.
    | Typical
    /// The token, of the calling body's assembly, whose type arguments, read against the calling
    /// body's instantiation, instantiate the callee: a MethodSpec, or a MemberRef whose parent is a
    /// TypeSpec or a non-generic type with a generic ancestor.
    | Spelled of MetadataToken

/// A method a body calls, and how the call instantiates it.
type internal Callee =
    {
        Callee : MethodKey
        Spelling : CalleeSpelling
    }

/// A call instruction, as far as the body says.
[<RequireQualifiedAccess>]
type internal CallSite =
    /// A call of this method.
    | Direct of Callee
    /// A call of a static virtual method, or a `callvirt` of a method a derived type may override,
    /// which a `constrained.` prefix naming this type, a token of the calling body's assembly,
    /// directs: the type decides what runs.
    | Constrained of constrainedType : MetadataToken * Callee

/// What one method body does by itself: the exceptions it raises, the places it cannot see
/// through, and the methods it calls, each at the IL offset where it happens, so that the body's
/// own exception clauses can be applied to all three alike.
type internal LocalFacts =
    {
        Raises : (int * ThrownType) list
        Opaque : (int * Opacity) list
        Calls : (int * CallSite) list
        /// Each `rethrow`, with the `catch` or `filter` clause whose handler it is in: it re-raises
        /// what that clause caught.
        Rethrows : (int * ExceptionRegion) list
        Regions : ExceptionRegion list
        /// What happens outside the body, so that none of its own handlers can catch it: what
        /// binding the tokens the body names and the types of its locals and `catch` clauses
        /// throws before it runs (a member or type the assembly it is looked for in does not have,
        /// a module initializer that fails, or a member of a type variable, which turns on the
        /// instantiation), and what taking and releasing a synchronized method's monitor throws.
        OutsideBody : Set<OutsideBodyFact>
    }

/// What a `throw` is handed, as far as the instruction before it says.
[<RequireQualifiedAccess>]
type internal ThrowOperand =
    | Object of ThrownType
    | Null
    | Untyped

/// What a `rethrow` re-raises of one thing its clause's protected block raised.
[<RequireQualifiedAccess>]
type internal Rethrown =
    /// The clause cannot catch it.
    | Nothing
    | Thrown of ThrownType
    /// Something the analysis cannot name.
    | Unknown

/// What a call instruction's token names.
[<RequireQualifiedAccess>]
type internal CallTarget =
    | Method of Callee
    | ArrayAccessor of arrayType : TypeDefn * ArrayAccessor
    | Missing
    | TypeMissing
    | AssemblyMissing
    | DependsOnInstantiation

/// What CoreCLR runs when a method is called (`IntrinsicBody.classify`).
[<RequireQualifiedAccess>]
type internal Runs =
    /// IL: the method's own, or what CoreCLR's VM substitutes for it. `selfCall` is the JIT's
    /// expansion of the IL's call to the method itself, where it has one; that call is not a call.
    | Il of MethodInstructions<TypeDefn> * selfCall : JitExpansion option
    /// One of the runtime's own operations, the whole of the method.
    | Primitive of IntrinsicPrimitive
    /// Native code whose behaviour `NativeMethod` describes, the whole of the method.
    | Native of NativeMethod
    /// Nothing the analysis can see into.
    | Opaque of Opacity

/// Which instantiation of a method definition a summary describes.
[<RequireQualifiedAccess>]
type internal Instantiation =
    /// Every instantiation at once: the definition's type variables stand for any types, so a call
    /// whose target turns on one of them is opaque.
    | Open
    /// One instantiation: the declaring type's arguments, then the method's own.
    | Closed of typeArguments : ConcreteTypeHandle list * methodArguments : ConcreteTypeHandle list

/// One instantiation of a method definition, which a summary is computed for.
type internal MethodInstance =
    {
        Definition : MethodKey
        Arguments : Instantiation
    }

/// What a `constrained.` call does in one instance of the method making it.
[<RequireQualifiedAccess>]
type internal ConstrainedOutcome =
    /// It calls this instance.
    | Reaches of MethodInstance
    /// It throws this instead of calling anything.
    | Raises of ThrownType
    /// The type the prefix names does not decide what runs.
    | Undecided

/// What one instance of a method calls, each at the IL offset of the call: the instances it
/// reaches, what calls raise instead of reaching anything, and the calls whose target the instance
/// does not decide.
type internal InstanceCalls =
    {
        Callees : (int * MethodInstance) list
        Raises : (int * ThrownType) list
        Undecided : (int * Opacity) list
    }

/// An escape analysis in progress: the assemblies loaded so far and every answer computed so far.
/// Immutable; each query returns the state to ask the next one of.
type EscapeAnalysisState =
    private
        {
            LoggerFactory : ILoggerFactory
            RuntimeDirs : string list
            /// The architecture the JIT compiles for, and the CPU it compiles for: what a capability
            /// query answers, and whether a hardware instruction is emitted or throws.
            Target : JitTarget
            Profile : HardwareIntrinsicsProfile
            /// The assemblies loaded so far, the concrete types instantiated so far, and what the type
            /// system has memoised about them.
            TypeSystem : TypeSystemState
            BaseTypes : BaseClassTypes<DumpedAssembly>
            Facts : Map<MethodKey, LocalFacts>
            /// What each instance summarised so far calls.
            InstanceCalls : Map<MethodInstance, InstanceCalls>
            /// Whether dispatch on a receiver of each type definition can be resolved, as far as it
            /// has been asked (`dispatchBinds`).
            DispatchBinds : Map<ResolvedTypeIdentity, bool>
            Summaries : Map<MethodInstance, Escapes>
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
/// An interface cast or an array store calls <c>IDynamicInterfaceCastable.IsInterfaceImplemented</c>
/// on an object whose class implements it. That is assumed to throw nothing but the
/// <c>InvalidCastException</c> its documentation asks for.
///
/// That holds for assemblies that agree with each other. A member or type that a body names and
/// the loaded assembly it is looked for in lacks is reported, as the exception binding it throws,
/// and signals that they do not. What else such a disagreement can break is not checked, and the
/// exception it causes is not reported: a member that has become inaccessible to its caller, a
/// generic instantiation that a constraint added since rejects, or a named type whose own base
/// type, interfaces or fields are gone.
///
/// A generic method's IL is read once, since every instantiation runs the same IL, but its summary
/// is computed for each closed instantiation a call reaches. An instantiation decides what a
/// <c>constrained.</c> call on one of the method's type variables runs when it makes that a value
/// type or a sealed class, or the method called is a static virtual, which no derived class's
/// instance can receive. Asked about by itself, a generic definition stands for every
/// instantiation at once, so such a call is opaque in it. An instantiation whose type arguments nest
/// more than eight deep is analysed as its definition instead, so that a method calling itself at
/// ever deeper instantiations reaches finitely many.
/// </remarks>
[<RequireQualifiedAccess>]
module EscapeAnalysis =

    /// Begin an analysis over the assemblies `context` has loaded, loading any others it needs from
    /// `runtimeDirs`, of what they do when CoreCLR's JIT compiles them for `target` on a CPU
    /// `profile` describes. `profile` must not mark as supported a class of another target's
    /// instruction sets: CoreLib gives those bodies that throw, whatever the CPU.
    let create
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (target : JitTarget)
        (profile : HardwareIntrinsicsProfile)
        (context : TypeConcretization.ConcretizationContext<DumpedAssembly>)
        : EscapeAnalysisState
        =
        let foreign =
            JitTarget.all
            |> List.filter (fun other -> other <> target)
            |> List.map JitTarget.instructionSetNamespace
            |> Set.ofList

        for intrinsicClass in Set.union profile.IsSupported profile.IsHardwareAccelerated do
            if foreign.Contains intrinsicClass.Namespace then
                invalidArg
                    (nameof profile)
                    $"The profile marks %O{intrinsicClass} supported, but the JIT compiles for %A{target}"

        {
            LoggerFactory = loggerFactory
            RuntimeDirs = List.ofSeq runtimeDirs
            Target = target
            Profile = profile
            TypeSystem =
                { TypeSystemState.Empty with
                    _LoadedAssemblies = context.LoadedAssemblies
                    ConcreteTypes = context.ConcreteTypes
                }
            BaseTypes = context.BaseTypes
            Facts = Map.empty
            InstanceCalls = Map.empty
            DispatchBinds = Map.empty
            Summaries = Map.empty
            Bases = Map.empty
        }

    let private assemblyOf (state : EscapeAnalysisState) (fullName : string) : DumpedAssembly =
        state.TypeSystem._LoadedAssemblies.ByDefinitionName fullName

    let private withAssemblies (state : EscapeAnalysisState) (assemblies : LoadedAssemblies) : EscapeAnalysisState =
        { state with
            TypeSystem =
                { state.TypeSystem with
                    _LoadedAssemblies = assemblies
                }
        }

    /// The CoreLib type an `OpcodeFault` names.
    let private faultType (state : EscapeAnalysisState) (fault : OpcodeFault) : ResolvedTypeIdentity =
        let qualified = OpcodeFault.typeName fault
        let dot = qualified.LastIndexOf '.'
        let ns, name = qualified.Substring (0, dot), qualified.Substring (dot + 1)

        match state.BaseTypes.Corelib.TryGetTopLevelTypeDef ns name with
        | Some ty -> ty.Identity
        | None -> failwith $"CoreLib declares no %s{qualified}, which OpcodeFaults names"

    /// The CoreLib type of this namespace and name.
    let private corelibType (state : EscapeAnalysisState) (ns : string) (name : string) : ResolvedTypeIdentity =
        match state.BaseTypes.Corelib.TryGetTopLevelTypeDef ns name with
        | Some ty -> ty.Identity
        | None -> failwith $"CoreLib declares no %s{ns}.%s{name}"

    /// The CoreLib exception type of this name, which the runtime raises by itself.
    let private corelibException (state : EscapeAnalysisState) (name : string) : ResolvedTypeIdentity =
        corelibType state "System" name

    /// What binding a type reference in `assembly` finds.
    let private bindTypeRef
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (typeRef : TypeRef)
        : EscapeAnalysisState * TypeReferenceIdentity
        =
        let assemblies, identity =
            TypeResolution.tryResolveTypeRefIdentity
                state.LoggerFactory
                state.RuntimeDirs
                assembly
                typeRef
                state.TypeSystem._LoadedAssemblies

        withAssemblies state assemblies, identity

    /// The definition a type reference in `assembly` names, if it binds.
    let private resolveTypeRef
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (typeRef : TypeRef)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match bindTypeRef state assembly typeRef with
        | state, TypeReferenceIdentity.Resolved identity -> state, Some identity
        | state, TypeReferenceIdentity.TypeAbsent _
        | state, TypeReferenceIdentity.AssemblyUnavailable _ -> state, None

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
            state, Some (BaseClassTypes.ofPrimitive state.BaseTypes primitive).Identity
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
        let rec go (state : EscapeAnalysisState) (current : ResolvedTypeIdentity) (seen : Set<ResolvedTypeIdentity>) =
            if current = ancestor then
                state, true
            elif seen.Contains current then
                failwith $"The base chain of %O{ty} reaches %O{current} twice"
            else
                match baseOf state current with
                | state, Some parent -> go state parent (seen.Add current)
                | state, None -> state, false

        go state ty Set.empty

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

    let private isInterface (state : EscapeAnalysisState) (ty : ResolvedTypeIdentity) : bool =
        (snd (definitionOf state ty)).TypeAttributes.HasFlag TypeAttributes.Interface

    let private wrapperType (state : EscapeAnalysisState) : ResolvedTypeIdentity =
        corelibType state "System.Runtime.CompilerServices" "RuntimeWrappedException"

    /// Does a `catch` clause for `caught`, in a body of `assembly`, certainly stop what was thrown?
    /// `thrown` is `None` for one the analysis cannot name.
    ///
    /// A clause sees a thrown object that is not an exception as `RuntimeWrappedException` if
    /// `assembly` wraps such throws, and as itself if not, in which case it also sees any thrown
    /// `RuntimeWrappedException` as the object that wraps; an unknown one may be either, so only a
    /// clause catching everything it could be seen as stops it.
    let private catchStops
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caught : ResolvedTypeIdentity)
        (thrown : ThrownType option)
        : EscapeAnalysisState * bool
        =
        let objectType = state.BaseTypes.Object.Identity
        let exceptionType = state.BaseTypes.Exception.Identity
        let wraps = lazy (RuntimeCompatibility.wrapsNonExceptionThrows assembly)

        if caught = objectType then
            state, true
        else

        match thrown with
        | None -> state, caught = exceptionType && wraps.Force ()
        | Some (ThrownType.Exactly ty)
        | Some (ThrownType.SubtypeOf ty) ->

        // A clause in an assembly that does not wrap sees any `RuntimeWrappedException`
        // unwrapped, even one thrown explicitly, and what it wraps may be anything.
        let state, mayBeUnwrapped =
            if wraps.Force () then
                state, false
            else
                let wrapper = wrapperType state

                match thrown with
                | Some (ThrownType.SubtypeOf _) ->
                    match derivesFrom state wrapper ty with
                    | state, true -> state, true
                    | state, false -> state, isInterface state ty
                | _ -> state, ty = wrapper

        if mayBeUnwrapped then
            state, false
        else

        match derivesFrom state ty exceptionType with
        | state, true -> derivesFrom state ty caught
        | state, false ->
            // What is thrown is not an exception, or, below `object` or an interface, may be
            // either; each possibility must be caught.
            let mayBeException =
                match thrown with
                | Some (ThrownType.SubtypeOf _) -> ty = objectType || isInterface state ty
                | _ -> false

            let state, nonExceptionStopped =
                if wraps.Force () then
                    derivesFrom state (wrapperType state) caught
                else
                    derivesFrom state ty caught

            state, nonExceptionStopped && (not mayBeException || caught = exceptionType)

    /// Does an exception raised at `offset` get past `regions`, a body's clauses in the order the
    /// runtime tries them? `thrown` is `None` for one the analysis cannot name. A `finally` or
    /// `fault` never stops one, and a `filter` may decline, so neither counts.
    let private escapesHandlers
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (regions : ExceptionRegion list)
        (offset : int)
        (thrown : ThrownType option)
        : EscapeAnalysisState * bool
        =
        let rec go (state : EscapeAnalysisState) (regions : ExceptionRegion list) =
            match regions with
            | [] -> state, true
            | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, o) :: rest when
                offset >= o.TryOffset && offset < o.TryOffset + o.TryLength
                ->
                match catchType state assembly token with
                | state, Some caught ->
                    match catchStops state assembly caught thrown with
                    | state, true -> state, false
                    | state, false -> go state rest
                | state, None -> go state rest
            | _ :: rest -> go state rest

        go state regions

    /// What a `rethrow` in the handler of `clause`, a clause of a body in `assembly`, re-raises of
    /// `thrown`, which reached the clause from its protected block. It re-raises what the clause
    /// caught, as it was thrown.
    let private rethrownBy
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (clause : ExceptionRegion)
        (thrown : ThrownType option)
        : EscapeAnalysisState * Rethrown
        =
        let asThrown =
            match thrown with
            | Some thrown -> Rethrown.Thrown thrown
            | None -> Rethrown.Unknown

        match clause with
        | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, _) ->
            match catchType state assembly token with
            | state, None -> state, asThrown
            | state, Some caught ->

            match catchStops state assembly caught thrown with
            | state, true -> state, asThrown
            | state, false ->

            let exceptionType = state.BaseTypes.Exception.Identity
            let wrapper = wrapperType state
            let wraps = RuntimeCompatibility.wrapsNonExceptionThrows assembly

            // `catchStops` is exact for an object of a known class, except a
            // `RuntimeWrappedException` that a clause in an assembly that does not wrap sees as
            // what it wraps: CoreCLR matches a clause's type against the base chain of what the
            // clause sees, and nothing else, so a clause for an interface catches nothing
            // (`ShouldTypedClauseCatchThisException`, ExceptionHandling.cs).
            let state, never =
                match thrown with
                | _ when isInterface state caught -> state, true
                | Some (ThrownType.Exactly ty) -> state, wraps || ty <> wrapper
                | Some (ThrownType.SubtypeOf ty) when not (isInterface state ty) ->
                    match derivesFrom state ty exceptionType with
                    | state, false -> state, false
                    | state, true ->

                    match derivesFrom state caught ty with
                    | state, true -> state, false
                    | state, false ->

                    match derivesFrom state ty caught with
                    | state, true -> state, false
                    | state, false ->
                        if wraps then
                            state, true
                        else
                            match derivesFrom state wrapper ty with
                            | state, isWrapperAncestor -> state, not isWrapperAncestor
                | _ -> state, false

            if never then
                state, Rethrown.Nothing
            else

            // In an assembly that wraps, a clause for a type other than an ancestor of
            // `RuntimeWrappedException` sees only exceptions, each as itself, so what it catches
            // is one of that type.
            let state, narrowable =
                if not wraps then
                    state, false
                else
                    match derivesFrom state wrapper caught with
                    | state, isWrapperAncestor -> state, not isWrapperAncestor

            match thrown with
            | None
            | Some (ThrownType.SubtypeOf _) when narrowable -> state, Rethrown.Thrown (ThrownType.SubtypeOf caught)
            | _ -> state, asThrown
        // A `filter` handler holds whatever its filter accepted.
        | _ -> state, asThrown

    /// Whether a substitution mentions a type variable, which only a generic definition named
    /// without arguments leaves standing.
    let rec private mentionsVariable (arguments : ImmutableArray<TypeConcretization.SubstitutionArgument>) : bool =
        arguments
        |> Seq.exists (fun argument ->
            match argument with
            | TypeConcretization.SubstitutionArgument.Formal _ -> true
            | TypeConcretization.SubstitutionArgument.Closed _ -> false
            | TypeConcretization.SubstitutionArgument.Spelled (_, _, context) -> mentionsVariable context
        )

    /// What a call instruction's metadata token names, and whether it names the generic definition
    /// of the callee's declaring type rather than an instantiation of it.
    let rec private callTargetOf
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * CallTarget * bool
        =
        match token with
        | MetadataToken.MethodDef handle ->
            let method = assembly.Methods.[handle]
            let typical = not method.DeclaringTypeGenerics.IsEmpty

            let spelling =
                if typical || not method.Generics.IsEmpty then
                    CalleeSpelling.Typical
                else
                    CalleeSpelling.Fixed

            let callee =
                {
                    Callee = MethodKey.make assembly handle
                    Spelling = spelling
                }

            state, CallTarget.Method callee, typical
        | MetadataToken.MethodSpecification handle ->
            match callTargetOf state assembly assembly.MethodSpecs.[handle].Method with
            | state, CallTarget.Method callee, typical ->
                let spelling =
                    if typical then
                        CalleeSpelling.Typical
                    else
                        CalleeSpelling.Spelled token

                state,
                CallTarget.Method
                    { callee with
                        Spelling = spelling
                    },
                typical
            | unresolved -> unresolved
        | MetadataToken.MemberReference handle ->
            let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                {
                    ConcreteTypes = state.TypeSystem.ConcreteTypes
                    LoadedAssemblies = state.TypeSystem._LoadedAssemblies
                    BaseTypes = state.BaseTypes
                }

            let ctx, target =
                MethodReferenceResolution.resolve state.LoggerFactory state.RuntimeDirs ctx assembly handle

            let state =
                { state with
                    TypeSystem =
                        { state.TypeSystem with
                            _LoadedAssemblies = ctx.LoadedAssemblies
                            ConcreteTypes = ctx.ConcreteTypes
                        }
                }

            match target with
            | MethodReferenceTarget.Defined (declaring, method, declaringTypeArguments) ->
                // A TypeSpec parent spells the declaring type's arguments; any other parent spells
                // none, so a type variable left in them is the parent's own.
                let typical =
                    match assembly.Members.[handle].Parent with
                    | MetadataToken.TypeSpecification _ -> false
                    | _ -> mentionsVariable declaringTypeArguments.Arguments

                let definition = declaring.Methods.[method]

                let spelling =
                    // A generic method named without a MethodSpec has no arguments to run with.
                    if typical || not definition.Generics.IsEmpty then
                        CalleeSpelling.Typical
                    elif definition.DeclaringTypeGenerics.IsEmpty then
                        CalleeSpelling.Fixed
                    else
                        CalleeSpelling.Spelled token

                let callee =
                    {
                        Callee = MethodKey.make declaring method
                        Spelling = spelling
                    }

                state, CallTarget.Method callee, typical
            | MethodReferenceTarget.ArrayMethod (arrayType, accessor) ->
                state, CallTarget.ArrayAccessor (arrayType, accessor), false
            | MethodReferenceTarget.Missing -> state, CallTarget.Missing, false
            | MethodReferenceTarget.ParentTypeMissing _ -> state, CallTarget.TypeMissing, false
            | MethodReferenceTarget.ParentAssemblyUnavailable _ -> state, CallTarget.AssemblyMissing, false
            | MethodReferenceTarget.DependsOnInstantiation -> state, CallTarget.DependsOnInstantiation, false
        | other -> failwith $"A call in %s{assembly.DefinitionFullName} names %O{other}, which is not a method"

    /// What a call instruction's metadata token names.
    let private callTarget
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * CallTarget
        =
        let state, target, _ = callTargetOf state assembly token
        state, target

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

    /// Why binding a type reference fails, which decides what the binding throws.
    [<RequireQualifiedAccess>]
    type private BindFailure =
        /// Its assembly binds and declares no such type: `TypeLoadException`.
        | TypeAbsent
        /// No assembly is found for it: `FileNotFoundException`.
        | AssemblyUnavailable

    /// The exception binding fails with.
    let private bindFailureRaises (state : EscapeAnalysisState) (failure : BindFailure) : OutsideBodyFact =
        match failure with
        | BindFailure.TypeAbsent ->
            OutsideBodyFact.Raises (ThrownType.Exactly (corelibException state "TypeLoadException"))
        | BindFailure.AssemblyUnavailable ->
            OutsideBodyFact.Raises (ThrownType.Exactly (corelibType state "System.IO" "FileNotFoundException"))

    /// The first type reference in a spelling that fails to bind, and why, which makes binding the
    /// token that spells it throw; `None` if all of them bind. Custom modifiers and function pointer
    /// signatures are not bound.
    let rec private spellingBindFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option
        =
        match spelling with
        | TypeDefn.FromReference (typeRef, _) ->
            match bindTypeRef state assembly typeRef with
            | state, TypeReferenceIdentity.Resolved _ -> state, None
            | state, TypeReferenceIdentity.TypeAbsent _ -> state, Some BindFailure.TypeAbsent
            | state, TypeReferenceIdentity.AssemblyUnavailable _ -> state, Some BindFailure.AssemblyUnavailable
        | TypeDefn.GenericInstantiation (root, arguments) ->
            firstBindFailure state assembly (Seq.append [ root ] arguments)
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> spellingBindFailure state assembly element
        | TypeDefn.Modified modified -> spellingBindFailure state assembly modified.Unmodified
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> state, None

    /// The first type reference in `spellings`, in order, that fails to bind, and why.
    and private firstBindFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spellings : TypeDefn seq)
        : EscapeAnalysisState * BindFailure option
        =
        ((state, None), spellings)
        ||> Seq.fold (fun (state, failure) spelling ->
            match failure with
            | Some _ -> state, failure
            | None -> spellingBindFailure state assembly spelling
        )

    /// The types a method signature spells: its return type, if any, and its parameters'.
    let private signatureTypes (signature : TypeMethodSignature<TypeDefn>) : TypeDefn list =
        match signature.ReturnType with
        | MethodReturnType.Void -> signature.ParameterTypes
        | MethodReturnType.Returns ty -> ty :: signature.ParameterTypes

    /// Does every type reference that instantiating a spelling reads name a type? As
    /// `spellingBindFailure`, and through a function pointer's signature as well, whose types
    /// TypeSystem instantiates although the JIT binds none of them.
    let rec private concretizable
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * bool
        =
        let all (state : EscapeAnalysisState) (spellings : TypeDefn seq) =
            ((state, true), spellings)
            ||> Seq.fold (fun (state, soFar) spelling ->
                if soFar then
                    concretizable state assembly spelling
                else
                    state, false
            )

        match spelling with
        | TypeDefn.FunctionPointer signature -> all state (signatureTypes signature)
        | TypeDefn.GenericInstantiation (root, arguments) -> all state (Seq.append [ root ] arguments)
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> concretizable state assembly element
        | TypeDefn.Modified modified -> concretizable state assembly modified.Unmodified
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void ->
            let state, failure = spellingBindFailure state assembly spelling
            state, failure.IsNone

    /// The type definitions a spelling names that resolve, arguments included.
    let rec private namedIdentities
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * ResolvedTypeIdentity list
        =
        match spelling with
        | TypeDefn.FromReference (typeRef, _) ->
            match resolveTypeRef state assembly typeRef with
            | state, Some identity -> state, [ identity ]
            | state, None -> state, []
        | TypeDefn.FromDefinition (identity, _) -> state, [ identity ]
        | TypeDefn.GenericInstantiation (root, arguments) ->
            ((state, []), Seq.append [ root ] arguments)
            ||> Seq.fold (fun (state, acc) spelling ->
                let state, named = namedIdentities state assembly spelling
                state, named @ acc
            )
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> namedIdentities state assembly element
        | TypeDefn.Modified modified -> namedIdentities state assembly modified.Unmodified
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> state, []

    /// Does this assembly have a module initializer, the type initializer of `<Module>`?
    let private hasModuleInitializer (state : EscapeAnalysisState) (assemblyFullName : string) : bool =
        let assembly = assemblyOf state assemblyFullName

        match assembly.TypeDefs.TryGetValue (MetadataTokens.TypeDefinitionHandle 1) with
        | true, globalType when globalType.Name = "<Module>" ->
            globalType.Methods |> List.exists (fun m -> m.Name = ".cctor" && m.IsStatic)
        | _ -> false

    /// Whether binding a token with this opcode activates the modules of what it names, running
    /// each one's module initializer first if it has not run: CoreCLR's `CEEInfo::resolveToken`
    /// does for a method, a static field, and the type of a `box`, `constrained.` or `ldtoken`. A
    /// field token counts whatever the opcode, since `ldfld` may name a static field.
    let private activatesModules (op : UnaryMetadataTokenIlOp) : bool =
        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Newobj
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Jmp
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Ldtoken -> true
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Ldelema
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelem
        | UnaryMetadataTokenIlOp.Initobj
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> false

    /// The types whose modules binding `token` activates, as `CEEInfo::EnsureActive` walks them:
    /// the type it names, or the type declaring what it names, with every type argument spelled,
    /// and all their base types.
    let private activatedTypes
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * ResolvedTypeIdentity list
        =
        // A MemberRef's parent stands for the type declaring its target, which is the parent or
        // one of its base types.
        let rec spelled (state : EscapeAnalysisState) (token : MetadataToken) =
            match token with
            | MetadataToken.TypeDefinition handle -> state, [ assembly.TypeDefs.[handle].Identity ]
            | MetadataToken.MethodDef handle -> state, [ assembly.Methods.[handle].RequiredDeclaringType.Identity ]
            | MetadataToken.TypeReference handle ->
                namedIdentities
                    state
                    assembly
                    (TypeDefn.FromReference (assembly.TypeRefs.[handle], SignatureTypeKind.Unknown))
            | MetadataToken.TypeSpecification handle ->
                namedIdentities state assembly assembly.TypeSpecs.[handle].Signature
            | MetadataToken.MethodSpecification handle ->
                let spec = assembly.MethodSpecs.[handle]
                let state, inner = spelled state spec.Method

                ((state, inner), spec.Signature)
                ||> Seq.fold (fun (state, acc) argument ->
                    let state, named = namedIdentities state assembly argument
                    state, named @ acc
                )
            | MetadataToken.MemberReference handle -> spelled state assembly.Members.[handle].Parent
            | MetadataToken.FieldDefinition handle -> state, [ assembly.Fields.[handle].DeclaringType.Identity ]
            | _ -> state, []

        let state, named = spelled state token

        // Every base type, from each type named, and every type argument a base type spells.
        let rec withBases
            (state : EscapeAnalysisState)
            (pending : ResolvedTypeIdentity list)
            (seen : Set<ResolvedTypeIdentity>)
            =
            match pending with
            | [] -> state, seen
            | identity :: rest when seen.Contains identity -> withBases state rest seen
            | identity :: rest ->
                let definingAssembly, ty = definitionOf state identity

                let state, arguments =
                    match ty.BaseType with
                    | Some (BaseTypeInfo.TypeSpec handle) ->
                        namedIdentities state definingAssembly definingAssembly.TypeSpecs.[handle].Signature
                    | _ -> state, []

                match baseOf state identity with
                | state, Some parent -> withBases state (parent :: arguments @ rest) (seen.Add identity)
                | state, None -> withBases state (arguments @ rest) (seen.Add identity)

        let state, all = withBases state named Set.empty
        state, Set.toList all

    /// Bind the metadata token an instruction names, as the JIT does before the body runs: what it
    /// names when that is a method, and what binding it can throw.
    let rec private bindToken
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * CallTarget option * OutsideBodyFact list
        =
        let raisesOf (failure : BindFailure option) : OutsideBodyFact list =
            failure |> Option.map (bindFailureRaises state) |> Option.toList

        let dependsOnInstantiation = OutsideBodyFact.Opaque Opacity.DependsOnInstantiation

        match token with
        | MetadataToken.MethodDef handle ->
            let state, target = callTarget state assembly token

            let state, failure =
                firstBindFailure state assembly (signatureTypes assembly.Methods.[handle].Signature)

            state, Some target, raisesOf failure
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]
            let state, _, failures = bindToken state assembly spec.Method
            // The specification, not the method it instantiates, says how the callee is instantiated.
            let state, target = callTarget state assembly token
            let target = Some target

            let state, argumentsFailure = firstBindFailure state assembly spec.Signature
            state, target, raisesOf argumentsFailure @ failures
        | MetadataToken.MemberReference handle ->
            // Resolving the member reads only the parent's definition, but binding the reference
            // loads the parent type itself, with every type argument it spells.
            let state, parentFailure =
                match assembly.Members.[handle].Parent with
                | MetadataToken.TypeSpecification parent ->
                    spellingBindFailure state assembly assembly.TypeSpecs.[parent].Signature
                | _ -> state, None

            let state, target, failures =
                match assembly.Members.[handle].Signature with
                | MemberSignature.Method signature ->
                    let state, signatureFailure =
                        firstBindFailure state assembly (signatureTypes signature)

                    let signatureFailures = raisesOf signatureFailure

                    match callTarget state assembly token with
                    | state, CallTarget.Missing ->
                        state,
                        Some CallTarget.Missing,
                        OutsideBodyFact.Raises (ThrownType.Exactly (corelibException state "MissingMethodException"))
                        :: signatureFailures
                    | state, CallTarget.TypeMissing ->
                        state, Some CallTarget.TypeMissing, raisesOf (Some BindFailure.TypeAbsent)
                    | state, CallTarget.AssemblyMissing ->
                        state, Some CallTarget.AssemblyMissing, raisesOf (Some BindFailure.AssemblyUnavailable)
                    | state, CallTarget.DependsOnInstantiation ->
                        state, Some CallTarget.DependsOnInstantiation, dependsOnInstantiation :: signatureFailures
                    | state, target -> state, Some target, signatureFailures
                | MemberSignature.Field _ ->
                    let assemblies, target =
                        FieldReferenceResolution.resolve
                            state.LoggerFactory
                            state.RuntimeDirs
                            state.BaseTypes
                            state.TypeSystem._LoadedAssemblies
                            assembly
                            handle

                    let state = withAssemblies state assemblies

                    match target with
                    | FieldReferenceTarget.Missing ->
                        state,
                        None,
                        [
                            OutsideBodyFact.Raises (ThrownType.Exactly (corelibException state "MissingFieldException"))
                        ]
                    | FieldReferenceTarget.ParentTypeMissing _ -> state, None, raisesOf (Some BindFailure.TypeAbsent)
                    | FieldReferenceTarget.ParentAssemblyUnavailable _ ->
                        state, None, raisesOf (Some BindFailure.AssemblyUnavailable)
                    | FieldReferenceTarget.DependsOnInstantiation -> state, None, [ dependsOnInstantiation ]
                    | FieldReferenceTarget.Defined _ -> state, None, []

            state, target, raisesOf parentFailure @ failures
        | MetadataToken.TypeReference handle ->
            let state, failure =
                spellingBindFailure
                    state
                    assembly
                    (TypeDefn.FromReference (assembly.TypeRefs.[handle], SignatureTypeKind.Unknown))

            state, None, raisesOf failure
        | MetadataToken.TypeSpecification handle ->
            let state, failure =
                spellingBindFailure state assembly assembly.TypeSpecs.[handle].Signature

            state, None, raisesOf failure
        | MetadataToken.StandaloneSignature handle ->
            // A `calli`'s call-site signature.
            let signature =
                (assembly.PeReader.GetMetadataReader().GetStandaloneSignature handle)
                    .DecodeMethodSignature (TypeDefn.typeProvider assembly.Name, ())
                |> TypeMethodSignature.make

            let state, failure = firstBindFailure state assembly (signatureTypes signature)
            state, None, raisesOf failure
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
            Rethrows = []
            Regions = []
            OutsideBody = Set.empty
        }

    /// The exceptions an operation of the runtime's own can raise. Its contract says under which
    /// conditions on its arguments; the analysis does not track their values, so each may be raised.
    let private contractRaises (state : EscapeAnalysisState) (contract : IntrinsicContract) : ThrownType list =
        contract.Raises
        |> List.map (fun (fault, _) ->
            match fault with
            | PrimitiveFault.NullReference -> ThrownType.Exactly (corelibException state "NullReferenceException")
            | PrimitiveFault.DataMisaligned -> ThrownType.Exactly (corelibException state "DataMisalignedException")
        )

    /// What a fault the CPU reports raises, as the VM raises it; `None` for an out-of-range
    /// immediate, which the JIT throws by calling a helper in CoreLib.
    let private instructionRaises (state : EscapeAnalysisState) (fault : InstructionFault) : ThrownType option =
        match fault with
        | InstructionFault.NullAddress -> Some (ThrownType.Exactly (corelibException state "NullReferenceException"))
        | InstructionFault.ImmediateOutOfRange -> None
        | InstructionFault.ZeroDivisor -> Some (ThrownType.Exactly (corelibException state "DivideByZeroException"))
        | InstructionFault.QuotientOverflow -> Some (ThrownType.Exactly (corelibException state "OverflowException"))

    /// What CoreCLR runs when `key` is called. The VM's substitute for an intrinsic runs whatever IL
    /// CoreLib ships in its place, working or not.
    let private runsFor (assembly : DumpedAssembly) (key : MethodKey) : Runs =
        let method = assembly.Methods.[key.Method.Get]

        let intrinsic =
            if IntrinsicBody.isIntrinsic assembly key.Method.Get then
                Some (IntrinsicBody.classify assembly key.Method.Get)
            else
                None

        let substituted =
            match intrinsic with
            | Some _ -> VmSubstitution.unsafeStub assembly key.Method.Get
            | None -> None

        match substituted, intrinsic with
        | Some stub, _ -> Runs.Il (stub, None)
        | None, Some IntrinsicBody.VmSubstitution ->
            // A substitute the VM chooses by instantiation, which may be one of the runtime's own
            // operations.
            match IntrinsicPrimitive.recognise assembly key.Method.Get with
            | Some primitive -> Runs.Primitive primitive
            | None -> Runs.Opaque Opacity.IntrinsicExpansion
        | None, Some (IntrinsicBody.JitExpansion expansion) ->
            match method.Body with
            | MethodBody.Il body -> Runs.Il (body, Some expansion)
            | _ ->
                failwith
                    $"%O{key}: IntrinsicBody classifies it as a JIT expansion, whose IL calls itself, but it has no IL"
        | None, _ ->
            match method.Body with
            | MethodBody.Abstract -> Runs.Opaque Opacity.AbstractBody
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _ ->
                match NativeMethod.recognise assembly key.Method.Get with
                | Some native -> Runs.Native native
                | None -> Runs.Opaque Opacity.NativeBody
            | MethodBody.Il body -> Runs.Il (body, None)

    /// What one body does by itself.
    let private factsOf (state : EscapeAnalysisState) (key : MethodKey) : EscapeAnalysisState * LocalFacts =
        let assembly, method = methodOf state key

        let contracted (contract : IntrinsicContract) : LocalFacts =
            {
                Raises = contractRaises state contract |> List.map (fun thrown -> 0, thrown)
                Opaque = []
                Calls = []
                Rethrows = []
                Regions = []
                OutsideBody = Set.empty
            }

        match runsFor assembly key with
        | Runs.Opaque reason -> state, opaqueFromEntry reason
        | Runs.Primitive primitive -> state, contracted (IntrinsicPrimitive.contract primitive)
        | Runs.Native native -> state, contracted (NativeMethod.contract native)
        | Runs.Il (body, selfCall) ->

        let ops = body.Instructions |> Array.ofList

        // Offsets some branch or handler can land on: a `throw` at one of these may be reached
        // from somewhere other than the instruction above it, so that instruction is not
        // necessarily what produced its operand.
        let entered = ControlFlow.landedOn body

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
        // Nor does a call of a static virtual name the type it lands on, but that type's
        // initializer is the call site's to account for: `callsOf` does, for each instance whose
        // dispatch it decides, and a call it does not decide is opaque.
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
                    | Some (CallTarget.Method callee) -> state, Some (declaringTypeOf state callee.Callee)
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

            match methodTarget with
            | Some (CallTarget.Method callee) when isStaticVirtual state callee.Callee -> state, true
            | _ ->

            match owner state with
            | state, Some owner -> state, (Some owner = initializing || not (hasTypeInitializer state owner))
            | state, None -> state, false

        // 0. Bind the token the instruction names, which the JIT does before the body runs. This
        // binds every instruction's, as the JIT does for code no run reaches unless a branch it
        // folds cuts that code off; it folds a branch a capability query decides, so for code
        // only such a branch reaches, this is an over-approximation.
        let bind
            (
                state : EscapeAnalysisState,
                targets : Map<int, CallTarget>,
                unbound : Set<int>,
                bindingFailures : Set<OutsideBodyFact>
            )
            (index : int)
            =
            let op, _ = ops.[index]

            let state, methodTarget, unbound, bindingFailures =
                match op with
                | IlOp.UnaryMetadataToken (tokenOp, MetadataOperand.FromMetadata token) ->
                    let state, target, failures = bindToken state assembly token.Token

                    // A token that fails to bind names nothing to instantiate: the JIT throws
                    // before the body runs.
                    let target, unbound =
                        if failures.IsEmpty then
                            target, unbound
                        else
                            let target =
                                match target with
                                | Some (CallTarget.Method callee) ->
                                    Some (
                                        CallTarget.Method
                                            { callee with
                                                Spelling = CalleeSpelling.Typical
                                            }
                                    )
                                | other -> other

                            target, Set.add index unbound

                    // Binding also runs the module initializer of every other module it activates;
                    // this module's has run, since its code is running.
                    let state, initializes =
                        if activatesModules tokenOp then
                            let state, activated = activatedTypes state assembly token.Token

                            state,
                            activated
                            |> List.exists (fun identity ->
                                identity.AssemblyFullName <> assembly.DefinitionFullName
                                && hasModuleInitializer state identity.AssemblyFullName
                            )
                        else
                            state, false

                    let failures =
                        if initializes then
                            OutsideBodyFact.Raises (
                                ThrownType.Exactly (corelibException state "TypeInitializationException")
                            )
                            :: failures
                        else
                            failures

                    state, target, unbound, Set.union bindingFailures (Set.ofList failures)
                | _ -> state, None, unbound, bindingFailures

            let targets =
                match methodTarget with
                | Some target -> Map.add index target targets
                | None -> targets

            state, targets, unbound, bindingFailures

        // The `constrained.` prefix on the instruction at `index`: its own index, and the type it
        // names. An instruction's prefixes immediately precede it, and none of them can be a
        // branch target.
        let constrainedPrefix (index : int) : (int * MetadataToken) option =
            let rec back (at : int) : (int * MetadataToken) option =
                if at < 0 then
                    None
                else
                    match fst ops.[at] with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Constrained, MetadataOperand.FromMetadata token) ->
                        Some (at, token.Token)
                    | IlOp.Nullary (NullaryIlOp.Tail | NullaryIlOp.Volatile | NullaryIlOp.Readonly)
                    | IlOp.UnaryConst (UnaryConstIlOp.Unaligned _) -> back (at - 1)
                    | _ -> None

            back (index - 1)

        let folder
            (targets : Map<int, CallTarget>)
            (unbound : Set<int>)
            (
                state : EscapeAnalysisState,
                raises : (int * ThrownType) list,
                opaque : (int * Opacity) list,
                calls : (int * CallSite) list
            )
            (index : int)
            =
            let op, offset = ops.[index]
            let methodTarget = Map.tryFind index targets

            // 1. What the instruction raises by itself. What a `rethrow` raises is its handler's to
            // say, which `rethrows` below records.
            let state, raises, opaque =
                match OpcodeFaults.ofIlOp op with
                | OpcodeFaults.Unmodelled ->
                    match op with
                    | IlOp.Nullary NullaryIlOp.Rethrow -> state, raises, opaque
                    | _ -> state, raises, (offset, Opacity.UnmodelledOpcode) :: opaque
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

                    let state, operand =
                        match previous with
                        | Some (IlOp.Nullary NullaryIlOp.LdNull) -> state, ThrowOperand.Null
                        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj,
                                                         MetadataOperand.FromMetadata token)) ->
                            match callTarget state assembly token.Token with
                            | state, CallTarget.Method constructor ->
                                state,
                                ThrowOperand.Object (ThrownType.Exactly (declaringTypeOf state constructor.Callee))
                            | state, _ -> state, ThrowOperand.Untyped
                        | Some (IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt),
                                                         MetadataOperand.FromMetadata token)) ->
                            match returnTypeOfCall state assembly token.Token with
                            | state, Some ty -> state, ThrowOperand.Object (ThrownType.SubtypeOf ty)
                            | state, None -> state, ThrowOperand.Untyped
                        | _ -> state, ThrowOperand.Untyped

                    match operand with
                    | ThrowOperand.Object thrown -> state, (offset, thrown) :: raises, opaque
                    // The fault `OpcodeFaults` names for `throw` is all a null operand raises.
                    | ThrowOperand.Null -> state, raises, opaque
                    | ThrowOperand.Untyped -> state, raises, (offset, Opacity.UntypedThrow) :: opaque
                | _ -> state, raises, opaque

            // 3. What the instruction calls.
            let state, raises, opaque, calls =
                match op with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, _) ->
                    state, raises, (offset, Opacity.IndirectCall) :: opaque, calls
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldvirtftn, MetadataOperand.FromMetadata _) ->
                    let onInterface =
                        match methodTarget with
                        | Some (CallTarget.Method target) ->
                            let _, declaring = definitionOf state (declaringTypeOf state target.Callee)
                            declaring.TypeAttributes.HasFlag TypeAttributes.Interface
                        | _ -> false

                    if onInterface then
                        state, raises, (offset, Opacity.InterfaceMethodPointer) :: opaque, calls
                    else
                        state, raises, opaque, calls
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp) as call,
                                           operand) ->
                    match operand, methodTarget, selfCall with
                    | MetadataOperand.FromDynamicScope _, _, _ ->
                        state, raises, (offset, Opacity.IndirectCall) :: opaque, calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.Method callee), Some expansion when
                        callee.Callee = key
                        && call <> UnaryMetadataTokenIlOp.Newobj
                        && call <> UnaryMetadataTokenIlOp.Jmp
                        ->
                        // The body's call to itself, which the JIT expands where it stands
                        // (`gtIsRecursiveCall`): no call happens, so the raises are this offset's.
                        let raisedHere (thrown : ThrownType list) =
                            state, (thrown |> List.map (fun thrown -> offset, thrown)) @ raises, opaque, calls

                        match IntrinsicBody.expandSelfCall state.Profile expansion with
                        | SelfCallExpansion.Constant _ -> state, raises, opaque, calls
                        | SelfCallExpansion.ThrowPlatformNotSupported ->
                            // The JIT calls CoreLib's helper in place of the method, and what the
                            // helper raises is its own IL's to say.
                            let corelib = state.BaseTypes.Corelib

                            let helper =
                                MethodKey.make corelib (IntrinsicBody.platformNotSupportedHelper corelib)

                            let helper =
                                CallSite.Direct
                                    {
                                        Callee = helper
                                        Spelling = CalleeSpelling.Fixed
                                    }

                            state, raises, opaque, (offset, helper) :: calls
                        | SelfCallExpansion.Primitive primitive ->
                            raisedHere (contractRaises state (IntrinsicPrimitive.contract primitive))
                        | SelfCallExpansion.HardwareInstruction intrinsicClass ->
                            match HardwareInstruction.contract state.Target intrinsicClass method.Name with
                            | InstructionContract.Raises faults ->
                                let state, raises, opaque, calls =
                                    faults |> Set.toList |> List.choose (instructionRaises state) |> raisedHere

                                // The JIT throws for an out-of-range immediate by calling CoreLib's
                                // helper, and what that raises is its own IL's to say.
                                if faults.Contains InstructionFault.ImmediateOutOfRange then
                                    let corelib = state.BaseTypes.Corelib

                                    let helper = MethodKey.make corelib (IntrinsicBody.argumentOutOfRangeHelper corelib)

                                    let helper =
                                        CallSite.Direct
                                            {
                                                Callee = helper
                                                Spelling = CalleeSpelling.Fixed
                                            }

                                    state, raises, opaque, (offset, helper) :: calls
                                else
                                    state, raises, opaque, calls
                            | InstructionContract.Unknown ->
                                state, raises, (offset, Opacity.IntrinsicExpansion) :: opaque, calls
                        | SelfCallExpansion.Unrecognised ->
                            state, raises, (offset, Opacity.IntrinsicExpansion) :: opaque, calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.Method callee), _ ->
                        // A static virtual is dispatched on the type a `constrained.` prefix names,
                        // which is how the only legal call to one is written; an overridable
                        // method, on that type where the prefix is there.
                        if
                            isStaticVirtual state callee.Callee
                            || (call = UnaryMetadataTokenIlOp.Callvirt && isOverridable state callee.Callee)
                        then
                            match constrainedPrefix index with
                            | Some (prefix, constrainedType) when not (unbound.Contains prefix) ->
                                state, raises, opaque, (offset, CallSite.Constrained (constrainedType, callee)) :: calls
                            | _ -> state, raises, (offset, Opacity.VirtualCall) :: opaque, calls
                        else
                            state, raises, opaque, (offset, CallSite.Direct callee) :: calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.ArrayAccessor (arrayType, accessor)), _ ->
                        let raised =
                            arrayAccessorRaises state arrayType accessor
                            |> List.map (fun thrown -> offset, thrown)

                        state, raised @ raises, opaque, calls
                    | MetadataOperand.FromMetadata _, Some CallTarget.DependsOnInstantiation, _ ->
                        state, raises, (offset, Opacity.DependsOnInstantiation) :: opaque, calls
                    // Binding the token fails, which step 0 recorded; there is nothing to call.
                    | MetadataOperand.FromMetadata _, Some CallTarget.Missing, _
                    | MetadataOperand.FromMetadata _, Some CallTarget.TypeMissing, _
                    | MetadataOperand.FromMetadata _, Some CallTarget.AssemblyMissing, _ -> state, raises, opaque, calls
                    | MetadataOperand.FromMetadata token, None, _ ->
                        failwith
                            $"A call in %s{assembly.DefinitionFullName} names %O{token.Token}, which is not a method"
                | _ -> state, raises, opaque, calls

            state, raises, opaque, calls

        // The JIT loads the type of every local and every `catch` clause before the body runs, as
        // it binds every token.
        let state, localFailures =
            ((state, Set.empty), Option.defaultValue ImmutableArray.Empty body.LocalVars)
            ||> Seq.fold (fun (state, failures) local ->
                match spellingBindFailure state assembly local with
                | state, None -> state, failures
                | state, Some failure -> state, Set.add (bindFailureRaises state failure) failures
            )

        let state, localFailures =
            ((state, localFailures), body.ExceptionRegions)
            ||> Seq.fold (fun (state, failures) region ->
                match region with
                | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, _) ->
                    let state, _, clauseFailures = bindToken state assembly token
                    state, Set.union failures (Set.ofList clauseFailures)
                | _ -> state, failures
            )

        // A synchronized method takes a monitor around its body, outside its handlers: on its
        // receiver, which `call` may pass as null, or on its type if static. Taking it may be
        // interrupted, and releasing it fails if the body already has.
        let localFailures =
            let synchronized =
                match method with
                | MethodInfo.Metadata (_, facts) -> facts.ImplAttributes.HasFlag MethodImplAttributes.Synchronized
                | MethodInfo.Synthesised _ -> false

            if synchronized then
                [
                    corelibType state "System.Threading" "ThreadInterruptedException"
                    corelibType state "System.Threading" "SynchronizationLockException"
                    if not method.IsStatic then
                        corelibException state "ArgumentNullException"
                ]
                |> List.map (ThrownType.Exactly >> OutsideBodyFact.Raises)
                |> Set.ofList
                |> Set.union localFailures
            else
                localFailures

        let state, targets, unbound, bindingFailures =
            ((state, Map.empty, Set.empty, localFailures), [ 0 .. ops.Length - 1 ])
            ||> List.fold bind

        // What each call to a capability query returns on this CPU, which decides a branch on it.
        let constants =
            ops
            |> Seq.indexed
            |> Seq.choose (fun (index, (op, offset)) ->
                match op, Map.tryFind index targets with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromMetadata _),
                  Some (CallTarget.Method callee) ->
                    let calleeAssembly, _ = methodOf state callee.Callee

                    IntrinsicBody.constantResult state.Profile calleeAssembly callee.Callee.Method.Get
                    |> Option.map (fun value -> offset, value)
                | _ -> None
            )
            |> Map.ofSeq

        let executed = ControlFlow.mayExecute body constants

        let state, raises, opaque, calls =
            ((state, [], [], []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun acc index ->
                if executed.Contains (snd ops.[index]) then
                    folder targets unbound acc index
                else
                    acc
            )

        let regions = List.ofSeq body.ExceptionRegions

        // Each `rethrow` belongs to the innermost handler, or filter, around it. Only a `catch`
        // or `filter` handler may hold one (ECMA-335 III.4.24), so one anywhere else is opaque.
        let opaque, rethrows =
            ((List.rev opaque, []), ops)
            ||> Array.fold (fun (opaque, rethrows) (op, offset) ->
                match op with
                | IlOp.Nullary NullaryIlOp.Rethrow when executed.Contains offset ->
                    let blocks =
                        regions
                        |> List.collect (fun region ->
                            let handlerOf (o : ExceptionOffset) =
                                o.HandlerOffset, o.HandlerLength, Some region

                            match region with
                            | ExceptionRegion.Catch (_, o) -> [ handlerOf o ]
                            | ExceptionRegion.Filter (filterOffset, o) ->
                                [ handlerOf o ; filterOffset, o.HandlerOffset - filterOffset, None ]
                            | ExceptionRegion.Finally o
                            | ExceptionRegion.Fault o -> [ o.HandlerOffset, o.HandlerLength, None ]
                        )
                        |> List.filter (fun (start, length, _) -> offset >= start && offset < start + length)

                    match blocks |> List.sortBy (fun (_, length, _) -> length) with
                    | (_, _, Some clause) :: _ -> opaque, (offset, clause) :: rethrows
                    | _ -> opaque @ [ offset, Opacity.Rethrow ], rethrows
                | _ -> opaque, rethrows
            )

        state,
        {
            Raises = List.rev raises
            Opaque = opaque
            Calls = List.rev calls
            Rethrows = List.rev rethrows
            Regions = regions
            OutsideBody = bindingFailures
        }

    /// How deeply an instance's type arguments may nest. A generic method can call itself at a
    /// deeper instantiation each time, so the instances reachable from one call are unbounded; one
    /// nested deeper than this is analysed as the open definition instead.
    let private instantiationDepthLimit : int = 8

    /// Whether a spelling mentions a type variable of the body that spells it.
    let rec private mentionsTypeVariable (spelling : TypeDefn) : bool =
        match spelling with
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.GenericInstantiation (root, arguments) ->
            mentionsTypeVariable root || Seq.exists mentionsTypeVariable arguments
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> mentionsTypeVariable element
        | TypeDefn.Modified modified -> mentionsTypeVariable modified.Unmodified
        | TypeDefn.FunctionPointer signature -> signatureTypes signature |> List.exists mentionsTypeVariable
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void -> false

    /// The type arguments a call's token spells, over the calling body's type variables.
    let rec private spelledArguments (assembly : DumpedAssembly) (token : MetadataToken) : TypeDefn list =
        match token with
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]
            List.ofSeq spec.Signature @ spelledArguments assembly spec.Method
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Parent with
            | MetadataToken.TypeSpecification parent -> [ assembly.TypeSpecs.[parent].Signature ]
            | _ -> []
        | _ -> []

    /// How deeply a type's arguments nest.
    let rec private depthOf (state : EscapeAnalysisState) (handle : ConcreteTypeHandle) : int =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle state.TypeSystem.ConcreteTypes with
            | Some ty -> 1 + (ty.Generics |> Seq.map (depthOf state) |> Seq.fold max 0)
            | None -> failwith $"Concrete type handle %O{handle} is not registered"
        | ConcreteTypeHandle.Byref element
        | ConcreteTypeHandle.Pointer element
        | ConcreteTypeHandle.OneDimArrayZero element
        | ConcreteTypeHandle.Array (element, _) -> 1 + depthOf state element
        | ConcreteTypeHandle.FunctionPointer signature ->
            let returned =
                match signature.ReturnType with
                | MethodReturnType.Void -> []
                | MethodReturnType.Returns ty -> [ ty ]

            1
            + (returned @ signature.ParameterTypes
               |> List.map (depthOf state)
               |> List.fold max 0)

    /// The instance of `definition` these arguments make, or its open definition if they nest
    /// deeper than `instantiationDepthLimit`.
    let private instanceOf
        (state : EscapeAnalysisState)
        (definition : MethodKey)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        : MethodInstance
        =
        let arguments =
            if
                typeArguments @ methodArguments
                |> List.exists (fun argument -> depthOf state argument > instantiationDepthLimit)
            then
                Instantiation.Open
            else
                Instantiation.Closed (typeArguments, methodArguments)

        {
            Definition = definition
            Arguments = arguments
        }

    /// A type a body of `assembly` spells, as that body's instantiation makes it.
    let private concretize
        (state : EscapeAnalysisState)
        (assembly : string)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        (spelling : TypeDefn)
        : EscapeAnalysisState * ConcreteTypeHandle
        =
        let typeSystem, handle =
            TypeSystemState.concretizeType
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                state.TypeSystem
                assembly
                (ImmutableArray.CreateRange typeArguments)
                (ImmutableArray.CreateRange methodArguments)
                spelling

        { state with
            TypeSystem = typeSystem
        },
        handle

    /// The arguments of the declaring type and of the method that a call's token, of a body of
    /// `assembly` whose instantiation is `typeArguments` and `methodArguments`, instantiates its
    /// callee with: a MemberRef's as the interpreter binds them
    /// (`MemberReferenceInstantiation.resolveMember`), and a MethodSpec's own.
    let rec private spelledInstantiation
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        (token : MetadataToken)
        : EscapeAnalysisState * ConcreteTypeHandle list * ConcreteTypeHandle list
        =
        match token with
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]

            let state, calleeMethodArguments =
                ((state, []), spec.Signature)
                ||> Seq.fold (fun (state, handles) argument ->
                    let state, handle =
                        concretize state assembly.DefinitionFullName typeArguments methodArguments argument

                    state, handle :: handles
                )

            let state, calleeTypeArguments, _ =
                match spec.Method with
                | MetadataToken.MethodDef _ -> state, [], []
                | inner -> spelledInstantiation state assembly typeArguments methodArguments inner

            state, calleeTypeArguments, List.rev calleeMethodArguments
        | MetadataToken.MemberReference handle ->
            let typeSystem, _, _, declaringTypeArguments =
                MemberReferenceInstantiation.resolveMember
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    assembly
                    (ImmutableArray.CreateRange typeArguments)
                    (ImmutableArray.CreateRange methodArguments)
                    handle
                    state.TypeSystem

            let state =
                { state with
                    TypeSystem = typeSystem
                }

            // Closed already: the interpreter concretises them in the calling assembly with no
            // context (`ExecutionConcretization.concretizeMethodForExecution`), as here.
            let state, calleeTypeArguments =
                ((state, []), declaringTypeArguments)
                ||> Seq.fold (fun (state, handles) argument ->
                    let state, handle = concretize state assembly.DefinitionFullName [] [] argument
                    state, handle :: handles
                )

            state, List.rev calleeTypeArguments, []
        | other -> failwith $"A call in %s{assembly.DefinitionFullName} spells an instantiation with %O{other}"

    /// The instance a call of `callee` from a body of `assembly` reaches, when that body runs as
    /// `caller`.
    let private calleeInstance
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (callee : Callee)
        : EscapeAnalysisState * MethodInstance
        =
        let unknown : MethodInstance =
            {
                Definition = callee.Callee
                Arguments = Instantiation.Open
            }

        match callee.Spelling with
        | CalleeSpelling.Fixed -> state, instanceOf state callee.Callee [] []
        | CalleeSpelling.Typical -> state, unknown
        | CalleeSpelling.Spelled token ->
            let context =
                match caller with
                | Instantiation.Closed (typeArguments, methodArguments) -> Some (typeArguments, methodArguments)
                | Instantiation.Open ->
                    if spelledArguments assembly token |> List.exists mentionsTypeVariable then
                        None
                    else
                        Some ([], [])

            match context with
            | None -> state, unknown
            | Some (typeArguments, methodArguments) ->
                let state, calleeTypeArguments, calleeMethodArguments =
                    spelledInstantiation state assembly typeArguments methodArguments token

                state, instanceOf state callee.Callee calleeTypeArguments calleeMethodArguments

    /// Whether everything dispatch reads of a receiver of the type `identity` binds: its base types,
    /// the interfaces any of them implements, and the signature of every method of each, which
    /// dispatch compares to find what implements a method. It reads no method's locals.
    let rec private dispatchBinds
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * bool
        =
        match state.DispatchBinds.TryFind identity with
        | Some known -> state, known
        | None ->

        let assembly, ty = definitionOf state identity

        // A type a spelling names, if the spelling binds.
        let named (state : EscapeAnalysisState) (spelling : TypeDefn) : EscapeAnalysisState * bool =
            match concretizable state assembly spelling with
            | state, false -> state, false
            | state, true ->
                match nominalIdentity state assembly spelling with
                | state, Some related -> dispatchBinds state related
                | state, None -> state, true

        let typeToken (state : EscapeAnalysisState) (token : MetadataToken) : EscapeAnalysisState * bool =
            match token with
            | MetadataToken.TypeDefinition handle -> dispatchBinds state assembly.TypeDefs.[handle].Identity
            | MetadataToken.TypeReference handle ->
                match resolveTypeRef state assembly assembly.TypeRefs.[handle] with
                | state, Some related -> dispatchBinds state related
                | state, None -> state, false
            | MetadataToken.TypeSpecification handle -> named state assembly.TypeSpecs.[handle].Signature
            | _ -> state, false

        let methodBinds
            (state : EscapeAnalysisState)
            (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            : EscapeAnalysisState * bool
            =
            ((state, true), signatureTypes method.Signature)
            ||> List.fold (fun (state, soFar) spelling ->
                if soFar then
                    concretizable state assembly spelling
                else
                    state, false
            )

        let checks : (EscapeAnalysisState -> EscapeAnalysisState * bool) list =
            [
                match ty.BaseType with
                | None -> ()
                | Some (BaseTypeInfo.TypeDef handle) ->
                    yield fun state -> typeToken state (MetadataToken.TypeDefinition handle)
                | Some (BaseTypeInfo.TypeRef handle) ->
                    yield fun state -> typeToken state (MetadataToken.TypeReference handle)
                | Some (BaseTypeInfo.TypeSpec handle) ->
                    yield fun state -> typeToken state (MetadataToken.TypeSpecification handle)
                for implemented in ty.ImplementedInterfaces do
                    yield fun state -> typeToken state implemented.InterfaceHandle
                for method in ty.Methods do
                    yield fun state -> methodBinds state method
            ]

        let state, binds =
            ((state, true), checks)
            ||> List.fold (fun (state, soFar) check -> if soFar then check state else state, false)

        { state with
            DispatchBinds = state.DispatchBinds.Add (identity, binds)
        },
        binds

    /// What a `constrained.` call does, when the body of `assembly` making it runs as `caller`, if
    /// the type the prefix names decides it, which it does for a value type and for a sealed class
    /// (ECMA-335 III.2.1). Undecided where an instance of a derived class may receive the call, or
    /// the type is not known.
    let private constrainedInstance
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (constrainedType : MetadataToken)
        (callee : Callee)
        : EscapeAnalysisState * ConstrainedOutcome
        =
        let state, named = calleeInstance state assembly caller callee

        let typeSystem, spelling, spellingAssembly =
            TypeSystemState.resolveTypeMetadataToken
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                state.TypeSystem
                assembly
                constrainedType

        let state =
            { state with
                TypeSystem = typeSystem
            }

        let context =
            match caller with
            | Instantiation.Closed (typeArguments, methodArguments) -> Some (typeArguments, methodArguments)
            | Instantiation.Open ->
                if mentionsTypeVariable spelling then
                    None
                else
                    Some ([], [])

        match named.Arguments, context with
        | Instantiation.Open, _
        | _, None -> state, ConstrainedOutcome.Undecided
        | Instantiation.Closed (namedTypeArguments, namedMethodArguments), Some (typeArguments, methodArguments) ->

        let state, receiver =
            concretize state spellingAssembly.DefinitionFullName typeArguments methodArguments spelling

        let _, definition = methodOf state named.Definition

        // Instantiating the method the call names reads its locals, which the JIT does not unless
        // that method runs.
        let namedLocals =
            match definition.Body with
            | MethodBody.Il body -> body.LocalVars |> Option.map List.ofSeq |> Option.defaultValue []
            | _ -> []

        let state, binds =
            match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem receiver with
            | None -> state, true
            | Some (_, receiverType) ->
                match dispatchBinds state receiverType.Identity with
                | state, true -> dispatchBinds state definition.RequiredDeclaringType.Identity
                | state, false -> state, false

        let state, binds =
            if binds then
                let definitionAssembly = assemblyOf state definition.DeclaringAssemblyFullName

                ((state, true), namedLocals)
                ||> List.fold (fun (state, soFar) spelling ->
                    if soFar then
                        concretizable state definitionAssembly spelling
                    else
                        state, false
                )
            else
                state, false

        if not binds then
            state, ConstrainedOutcome.Undecided
        else

        let typeSystem, concretized, declaringType =
            MethodConcretisation.concretizeMethodWithAllGenerics
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                (ImmutableArray.CreateRange namedTypeArguments)
                definition
                (ImmutableArray.CreateRange namedMethodArguments)
                state.TypeSystem

        let implementationOn (walkBaseTypes : bool) (typeSystem : TypeSystemState) =
            ConcreteVirtualDispatch.tryResolveVirtualImplementation
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                concretized.Generics
                concretized
                receiver
                walkBaseTypes
                typeSystem

        // What `callvirt` runs on a receiver of exactly the type `receiver`: an override, or else
        // the method the call names.
        let dispatchedOn (typeSystem : TypeSystemState) : TypeSystemState * VirtualImplementation =
            match implementationOn true typeSystem with
            | typeSystem, VirtualImplementation.NotOverridden ->
                let named : DispatchedMethod =
                    {
                        Definition = definition
                        TypeGenerics = ImmutableArray.CreateRange namedTypeArguments
                        MethodGenerics = ImmutableArray.CreateRange namedMethodArguments
                    }

                typeSystem, VirtualImplementation.Found named
            | decided -> decided

        // A type that is not one the method's declaring type admits leaves what runs to the
        // receiver's class: one implementing `IDynamicInterfaceCastable` is asked for it.
        let typeSystem, admitted =
            TypeAssignability.isConcreteTypeAssignableTo
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                typeSystem
                receiver
                declaringType

        // `None` where the type does not decide what runs.
        let typeSystem, runs =
            match TypeSystemState.tryGetConcreteTypeInfo typeSystem receiver with
            | _ when not admitted -> typeSystem, None
            // An array may be of a covariant derived element type.
            | None -> typeSystem, None
            | Some (_, receiverType) ->
                if definition.IsStatic then
                    // No object receives the call, so the type decides it, however it is derived
                    // from.
                    match
                        StaticVirtualDispatch.resolve
                            state.LoggerFactory
                            state.RuntimeDirs
                            state.BaseTypes
                            receiver
                            concretized
                            typeSystem
                    with
                    | typeSystem, VirtualImplementation.Found runs when not runs.Definition.IsStatic -> typeSystem, None
                    | typeSystem, decided -> typeSystem, Some decided
                elif LoadedTypeInfo.isValueType state.BaseTypes typeSystem._LoadedAssemblies receiverType then
                    match implementationOn false typeSystem with
                    | typeSystem, VirtualImplementation.NotOverridden ->
                        // A value type that does not implement the method itself is boxed, and the
                        // call dispatched on the box (ECMA-335 III.2.1): to a method it inherits from
                        // Object, ValueType or Enum, or to an interface's default body. A boxed
                        // Nullable is its underlying value or null, not a Nullable.
                        if receiverType.Identity <> state.BaseTypes.Nullable.Identity then
                            let typeSystem, decided = dispatchedOn typeSystem
                            typeSystem, Some decided
                        else
                            typeSystem, None
                    | typeSystem, decided -> typeSystem, Some decided
                elif receiverType.TypeAttributes.HasFlag TypeAttributes.Sealed then
                    let typeSystem, decided = dispatchedOn typeSystem
                    typeSystem, Some decided
                else
                    typeSystem, None

        let state =
            { state with
                TypeSystem = typeSystem
            }

        match runs with
        | None
        | Some VirtualImplementation.NotOverridden
        | Some (VirtualImplementation.Unmodelled _) -> state, ConstrainedOutcome.Undecided
        | Some (VirtualImplementation.Ambiguous _) ->
            state,
            ConstrainedOutcome.Raises (
                ThrownType.Exactly (corelibType state "System.Runtime" "AmbiguousImplementationException")
            )
        | Some (VirtualImplementation.Found runs) ->
            match runs.Definition.TryMetadata with
            | None -> state, ConstrainedOutcome.Undecided
            | Some facts ->
                let key =
                    MethodKey.make (assemblyOf state runs.Definition.DeclaringAssemblyFullName) facts.Handle

                state,
                ConstrainedOutcome.Reaches (
                    instanceOf state key (List.ofSeq runs.TypeGenerics) (List.ofSeq runs.MethodGenerics)
                )

    /// What `instance` calls, from the facts of its definition.
    let private callsOf
        (state : EscapeAnalysisState)
        (instance : MethodInstance)
        (facts : LocalFacts)
        : EscapeAnalysisState * InstanceCalls
        =
        let assembly = assemblyOf state instance.Definition.AssemblyFullName

        let state, callees, raises, undecided =
            ((state, [], [], []), facts.Calls)
            ||> List.fold (fun (state, callees, raises, undecided) (offset, site) ->
                match site with
                | CallSite.Direct callee ->
                    let state, reached = calleeInstance state assembly instance.Arguments callee
                    state, (offset, reached) :: callees, raises, undecided
                | CallSite.Constrained (constrainedType, callee) ->
                    match constrainedInstance state assembly instance.Arguments constrainedType callee with
                    | state, ConstrainedOutcome.Reaches reached ->
                        // A static method's call runs its declaring type's initializer, which may
                        // fail; the facts leave that to the type dispatch lands on.
                        let raises =
                            if
                                isStaticVirtual state callee.Callee
                                && hasTypeInitializer state (declaringTypeOf state reached.Definition)
                            then
                                (offset, ThrownType.Exactly (corelibException state "TypeInitializationException"))
                                :: raises
                            else
                                raises

                        state, (offset, reached) :: callees, raises, undecided
                    | state, ConstrainedOutcome.Raises thrown -> state, callees, (offset, thrown) :: raises, undecided
                    | state, ConstrainedOutcome.Undecided ->
                        state, callees, raises, (offset, Opacity.VirtualCall) :: undecided
            )

        state,
        {
            Callees = List.rev callees
            Raises = List.rev raises
            Undecided = List.rev undecided
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
        // A generic definition is asked about for every instantiation at once.
        let root =
            let _, definition = methodOf state method

            if definition.DeclaringTypeGenerics.IsEmpty && definition.Generics.IsEmpty then
                instanceOf state method [] []
            else
                {
                    Definition = method
                    Arguments = Instantiation.Open
                }

        match state.Summaries.TryFind root with
        | Some known -> state, known
        | None ->

        // Every instance reachable from `root` that has no summary yet, with what it calls and the
        // local facts of its definition.
        let rec discover (state : EscapeAnalysisState) (pending : MethodInstance list) (found : MethodInstance list) =
            match pending with
            | [] -> state, found
            | key :: rest when state.Summaries.ContainsKey key || state.InstanceCalls.ContainsKey key ->
                discover state rest found
            | key :: rest ->
                let state, facts =
                    match state.Facts.TryFind key.Definition with
                    | Some facts -> state, facts
                    | None ->
                        let state, facts = factsOf state key.Definition

                        { state with
                            Facts = state.Facts.Add (key.Definition, facts)
                        },
                        facts

                let state, calls = callsOf state key facts

                let state =
                    { state with
                        InstanceCalls = state.InstanceCalls.Add (key, calls)
                    }

                discover state ((calls.Callees |> List.map snd) @ rest) (key :: found)

        let state, reachable = discover state [ root ] []

        // Whether a raise at `offset` of `key`'s body gets out of it.
        let escapesAt (state : EscapeAnalysisState) (key : MethodInstance) (offset : int) (thrown : ThrownType option) =
            let facts = state.Facts.[key.Definition]
            escapesHandlers state (assemblyOf state key.Definition.AssemblyFullName) facts.Regions offset thrown

        // What each `rethrow` in `key`'s body re-raises, given what its callees let escape: what
        // its clause caught of what the clause's protected block raised. That block may hold
        // another `rethrow`, in a nested handler or in the protected block of a clause nested in
        // a handler, so the rethrows depend on each other; they are solved together, to the least
        // fixed point.
        let rethrown
            (state : EscapeAnalysisState)
            (key : MethodInstance)
            (summaryOf : MethodInstance -> Escapes)
            : EscapeAnalysisState * (int * ThrownType option) list
            =
            let facts = state.Facts.[key.Definition]
            let calls = state.InstanceCalls.[key]
            let assembly = assemblyOf state key.Definition.AssemblyFullName

            let protectedBy (clause : ExceptionRegion) : ExceptionOffset =
                match clause with
                | ExceptionRegion.Catch (_, o)
                | ExceptionRegion.Filter (_, o)
                | ExceptionRegion.Finally o
                | ExceptionRegion.Fault o -> o

            let rec solve
                (state : EscapeAnalysisState)
                (current : Map<int, Set<ThrownType option>>)
                : EscapeAnalysisState * Map<int, Set<ThrownType option>>
                =
                let state, next =
                    ((state, Map.empty), facts.Rethrows)
                    ||> List.fold (fun (state, next) (offset, clause) ->
                        let o = protectedBy clause

                        let inside (at : int) =
                            at >= o.TryOffset && at < o.TryOffset + o.TryLength

                        let raised =
                            [
                                for at, thrown in facts.Raises @ calls.Raises do
                                    if inside at then
                                        yield at, Some thrown
                                for at, _ in facts.Opaque @ calls.Undecided do
                                    if inside at then
                                        yield at, None
                                for at, callee in calls.Callees do
                                    if inside at then
                                        let calleeEscapes = summaryOf callee

                                        for thrown in calleeEscapes.Types do
                                            yield at, Some thrown

                                        if calleeEscapes.Unknown then
                                            yield at, None
                                for KeyValue (at, rethrownThere) in current do
                                    if inside at then
                                        for thrown in rethrownThere do
                                            yield at, thrown
                            ]
                            |> List.distinct

                        // The clauses the runtime tries before this one.
                        let before = facts.Regions |> List.takeWhile (fun region -> region <> clause)

                        let state, here =
                            ((state, Set.empty), raised)
                            ||> List.fold (fun (state, here) (at, thrown) ->
                                match escapesHandlers state assembly before at thrown with
                                | state, false -> state, here
                                | state, true ->
                                    match rethrownBy state assembly clause thrown with
                                    | state, Rethrown.Nothing -> state, here
                                    | state, Rethrown.Thrown thrown -> state, Set.add (Some thrown) here
                                    | state, Rethrown.Unknown -> state, Set.add None here
                            )

                        state, Map.add offset here next
                    )

                if next = current then state, current else solve state next

            let state, solved =
                solve state (facts.Rethrows |> List.map (fun (offset, _) -> offset, Set.empty) |> Map.ofList)

            state,
            [
                for KeyValue (offset, rethrownHere) in solved do
                    for thrown in rethrownHere do
                        yield offset, thrown
            ]

        let seedOf (state : EscapeAnalysisState) (key : MethodInstance) : EscapeAnalysisState * Escapes =
            let facts = state.Facts.[key.Definition]
            let calls = state.InstanceCalls.[key]

            // What happens outside the body is past all its handlers.
            let outside =
                facts.OutsideBody
                |> Seq.choose (fun fact ->
                    match fact with
                    | OutsideBodyFact.Raises thrown -> Some thrown
                    | OutsideBodyFact.Opaque _ -> None
                )
                |> Set.ofSeq

            let opaqueOutside =
                facts.OutsideBody
                |> Set.exists (fun fact ->
                    match fact with
                    | OutsideBodyFact.Raises _ -> false
                    | OutsideBodyFact.Opaque _ -> true
                )

            let state, types =
                ((state, outside), facts.Raises @ calls.Raises)
                ||> List.fold (fun (state, types) (offset, thrown) ->
                    match escapesAt state key offset (Some thrown) with
                    | state, true -> state, Set.add thrown types
                    | state, false -> state, types
                )

            let state, unknown =
                ((state, opaqueOutside), facts.Opaque @ calls.Undecided)
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
        let rec iterate (state : EscapeAnalysisState) (current : Map<MethodInstance, Escapes>) =
            let summaryOf (key : MethodInstance) : Escapes =
                match state.Summaries.TryFind key with
                | Some known -> known
                | None -> current.[key]

            let state, next =
                ((state, current), reachable)
                ||> List.fold (fun (state, next) key ->
                    let calls = state.InstanceCalls.[key]

                    let state, escaping =
                        ((state, seeds.[key]), calls.Callees)
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

                    let state, rethrows = rethrown state key summaryOf

                    let state, escaping =
                        ((state, escaping), rethrows)
                        ||> List.fold (fun (state, acc) (offset, thrown) ->
                            match thrown with
                            | Some thrown when not (acc.Types.Contains thrown) ->
                                match escapesAt state key offset (Some thrown) with
                                | state, true ->
                                    state,
                                    { acc with
                                        Types = acc.Types.Add thrown
                                    }
                                | state, false -> state, acc
                            | Some _ -> state, acc
                            | None when not acc.Unknown ->
                                match escapesAt state key offset None with
                                | state, unknown ->
                                    state,
                                    { acc with
                                        Unknown = unknown
                                    }
                            | None -> state, acc
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

        state, state.Summaries.[root]

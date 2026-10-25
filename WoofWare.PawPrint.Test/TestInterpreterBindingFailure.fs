namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The interpreter's resolution of a token reports what the token's binding fails with, as the type
/// system's does, keeping whatever the binding loaded and registered before it failed.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInterpreterBindingFailure =

    let private machine (f : UnmodelledImage.Fixture) : IlMachineState =
        let state = IlMachineState.initial f.LoggerFactory HostImage.runtimeDirs f.Analysed

        { state with
            TypeSystem =
                { state.TypeSystem with
                    _LoadedAssemblies = f.Assemblies
                }
        }

    /// What a type system has loaded and registered, as two type systems can be compared by.
    let private footprint (typeSystem : TypeSystemState) : string list * AllConcreteTypes =
        List.ofSeq typeSystem._LoadedAssemblies.DefinitionNamesInLoadOrder, typeSystem.ConcreteTypes

    /// A binding's outcome, as two of them can be compared by.
    let private outcome
        (describe : 'failure -> string)
        (bound : 'a -> string)
        (result : Result<'a, 'failure>)
        : string
        =
        match result with
        | Ok value -> $"Ok %s{bound value}"
        | Error failure -> $"Error %s{describe failure}"

    let private typeOutcome (result : Result<TypeDefn * DumpedAssembly, TypeBindingFailure>) : string =
        outcome
            TypeBindingFailure.describe
            (fun (defn : TypeDefn, assy : DumpedAssembly) -> $"%A{defn} in %s{assy.DefinitionFullName}")
            result

    let private typeInfoOutcome
        (result : Result<DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>)
        : string
        =
        outcome
            TypeBindingFailure.describe
            (fun (assy : DumpedAssembly, info : TypeInfo<TypeDefn, TypeDefn>) ->
                $"%s{info.Namespace}.%s{info.Name} in %s{assy.DefinitionFullName}"
            )
            result

    /// Each check's outcomes, so that a test can require both kinds among them.
    let private requireBoth (outcomes : string list) : unit =
        outcomes |> List.exists (fun o -> o.StartsWith "Ok ") |> shouldEqual true
        outcomes |> List.exists (fun o -> o.StartsWith "Error ") |> shouldEqual true

    /// Run the interpreter's resolution and the type system's from one state, and require the same
    /// outcome and the same type system after it.
    let private agrees
        (state : IlMachineState)
        (interpreter : IlMachineState -> IlMachineState * string)
        (typeSystem : TypeSystemState -> TypeSystemState * string)
        : string
        =
        let afterInterpreter, interpreted = interpreter state
        let afterTypeSystem, expected = typeSystem state.TypeSystem
        interpreted |> shouldEqual expected
        footprint afterInterpreter.TypeSystem |> shouldEqual (footprint afterTypeSystem)
        expected

    [<Test>]
    let ``every type token of an image resolves in the interpreter as in the type system`` () : unit =
        let f = UnmodelledImage.fixture ()
        let state = machine f
        let dirs = HostImage.runtimeDirs

        let tokens =
            [
                yield! f.Analysed.TypeRefs.Keys |> Seq.map MetadataToken.TypeReference
                yield! f.Analysed.TypeDefs.Keys |> Seq.map MetadataToken.TypeDefinition
                yield! f.Analysed.TypeSpecs.Keys |> Seq.map MetadataToken.TypeSpecification
            ]

        tokens
        |> List.map (fun token ->
            agrees
                state
                (fun state ->
                    let state, result =
                        IlMachineState.resolveTypeMetadataToken
                            f.LoggerFactory
                            f.BaseClassTypes
                            state
                            f.Analysed
                            token

                    state, typeOutcome result
                )
                (fun typeSystem ->
                    let typeSystem, result =
                        TypeSystemState.resolveTypeMetadataToken
                            f.LoggerFactory
                            dirs
                            f.BaseClassTypes
                            typeSystem
                            f.Analysed
                            token

                    typeSystem, typeOutcome result
                )
        )
        |> requireBoth

        let definitionOutcome (result : Result<TypeDefn, TypeBindingFailure>) : string =
            outcome TypeBindingFailure.describe (fun (defn : TypeDefn) -> $"%A{defn}") result

        [
            for handle in f.Analysed.TypeDefs.Keys do
                agrees
                    state
                    (fun state ->
                        let state, result =
                            IlMachineState.lookupTypeDefn f.LoggerFactory f.BaseClassTypes state f.Analysed handle

                        state, definitionOutcome result
                    )
                    (fun typeSystem ->
                        let typeSystem, result =
                            TypeSystemState.lookupTypeDefn
                                f.LoggerFactory
                                dirs
                                f.BaseClassTypes
                                typeSystem
                                f.Analysed
                                handle

                        typeSystem, definitionOutcome result
                    )
        ]
        |> requireBoth

    [<Test>]
    let ``a type reference token that does not bind stops the run naming the reference`` () : unit =
        let f = UnmodelledImage.fixture ()
        let token = MetadataToken.TypeReference f.Shapes.Absent

        let _, result =
            IlMachineState.resolveTypeMetadataToken f.LoggerFactory f.BaseClassTypes (machine f) f.Analysed token

        let message =
            try
                TokenBindingStop.typeToken f.Analysed token result
                |> ignore<TypeDefn * DumpedAssembly>

                failwith "expected the run to stop"
            with e ->
                e.Message

        message
        |> shouldEqual (
            $"Type reference Gone from %s{f.Analysed.DefinitionFullName} does not resolve: top-level type System.Gone is not declared in %s{f.BaseClassTypes.Corelib.DefinitionFullName}"
        )

    [<Test>]
    let ``every type reference of an image resolves in the interpreter as in the type system`` () : unit =
        let f = UnmodelledImage.fixture ()
        let state = machine f
        let dirs = HostImage.runtimeDirs

        [
            for handle in f.Analysed.TypeRefs.Keys do
                yield
                    agrees
                        state
                        (fun state ->
                            let state, result =
                                IlMachineState.lookupTypeRef f.LoggerFactory f.BaseClassTypes state f.Analysed handle

                            state, typeOutcome result
                        )
                        (fun typeSystem ->
                            let typeSystem, result =
                                TypeSystemState.lookupTypeRef
                                    f.LoggerFactory
                                    dirs
                                    f.BaseClassTypes
                                    typeSystem
                                    f.Analysed
                                    handle

                            typeSystem, typeOutcome result
                        )

                yield
                    agrees
                        state
                        (fun state ->
                            let state, result =
                                IlMachineState.resolveType f.LoggerFactory handle ImmutableArray.Empty f.Analysed state

                            state, typeInfoOutcome result
                        )
                        (fun typeSystem ->
                            let typeSystem, result =
                                TypeSystemState.resolveType
                                    f.LoggerFactory
                                    dirs
                                    handle
                                    ImmutableArray.Empty
                                    f.Analysed
                                    typeSystem

                            typeSystem, typeInfoOutcome result
                        )

                yield
                    agrees
                        state
                        (fun state ->
                            let state, result =
                                IlMachineState.resolveTypeFromRef
                                    f.LoggerFactory
                                    f.Analysed
                                    f.Analysed.TypeRefs.[handle]
                                    ImmutableArray.Empty
                                    state

                            state, typeInfoOutcome result
                        )
                        (fun typeSystem ->
                            let typeSystem, result =
                                TypeSystemState.resolveTypeFromRef
                                    f.LoggerFactory
                                    dirs
                                    f.Analysed
                                    f.Analysed.TypeRefs.[handle]
                                    ImmutableArray.Empty
                                    typeSystem

                            typeSystem, typeInfoOutcome result
                        )
        ]
        |> requireBoth

    [<Test>]
    let ``every type specification and base type of an image resolves in the interpreter as in the type system``
        ()
        : unit
        =
        let f = UnmodelledImage.fixture ()
        let state = machine f
        let dirs = HostImage.runtimeDirs

        let specs =
            [
                for handle in f.Analysed.TypeSpecs.Keys do
                    agrees
                        state
                        (fun state ->
                            let state, result =
                                IlMachineState.resolveTypeFromSpec
                                    f.LoggerFactory
                                    f.BaseClassTypes
                                    handle
                                    f.Analysed
                                    ImmutableArray.Empty
                                    ImmutableArray.Empty
                                    state

                            state, typeInfoOutcome result
                        )
                        (fun typeSystem ->
                            let typeSystem, result =
                                TypeSystemState.resolveTypeFromSpec
                                    f.LoggerFactory
                                    dirs
                                    f.BaseClassTypes
                                    handle
                                    f.Analysed
                                    ImmutableArray.Empty
                                    ImmutableArray.Empty
                                    typeSystem

                            typeSystem, typeInfoOutcome result
                        )
            ]

        requireBoth specs

        let baseOutcome (result : Result<DumpedAssembly * TypeDefn, TypeBindingFailure>) : string =
            outcome
                TypeBindingFailure.describe
                (fun (assy : DumpedAssembly, defn : TypeDefn) -> $"%A{defn} in %s{assy.DefinitionFullName}")
                result

        let bases =
            [
                for definition in f.Analysed.TypeDefs.Values do
                    match definition.BaseType with
                    | None -> ()
                    | Some baseType ->
                        agrees
                            state
                            (fun state ->
                                let state, result =
                                    IlMachineState.resolveBaseTypeInfo
                                        f.LoggerFactory
                                        f.BaseClassTypes
                                        state
                                        f.Analysed
                                        baseType

                                state, baseOutcome result
                            )
                            (fun typeSystem ->
                                let typeSystem, result =
                                    TypeSystemState.resolveBaseTypeInfo
                                        f.LoggerFactory
                                        dirs
                                        f.BaseClassTypes
                                        typeSystem
                                        f.Analysed
                                        baseType

                                typeSystem, baseOutcome result
                            )
            ]

        requireBoth bases

    [<Test>]
    let ``a type token whose binding loads an assembly before it fails keeps the load`` () : unit =
        let f = UnmodelledImage.fixture ()
        let state = machine f
        let runtime = "System.Runtime"

        let isRuntime (fullName : string) : bool =
            fullName.StartsWith (runtime + ",", System.StringComparison.Ordinal)

        state.TypeSystem._LoadedAssemblies.DefinitionNames
        |> Seq.exists isRuntime
        |> shouldEqual false

        let after, result =
            IlMachineState.resolveTypeMetadataToken
                f.LoggerFactory
                f.BaseClassTypes
                state
                f.Analysed
                (MetadataToken.TypeReference f.Shapes.FacadeAbsent)

        match result with
        | Error (TypeBindingFailure.TypeAbsent _) -> ()
        | other -> failwith $"expected System.Gone to be absent from System.Runtime, got %A{other}"

        after.TypeSystem._LoadedAssemblies.DefinitionNames
        |> Seq.exists isRuntime
        |> shouldEqual true

    [<Test>]
    let ``every member reference of an image binds in the interpreter as in the type system`` () : unit =
        let f = UnmodelledImage.fixture ()
        let state = machine f
        let dirs = HostImage.runtimeDirs

        let memberOutcome
            (handle : MemberReferenceHandle)
            (result :
                Result<
                    System.Reflection.AssemblyName *
                    Choice<
                        WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
                        FieldReferenceBinding
                     > *
                    TypeDefn ImmutableArray,
                    MemberBindingFailure
                 >)
            : string
            =
            outcome
                (MemberReferenceInstantiation.describeFailure f.Analysed handle)
                (fun
                    (declaring : System.Reflection.AssemblyName,
                     member' :
                         Choice<
                             WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
                             FieldReferenceBinding
                          >,
                     arguments : TypeDefn ImmutableArray) ->
                    let named =
                        match member' with
                        | Choice1Of2 method -> $"method %s{method.Name}"
                        | Choice2Of2 field -> $"field %A{field}"

                    $"%s{named} of %s{declaring.FullName} with %A{List.ofSeq arguments}"
                )
                result

        [
            for handle in f.Analysed.Members.Keys do
                agrees
                    state
                    (fun state ->
                        let state, result =
                            IlMachineMemberResolution.resolveMemberWithGenerics
                                f.LoggerFactory
                                f.BaseClassTypes
                                (ThreadId.ThreadId 0)
                                f.Analysed
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                handle
                                state

                        state, memberOutcome handle result
                    )
                    (fun typeSystem ->
                        let typeSystem, result =
                            MemberReferenceInstantiation.resolveMemberWithGenerics
                                f.LoggerFactory
                                dirs
                                f.BaseClassTypes
                                f.Analysed
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                handle
                                typeSystem

                        let result =
                            result
                            |> Result.map (fun bound ->
                                bound.DeclaringAssembly, bound.Member, bound.TargetTypeGenerics
                            )

                        typeSystem, memberOutcome handle result
                    )
        ]
        |> requireBoth

    [<Test>]
    let ``every method of an image concretises in the interpreter as in the type system`` () : unit =
        let f = UnmodelledImage.fixture ()
        let state = machine f
        let dirs = HostImage.runtimeDirs

        let methodOutcome
            (result :
                Result<
                    WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
                    ConcreteTypeHandle,
                    TypeBindingFailure
                 >)
            : string
            =
            outcome
                TypeBindingFailure.describe
                (fun (method : WoofWare.PawPrint.MethodInfo<_, _, _>, declaring) -> $"%s{method.Name} on %O{declaring}")
                result

        [
            for method in f.Analysed.Methods.Values do
                yield
                    agrees
                        state
                        (fun state ->
                            let state, result =
                                ExecutionConcretization.concretizeMethodWithTypeGenerics
                                    f.LoggerFactory
                                    f.BaseClassTypes
                                    ImmutableArray.Empty
                                    method
                                    None
                                    f.Analysed.DefinitionFullName
                                    ImmutableArray.Empty
                                    state

                            state, methodOutcome result
                        )
                        (fun typeSystem ->
                            let typeSystem, result =
                                MethodConcretisation.concretizeMethodWithTypeGenerics
                                    f.LoggerFactory
                                    dirs
                                    f.BaseClassTypes
                                    ImmutableArray.Empty
                                    method
                                    None
                                    f.Analysed.DefinitionFullName
                                    ImmutableArray.Empty
                                    typeSystem

                            typeSystem, methodOutcome result
                        )

                yield
                    agrees
                        state
                        (fun state ->
                            let state, result =
                                ExecutionConcretization.concretizeMethodWithAllGenerics
                                    f.LoggerFactory
                                    f.BaseClassTypes
                                    ImmutableArray.Empty
                                    method
                                    ImmutableArray.Empty
                                    state

                            state, methodOutcome result
                        )
                        (fun typeSystem ->
                            let typeSystem, result =
                                MethodConcretisation.concretizeMethodWithAllGenerics
                                    f.LoggerFactory
                                    dirs
                                    f.BaseClassTypes
                                    ImmutableArray.Empty
                                    method
                                    ImmutableArray.Empty
                                    typeSystem

                            typeSystem, methodOutcome result
                        )
        ]
        |> requireBoth

    [<Test>]
    let ``a field whose declaring type's base type is absent fails to concretise`` () : unit =
        let f = UnmodelledImage.fixture ()
        let state = machine f

        let _, result =
            ExecutionConcretization.concretizeFieldDeclaringType
                f.LoggerFactory
                f.BaseClassTypes
                ImmutableArray.Empty
                ImmutableArray.Empty
                (f.Analysed.Fields.[f.Shapes.DerivedCount]
                 |> FieldInfo.mapTypeGenerics (fun i _ -> TypeDefn.GenericTypeParameter i))
                state

        match result with
        | Error (TypeBindingFailure.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
            ()
        | other -> failwith $"expected Derived's base type System.Gone to be absent, got %A{other}"

    /// The machine with a thread executing `N.Holder.M`, so that what reads the executing frame's
    /// generic context and assembly has one.
    let private running (f : UnmodelledImage.Fixture) : IlMachineState * ThreadId =
        let state = machine f
        let holderM = f.Analysed.Methods.[MetadataTokens.MethodDefinitionHandle 1]

        let state, method, _ =
            match
                ExecutionConcretization.concretizeMethodWithAllGenerics
                    f.LoggerFactory
                    f.BaseClassTypes
                    ImmutableArray.Empty
                    holderM
                    ImmutableArray.Empty
                    state
            with
            | state, Ok (method, declaring) -> state, method, declaring
            | _, Error failure -> failwith $"N.Holder.M must concretise: %s{TypeBindingFailure.describe failure}"

        match
            MethodState.Empty
                state.TypeSystem.ConcreteTypes
                f.BaseClassTypes
                state.TypeSystem._LoadedAssemblies
                f.Analysed
                method
                ImmutableArray.Empty
                ImmutableArray.Empty
                None
        with
        | Ok frame -> IlMachineState.addThread frame state
        | Error missing -> failwith $"unexpected missing assembly references creating frame: %O{missing}"

    let private resolveMember
        (f : UnmodelledImage.Fixture)
        (use' : MemberReferenceUse)
        (m : MemberReferenceHandle)
        : Result<MemberReferenceResolution, MemberBindingFailure>
        =
        let state, thread = running f

        IlMachineMemberResolution.resolveMember use' f.LoggerFactory f.BaseClassTypes thread f.Analysed m state
        |> snd

    let private unmodelledMember (fragments : string list) (result : Result<'a, MemberBindingFailure>) : unit =
        match result with
        | Error (MemberBindingFailure.Unmodelled unmodelled) ->
            for fragment in fragments do
                unmodelled.Diagnostic |> shouldContainText fragment
        | Error other -> failwith $"expected an unmodelled member binding, got %A{other}"
        | Ok _ -> failwith "expected an unmodelled member binding, but it bound"

    [<Test>]
    let ``a member reference through a generic definition with no instantiation is unmodelled outside ldtoken``
        ()
        : unit
        =
        let f = UnmodelledImage.fixture ()

        resolveMember f (MemberReferenceUse.Other "call") f.Shapes.OpenListClear
        |> unmodelledMember
            [
                "call"
                "System.Collections.Generic.List`1"
                "TODO: raise TypeLoadException"
            ]

        match resolveMember f MemberReferenceUse.Ldtoken f.Shapes.OpenListClear with
        | Ok (MemberReferenceResolution.Resolved _) -> ()
        | Ok (MemberReferenceResolution.RefusedParent refusal) ->
            failwith $"expected ldtoken to name the member, got a refusal %A{refusal}"
        | Error failure ->
            failwith
                $"expected ldtoken to name the member, got %s{MemberReferenceInstantiation.describeFailure f.Analysed f.Shapes.OpenListClear failure}"

    [<Test>]
    let ``a member reference that binds no method is the resolution's failure`` () : unit =
        let f = UnmodelledImage.fixture ()

        match resolveMember f (MemberReferenceUse.Other "call") f.Shapes.NoSuchMethod with
        | Error MemberBindingFailure.NoSuchMethod -> ()
        | other -> failwith $"expected Object::NoSuch to bind no method, got %A{Result.isOk other}"

    [<Test>]
    let ``a method whose type arguments from metadata do not bind fails to concretise for execution`` () : unit =
        let f = UnmodelledImage.fixture ()
        let state, thread = running f
        let holderM = f.Analysed.Methods.[MetadataTokens.MethodDefinitionHandle 1]

        let gone =
            TypeDefn.FromReference (f.Analysed.TypeRefs.[f.Shapes.Absent], SignatureTypeKind.Class)

        let _, result =
            ExecutionConcretization.concretizeMethodForExecution
                f.LoggerFactory
                f.BaseClassTypes
                thread
                holderM
                None
                (Some (ImmutableArray.Create gone))
                state

        match result with
        | Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) -> ()
        | other -> failwith $"expected System.Gone to be absent, got %A{Result.isOk other}"

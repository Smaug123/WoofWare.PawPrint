namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The type system reports what a guest's metadata names and it cannot bind as a result, as the
/// Loader does: instantiating a type or a method, comparing signatures, and binding a MemberRef.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTypeSystemBindingFailure =

    let private context (f : UnmodelledImage.Fixture) : TypeConcretization.ConcretizationContext<DumpedAssembly> =
        {
            ConcreteTypes = AllConcreteTypes.Empty
            LoadedAssemblies = f.Assemblies
            BaseTypes = f.BaseClassTypes
        }

    let private loader (f : UnmodelledImage.Fixture) : IAssemblyLoad =
        TypeResolution.directoryLoader f.LoggerFactory HostImage.runtimeDirs

    let private spelled (f : UnmodelledImage.Fixture) (handle : TypeReferenceHandle) : TypeDefn =
        TypeDefn.FromReference (f.Analysed.TypeRefs.[handle], SignatureTypeKind.Class)

    let private concretize
        (f : UnmodelledImage.Fixture)
        (ty : TypeDefn)
        : Result<ConcreteTypeHandle, TypeBindingFailure>
        =
        TypeConcretization.concretizeType
            (context f)
            (loader f)
            f.Analysed.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            ty
        |> fst

    let private typeSystem (f : UnmodelledImage.Fixture) : TypeSystemState =
        { TypeSystemState.Empty with
            _LoadedAssemblies = f.Assemblies
        }

    let private int32 = TypeDefn.PrimitiveType PrimitiveType.Int32

    let private signature (parameters : TypeDefn list) : TypeMethodSignature<TypeDefn> =
        {
            Header =
                ComparableSignatureHeader.Make (
                    SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)
                )
            ParameterTypes = parameters
            GenericParameterCount = 0
            RequiredParameterCount = parameters.Length
            ReturnType = MethodReturnType.Void
        }

    let private genericFunctionPointer : TypeDefn =
        TypeDefn.FunctionPointer
            { signature [ int32 ] with
                Header =
                    ComparableSignatureHeader.Make (
                        SignatureHeader (
                            SignatureKind.Method,
                            SignatureCallingConvention.Default,
                            SignatureAttributes.Generic
                        )
                    )
                GenericParameterCount = 1
            }

    let private modified (ty : TypeDefn) (modifier : TypeDefn) : TypeDefn =
        TypeDefn.Modified
            {
                Unmodified = ty
                Modifier = modifier
                IsRequired = true
            }

    let private unmodelledSaying (fragment : string) (result : Result<'a, TypeBindingFailure>) : unit =
        match result with
        | Error (TypeBindingFailure.Unmodelled unmodelled) -> unmodelled.Diagnostic |> shouldContainText fragment
        | other -> failwith $"expected Unmodelled saying %s{fragment}, got %A{other}"

    let private isAbsentGone (result : Result<'a, TypeBindingFailure>) : unit =
        match result with
        | Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) -> ()
        | other -> failwith $"expected System.Gone to be absent, got %A{other}"

    let private isNowhere (result : Result<'a, TypeBindingFailure>) : unit =
        match result with
        | Error (TypeBindingFailure.AssemblyUnavailable reference) -> reference.Name.Name |> shouldEqual "Nowhere"
        | other -> failwith $"expected Nowhere to be unavailable, got %A{other}"

    [<Test>]
    let ``a type reference that does not bind is concretisation's failure`` () : unit =
        let f = UnmodelledImage.fixture ()

        concretize f (spelled f f.Shapes.Absent) |> isAbsentGone
        concretize f (spelled f f.Shapes.Unbound) |> isNowhere

        concretize f (spelled f f.Shapes.ModuleScoped)
        |> unmodelledSaying "ModuleRef type resolution is not yet supported for type N.Inner"

    [<Test>]
    let ``shapes concretisation does not implement are unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()
        let list = spelled f f.Shapes.List

        concretize f list |> unmodelledSaying "open generic type"

        concretize f genericFunctionPointer
        |> unmodelledSaying "GENERIC calling convention"

        concretize
            f
            (TypeDefn.GenericInstantiation (list, ImmutableArray.Create (modified int32 (spelled f f.Shapes.Object))))
        |> unmodelledSaying "carrying a custom modifier"

        let listDefinition =
            match
                TypeResolution.resolveTypeFromRef
                    f.LoggerFactory
                    HostImage.runtimeDirs
                    f.Analysed
                    f.Analysed.TypeRefs.[f.Shapes.List]
                    ImmutableArray.Empty
                    f.Assemblies
            with
            | _, Ok (_, info) -> info.Identity
            | _, Error failure -> failwith (TypeBindingFailure.describe failure)

        concretize f (TypeDefn.FromDefinition (listDefinition, SignatureTypeKind.Class))
        |> unmodelledSaying "open generic type"

        concretize f (TypeDefn.GenericInstantiation (int32, ImmutableArray.Create int32))
        |> unmodelledSaying "Generic instantiation of"

    [<Test>]
    let ``comparing signatures fails where a type it must bind does not`` () : unit =
        let f = UnmodelledImage.fixture ()

        let comparand (parameter : TypeDefn) : TypeConcretization.SignatureComparand =
            {
                Signature = signature [ parameter ]
                AssemblyFullName = f.Analysed.DefinitionFullName
                DeclaringTypeGenerics = TypeConcretization.SubstitutionContext.ofClosed ImmutableArray.Empty
            }

        TypeConcretization.signaturesEquivalent
            (context f)
            (loader f)
            false
            (comparand (spelled f f.Shapes.UnboundObject))
            (comparand (spelled f f.Shapes.Object))
        |> fst
        |> isNowhere

        TypeConcretization.signaturesEquivalent
            (context f)
            (loader f)
            false
            (comparand genericFunctionPointer)
            (comparand genericFunctionPointer)
        |> fst
        |> unmodelledSaying "GENERIC calling convention"

    [<Test>]
    let ``a comparison stops at the first unequal parameter, binding nothing after it`` () : unit =
        let f = UnmodelledImage.fixture ()

        let comparand (parameters : TypeDefn list) : TypeConcretization.SignatureComparand =
            {
                Signature = signature parameters
                AssemblyFullName = f.Analysed.DefinitionFullName
                DeclaringTypeGenerics = TypeConcretization.SubstitutionContext.ofClosed ImmutableArray.Empty
            }

        let object = spelled f f.Shapes.Object

        TypeConcretization.signaturesEquivalent
            (context f)
            (loader f)
            false
            (comparand [ int32 ; spelled f f.Shapes.UnboundObject ])
            (comparand [ object ; object ])
        |> fst
        |> shouldEqual (Ok false)

    [<Test>]
    let ``priming a type whose base type is absent fails`` () : unit =
        let f = UnmodelledImage.fixture ()
        let derived = f.Analysed.TypeDefs.[f.Shapes.Derived]

        let handle, ctx =
            TypeConcretization.concretizeTypeDefinition (context f) derived.Identity

        match
            Concretization.ensureBaseAssembliesLoadedForConcreteHandle
                (loader f)
                f.BaseClassTypes
                (System.Collections.Generic.HashSet ())
                ctx.LoadedAssemblies
                ctx.ConcreteTypes
                handle
        with
        | _,
          _,
          Some (TypeBindingFailure.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
            ()
        | _, _, other -> failwith $"expected the base type to be absent, got %A{other}"

    [<Test>]
    let ``a method reference whose parent's base type is absent names the absent type`` () : unit =
        let f = UnmodelledImage.fixture ()

        match
            MethodReferenceResolution.resolve
                f.LoggerFactory
                HostImage.runtimeDirs
                (context f)
                f.Analysed
                f.Shapes.DerivedRunReference
        with
        // Loading the parent reaches its base type, so the absent type is the parent's failure.
        | _, MethodReferenceTarget.ParentTypeMissing (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone")) ->
            ()
        | _, other -> failwith $"expected the parent's base type to be absent, got %A{other}"

    [<Test>]
    let ``a method whose signature or declaring type does not bind fails to concretise`` () : unit =
        let f = UnmodelledImage.fixture ()

        let concretizeMethod (handle : MethodDefinitionHandle) =
            let result, _, _ =
                Concretization.concretizeMethod
                    AllConcreteTypes.Empty
                    (loader f)
                    f.Assemblies
                    f.BaseClassTypes
                    f.Analysed.Methods.[handle]
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            result

        concretizeMethod f.Shapes.Takes |> isAbsentGone

        match concretizeMethod f.Shapes.DerivedRun with
        | Error (TypeBindingFailure.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
            ()
        | other -> failwith $"expected the base type to be absent, got %A{other}"

    [<Test>]
    let ``a type reference the type system cannot resolve is its failure`` () : unit =
        let f = UnmodelledImage.fixture ()

        TypeSystemState.resolveType
            f.LoggerFactory
            HostImage.runtimeDirs
            f.Shapes.Absent
            ImmutableArray.Empty
            f.Analysed
            (typeSystem f)
        |> snd
        |> isAbsentGone

        TypeSystemState.resolveTypeMetadataToken
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            (typeSystem f)
            f.Analysed
            (MetadataToken.TypeReference f.Shapes.Unbound)
        |> snd
        |> isNowhere

    [<Test>]
    let ``a method reference whose parent is a module reference is unbound`` () : unit =
        let f = UnmodelledImage.fixture ()

        match
            MethodReferenceResolution.resolve
                f.LoggerFactory
                HostImage.runtimeDirs
                (context f)
                f.Analysed
                f.Shapes.ModuleMethod
        with
        | _, MethodReferenceTarget.Unbound (TypeBindingFailure.Unmodelled unmodelled) ->
            unmodelled.Diagnostic
            |> shouldContainText "names a global member of another module, which is not modelled"
        | _, other -> failwith $"expected Unbound, got %A{other}"

    let private bindMember (f : UnmodelledImage.Fixture) (handle : MemberReferenceHandle) =
        MemberReferenceInstantiation.resolveMemberWithGenerics
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            f.Analysed
            ImmutableArray.Empty
            ImmutableArray.Empty
            handle
            (typeSystem f)
        |> snd

    [<Test>]
    let ``a member reference that binds nothing is the binding's failure`` () : unit =
        let f = UnmodelledImage.fixture ()

        match bindMember f f.Shapes.NoSuchMethod with
        | Error MemberBindingFailure.NoSuchMethod -> ()
        | other -> failwith $"expected NoSuchMethod, got %A{other}"

        match bindMember f f.Shapes.AbsentParentMethod with
        | Error (MemberBindingFailure.TypeUnbound failure) ->
            (Error failure : Result<unit, TypeBindingFailure>) |> isAbsentGone
        | other -> failwith $"expected TypeUnbound, got %A{other}"

        match bindMember f f.Shapes.UnboundParentField with
        | Error (MemberBindingFailure.TypeUnbound failure) ->
            (Error failure : Result<unit, TypeBindingFailure>) |> isNowhere
        | other -> failwith $"expected TypeUnbound, got %A{other}"

        match bindMember f f.Shapes.ModuleMethod with
        | Error (MemberBindingFailure.TypeUnbound (TypeBindingFailure.Unmodelled _)) -> ()
        | other -> failwith $"expected an unmodelled parent, got %A{other}"

        match bindMember f f.Shapes.MethodParent with
        | Error (MemberBindingFailure.Unmodelled unmodelled) ->
            unmodelled.Diagnostic
            |> shouldContainText "a field reference's parent is a MethodDef, which ECMA-335 does not permit"
        | other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``an instantiation whose argument is an uninstantiated generic definition is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()
        let fragment = "names the generic definition"

        TypeResolution.resolveTypeFromSpec
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            f.Shapes.ListOfBareList
            f.Analysed
            ImmutableArray.Empty
            ImmutableArray.Empty
            f.Assemblies
        |> snd
        |> unmodelledSaying fragment

        TypeSystemState.resolveTypeFromSpec
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            f.Shapes.ListOfBareList
            f.Analysed
            ImmutableArray.Empty
            ImmutableArray.Empty
            (typeSystem f)
        |> snd
        |> unmodelledSaying fragment

        // An environment has nothing to instantiate the bare argument at, however many arguments
        // it holds.
        TypeSystemState.resolveTypeFromSpec
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            f.Shapes.ListOfBareList
            f.Analysed
            (ImmutableArray.Create int32)
            ImmutableArray.Empty
            (typeSystem f)
        |> snd
        |> unmodelledSaying fragment

        match bindMember f f.Shapes.ListOfBareListClear with
        | Error (MemberBindingFailure.TypeUnbound failure) ->
            (Error failure : Result<unit, TypeBindingFailure>) |> unmodelledSaying fragment
        | other -> failwith $"expected TypeUnbound, got %A{other}"

    [<Test>]
    let ``a failure to bind a member reference names the row`` () : unit =
        let f = UnmodelledImage.fixture ()

        MemberReferenceInstantiation.describeFailure f.Analysed f.Shapes.NoSuchMethod MemberBindingFailure.NoSuchMethod
        |> shouldEqual
            $"MemberRef NoSuch (row %d{System.Reflection.Metadata.Ecma335.MetadataTokens.GetRowNumber (
                                           MemberReferenceHandle.op_Implicit f.Shapes.NoSuchMethod : EntityHandle
                                       )}) of %s{f.Analysed.DefinitionFullName} binds to no method, so CoreCLR throws MissingMethodException; that is not modelled"

    /// How a spelling fails to concretise, as the reference implementation below decides it.
    [<RequireQualifiedAccess>]
    type private Expected =
        | Absent
        | Unavailable
        | Unmodelled

    /// A leaf of a generated spelling: what it is, and how it fails to concretise if it does.
    type private Leaf =
        {
            Spelling : UnmodelledImage.Fixture -> TypeDefn
            Fails : Expected option
        }

    [<RequireQualifiedAccess>]
    type private Spelling =
        | Leaf of Leaf
        | Array of Spelling * rank : int
        | Vector of Spelling
        | Byref of Spelling
        | Pointer of Spelling
        | Pinned of Spelling
        | Modified of Spelling
        | ListOf of Spelling list
        | FunctionPointer of returns : Spelling option * parameters : Spelling list

    let private leaves : Leaf list =
        [
            {
                Spelling = fun _ -> int32
                Fails = None
            }
            {
                Spelling = fun f -> spelled f f.Shapes.Object
                Fails = None
            }
            {
                Spelling = fun f -> spelled f f.Shapes.Absent
                Fails = Some Expected.Absent
            }
            {
                Spelling = fun f -> spelled f f.Shapes.Unbound
                Fails = Some Expected.Unavailable
            }
            {
                Spelling = fun f -> spelled f f.Shapes.ModuleScoped
                Fails = Some Expected.Unmodelled
            }
            {
                // `List`1` named without an instantiation.
                Spelling = fun f -> spelled f f.Shapes.List
                Fails = Some Expected.Unmodelled
            }
            {
                Spelling = fun _ -> genericFunctionPointer
                Fails = Some Expected.Unmodelled
            }
        ]

    let rec private toTypeDefn (f : UnmodelledImage.Fixture) (spelling : Spelling) : TypeDefn =
        match spelling with
        | Spelling.Leaf leaf -> leaf.Spelling f
        | Spelling.Array (element, rank) -> TypeDefn.Array (toTypeDefn f element, rank)
        | Spelling.Vector element -> TypeDefn.OneDimensionalArrayLowerBoundZero (toTypeDefn f element)
        | Spelling.Byref element -> TypeDefn.Byref (toTypeDefn f element)
        | Spelling.Pointer element -> TypeDefn.Pointer (toTypeDefn f element)
        | Spelling.Pinned element -> TypeDefn.Pinned (toTypeDefn f element)
        | Spelling.Modified element -> modified (toTypeDefn f element) (spelled f f.Shapes.Object)
        | Spelling.ListOf arguments ->
            TypeDefn.GenericInstantiation (
                spelled f f.Shapes.List,
                arguments |> List.map (toTypeDefn f) |> ImmutableArray.CreateRange
            )
        | Spelling.FunctionPointer (returns, parameters) ->
            TypeDefn.FunctionPointer
                { signature (parameters |> List.map (toTypeDefn f)) with
                    ReturnType =
                        match returns with
                        | None -> MethodReturnType.Void
                        | Some returns -> MethodReturnType.Returns (toTypeDefn f returns)
                }

    let rec private carriesModifier (spelling : Spelling) : bool =
        match spelling with
        | Spelling.Leaf _ -> false
        | Spelling.Modified _ -> true
        | Spelling.Array (element, _)
        | Spelling.Vector element
        | Spelling.Byref element
        | Spelling.Pointer element
        | Spelling.Pinned element -> carriesModifier element
        | Spelling.ListOf arguments -> arguments |> List.exists carriesModifier
        | Spelling.FunctionPointer (returns, parameters) ->
            (returns |> Option.exists carriesModifier)
            || (parameters |> List.exists carriesModifier)

    /// The reference implementation: the first part of the spelling, in the order a runtime type is
    /// built from it, that does not bind. An instantiation is refused outright where an argument
    /// carries a custom modifier, before any argument is bound; otherwise its arguments come first,
    /// in order, and then the generic definition, which here always binds.
    let rec private expected (spelling : Spelling) : Expected option =
        let firstOf (parts : Spelling list) : Expected option = parts |> List.tryPick expected

        match spelling with
        | Spelling.Leaf leaf -> leaf.Fails
        | Spelling.Array (element, _)
        | Spelling.Vector element
        | Spelling.Byref element
        | Spelling.Pointer element
        | Spelling.Pinned element
        | Spelling.Modified element -> expected element
        | Spelling.ListOf arguments ->
            if arguments |> List.exists carriesModifier then
                Some Expected.Unmodelled
            else
                firstOf arguments
        | Spelling.FunctionPointer (returns, parameters) -> firstOf (Option.toList returns @ parameters)

    let private genSpelling : Gen<Spelling> =
        let leaf = Gen.elements leaves |> Gen.map Spelling.Leaf

        let rec go (size : int) : Gen<Spelling> =
            if size <= 0 then
                leaf
            else
                let smaller = go (size / 2)
                let several = Gen.choose (1, 3) |> Gen.bind (fun n -> Gen.listOfLength n smaller)

                Gen.frequency
                    [
                        3, leaf
                        1, Gen.map2 (fun element rank -> Spelling.Array (element, rank)) smaller (Gen.choose (1, 3))
                        1, Gen.map Spelling.Vector smaller
                        1, Gen.map Spelling.Byref smaller
                        1, Gen.map Spelling.Pointer smaller
                        1, Gen.map Spelling.Pinned smaller
                        1, Gen.map Spelling.Modified smaller
                        2, Gen.map Spelling.ListOf several
                        2,
                        Gen.map2
                            (fun returns parameters -> Spelling.FunctionPointer (returns, parameters))
                            (Gen.optionOf smaller)
                            (Gen.choose (0, 3) |> Gen.bind (fun n -> Gen.listOfLength n smaller))
                    ]

        Gen.sized (fun size -> go (min size 16))

    [<RequireQualifiedAccess>]
    type private Shown =
        | Binds
        | FailsAbsent
        | FailsUnavailable
        | FailsUnmodelled
        | RefusedForAModifier

    [<Test>]
    let ``concretisation fails exactly where the first part that does not bind is`` () : unit =
        let f = UnmodelledImage.fixture ()

        let property (cover : Shown -> unit) (spelling : Spelling) : bool =
            let actual = concretize f (toTypeDefn f spelling)

            match expected spelling, actual with
            | None, Ok _ -> cover Shown.Binds
            | Some Expected.Absent, Error (TypeBindingFailure.TypeAbsent _) -> cover Shown.FailsAbsent
            | Some Expected.Unavailable, Error (TypeBindingFailure.AssemblyUnavailable _) ->
                cover Shown.FailsUnavailable
            | Some Expected.Unmodelled, Error (TypeBindingFailure.Unmodelled unmodelled) ->
                if unmodelled.Diagnostic.Contains "custom modifier" then
                    cover Shown.RefusedForAModifier

                cover Shown.FailsUnmodelled
            | expected, actual -> failwith $"expected %A{expected}, got %A{actual}"

            true

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 1000) (Arb.fromGen genSpelling) property

        coverage.Count Shown.Binds |> shouldBeGreaterThan 100
        coverage.Count Shown.FailsAbsent |> shouldBeGreaterThan 100
        coverage.Count Shown.FailsUnavailable |> shouldBeGreaterThan 100
        coverage.Count Shown.FailsUnmodelled |> shouldBeGreaterThan 100
        coverage.Count Shown.RefusedForAModifier |> shouldBeGreaterThan 20

    [<RequireQualifiedAccess>]
    type private Resolved =
        | Binds
        | Fails
        | RefusedForABareArgument

    [<Test>]
    let ``the Loader resolves every spelling to a result rather than throwing`` () : unit =
        let f = UnmodelledImage.fixture ()

        let property (cover : Resolved -> unit) (spelling : Spelling) : bool =
            match
                TypeResolution.resolveTypeFromDefn
                    f.LoggerFactory
                    HostImage.runtimeDirs
                    f.BaseClassTypes
                    (toTypeDefn f spelling)
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    f.Analysed
                    f.Assemblies
                |> snd
            with
            | Ok _ -> cover Resolved.Binds
            | Error (TypeBindingFailure.Unmodelled unmodelled) when
                unmodelled.Diagnostic.Contains "names the generic definition"
                ->
                cover Resolved.RefusedForABareArgument
            | Error _ -> cover Resolved.Fails

            true

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 1000) (Arb.fromGen genSpelling) property

        coverage.Count Resolved.Binds |> shouldBeGreaterThan 100
        coverage.Count Resolved.Fails |> shouldBeGreaterThan 100
        coverage.Count Resolved.RefusedForABareArgument |> shouldBeGreaterThan 5

    [<RequireQualifiedAccess>]
    type private Agreement =
        | Binds
        | Fails

    /// The generated spellings mention no generic parameter, so the environment they are read in
    /// has nothing to bind in them: in particular, it cannot instantiate a generic definition one
    /// of them names bare.
    [<Test>]
    let ``the Loader resolves a spelling with no parameters alike in every environment`` () : unit =
        let f = UnmodelledImage.fixture ()
        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32
        let str = TypeDefn.PrimitiveType PrimitiveType.String

        let resolveIn (environment : TypeDefn list) (ty : TypeDefn) : Result<TypeDefn, TypeBindingFailure> =
            match
                TypeResolution.resolveTypeFromDefn
                    f.LoggerFactory
                    HostImage.runtimeDirs
                    f.BaseClassTypes
                    ty
                    (ImmutableArray.CreateRange environment)
                    ImmutableArray.Empty
                    f.Analysed
                    f.Assemblies
            with
            | assemblies, Ok (_, info) -> Ok (LoadedTypeInfo.typeInfoToTypeDefn f.BaseClassTypes assemblies info)
            | _, Error failure -> Error failure

        let property (cover : Agreement -> unit) (spelling : Spelling) : bool =
            let ty = toTypeDefn f spelling
            let inEmpty = resolveIn [] ty

            match inEmpty with
            | Ok _ -> cover Agreement.Binds
            | Error _ -> cover Agreement.Fails

            [ [ int32 ] ; [ int32 ; str ] ; [ str ; int32 ; int32 ] ]
            |> List.forall (fun environment -> resolveIn environment ty = inEmpty)

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 1000) (Arb.fromGen genSpelling) property

        coverage.Count Agreement.Binds |> shouldBeGreaterThan 100
        coverage.Count Agreement.Fails |> shouldBeGreaterThan 100

    [<Test>]
    let ``an instantiation of a type reference whose arguments do not match its parameters is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()
        let list = spelled f f.Shapes.List
        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32

        TypeResolution.resolveTypeFromDefn
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            (TypeDefn.GenericInstantiation (list, ImmutableArray.Create (int32, int32)))
            ImmutableArray.Empty
            ImmutableArray.Empty
            f.Analysed
            f.Assemblies
        |> snd
        |> unmodelledSaying "generic parameter"

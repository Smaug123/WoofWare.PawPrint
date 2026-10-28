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

    let private refusedAs (expected : TypeLoadRefusal -> bool) (result : Result<'a, TypeBindingFailure>) : unit =
        match result with
        | Error (TypeBindingFailure.Refused refusal) when expected refusal -> ()
        | other -> failwith $"expected a particular refusal, got %A{other}"

    [<Test>]
    let ``shapes concretisation does not implement are unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()
        let list = spelled f f.Shapes.List

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

        concretize f (TypeDefn.GenericInstantiation (int32, ImmutableArray.Create int32))
        |> unmodelledSaying "Generic instantiation of"

        // No signature can spell an instantiation with no arguments, even of a non-generic definition.
        concretize f (TypeDefn.GenericInstantiation (spelled f f.Shapes.Object, ImmutableArray.Empty))
        |> unmodelledSaying "names no arguments"

        // A generic definition named bare is refused, by reference and by definition alike.
        concretize f list
        |> refusedAs (
            function
            | TypeLoadRefusal.UninstantiatedDefinition definition -> definition = listDefinition
            | _ -> false
        )

        concretize f (TypeDefn.FromDefinition (listDefinition, SignatureTypeKind.Class))
        |> refusedAs (
            function
            | TypeLoadRefusal.UninstantiatedDefinition definition -> definition = listDefinition
            | _ -> false
        )

        // A non-generic definition given arguments is the wrong arity too.
        concretize f (TypeDefn.GenericInstantiation (spelled f f.Shapes.Object, ImmutableArray.Create int32))
        |> refusedAs (
            function
            | TypeLoadRefusal.WrongArity (_, 0, 1) -> true
            | _ -> false
        )

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
    let ``concretising a type whose base type is absent fails`` () : unit =
        let f = UnmodelledImage.fixture ()
        let derived = f.Analysed.TypeDefs.[f.Shapes.Derived]

        match TypeConcretization.concretizeTypeDefinition (loader f) (context f) derived.Identity with
        | Error (TypeBindingFailure.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))),
          _ -> ()
        | other, _ -> failwith $"expected the base type to be absent, got %A{other}"

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
    let ``a generic parameter outside its environment is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()

        let fromSpec (handle : TypeSpecificationHandle) (environment : TypeDefn list) =
            TypeResolution.resolveTypeFromSpec
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                handle
                f.Analysed
                (ImmutableArray.CreateRange environment)
                (ImmutableArray.CreateRange environment)
                f.Assemblies
            |> snd

        for handle, parameter in
            [
                f.Shapes.TypeParameter, "!0"
                f.Shapes.ListOfTypeParameter, "!0"
                f.Shapes.MethodParameter, "!!0"
                f.Shapes.ListOfMethodParameter, "!!0"
            ] do
            fromSpec handle []
            |> unmodelledSaying $"generic parameter %s{parameter} is out of range of an environment of 0 argument(s)"

            match fromSpec handle [ int32 ] with
            | Ok _ -> ()
            | Error failure ->
                failwith $"expected %s{parameter} to bind in [int32], got %s{TypeBindingFailure.describe failure}"

        TypeSystemState.resolveTypeFromSpec
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            f.Shapes.ListOfTypeParameter
            f.Analysed
            ImmutableArray.Empty
            ImmutableArray.Empty
            (typeSystem f)
        |> snd
        |> unmodelledSaying "BadImageFormatException"

        for handle in [ f.Shapes.ListOfTypeParameterClear ; f.Shapes.ListOfMethodParameterClear ] do
            match bindMember f handle with
            | Error (MemberBindingFailure.TypeUnbound failure) ->
                (Error failure : Result<unit, TypeBindingFailure>)
                |> unmodelledSaying "BadImageFormatException"
            | other -> failwith $"expected TypeUnbound, got %A{other}"

    [<Test>]
    let ``an array whose element type does not bind does not bind`` () : unit =
        let f = UnmodelledImage.fixture ()

        let fromSpec (handle : TypeSpecificationHandle) (environment : TypeDefn list) =
            TypeResolution.resolveTypeFromSpec
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                handle
                f.Analysed
                (ImmutableArray.CreateRange environment)
                ImmutableArray.Empty
                f.Assemblies
            |> snd

        fromSpec f.Shapes.AbsentVector [] |> isAbsentGone
        fromSpec f.Shapes.AbsentMatrix [] |> isAbsentGone
        fromSpec f.Shapes.UnboundVector [] |> isNowhere

        fromSpec f.Shapes.TypeParameterVector []
        |> unmodelledSaying "generic parameter !0 is out of range of an environment of 0 argument(s)"

        match fromSpec f.Shapes.TypeParameterVector [ int32 ] with
        | Ok (_, info) -> $"%s{info.Namespace}.%s{info.Name}" |> shouldEqual "System.Array"
        | Error failure -> failwith $"expected !0[] to bind in [int32], got %s{TypeBindingFailure.describe failure}"

        let resolve (ty : TypeDefn) =
            TypeResolution.resolveTypeFromDefn
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                ty
                ImmutableArray.Empty
                ImmutableArray.Empty
                f.Analysed
                f.Assemblies
            |> snd

        // The element is loaded through every wrapper CoreCLR loads it through.
        resolve (TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.Pointer (spelled f f.Shapes.Absent)))
        |> isAbsentGone

        resolve (
            TypeDefn.OneDimensionalArrayLowerBoundZero (
                TypeDefn.FunctionPointer
                    { signature [ int32 ; spelled f f.Shapes.Unbound ] with
                        ReturnType = MethodReturnType.Returns (spelled f f.Shapes.Absent)
                    }
            )
        )
        |> isAbsentGone

        resolve (TypeDefn.OneDimensionalArrayLowerBoundZero genericFunctionPointer)
        |> unmodelledSaying "BadImageFormatException"

        // CoreLib declares every primitive, `typedref` among them, so none stops an array binding.
        let typedref = TypeDefn.PrimitiveType PrimitiveType.TypedReference

        for element in
            [
                TypeDefn.Pointer typedref
                TypeDefn.FunctionPointer
                    { signature [ typedref ] with
                        ReturnType = MethodReturnType.Returns typedref
                    }
            ] do
            match resolve (TypeDefn.OneDimensionalArrayLowerBoundZero element) with
            | Ok _ -> ()
            | Error failure -> failwith $"expected %O{element}[] to bind, got %s{TypeBindingFailure.describe failure}"

        // An array of an instantiation binds exactly where the instantiation does, through a
        // function pointer in its argument too.
        let listOfFunctionPointers (parameter : TypeDefn) =
            TypeDefn.GenericInstantiation (
                spelled f f.Shapes.List,
                ImmutableArray.Create (
                    TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.FunctionPointer (signature [ parameter ]))
                )
            )

        for ty in
            [
                listOfFunctionPointers int32
                TypeDefn.OneDimensionalArrayLowerBoundZero (listOfFunctionPointers int32)
            ] do
            match resolve ty with
            | Ok _ -> ()
            | Error failure -> failwith $"expected %O{ty} to bind, got %s{TypeBindingFailure.describe failure}"

        resolve (listOfFunctionPointers (spelled f f.Shapes.Absent)) |> isAbsentGone

        resolve (TypeDefn.OneDimensionalArrayLowerBoundZero (listOfFunctionPointers (spelled f f.Shapes.Absent)))
        |> isAbsentGone

        match bindMember f f.Shapes.TypeParameterVectorToString with
        | Error (MemberBindingFailure.TypeUnbound failure) ->
            (Error failure : Result<unit, TypeBindingFailure>)
            |> unmodelledSaying "BadImageFormatException"
        | other -> failwith $"expected TypeUnbound, got %A{other}"

    let private resolveIn
        (f : UnmodelledImage.Fixture)
        (environment : TypeDefn list)
        (ty : TypeDefn)
        : LoadedAssemblies * Result<DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>, TypeBindingFailure>
        =
        TypeResolution.resolveTypeFromDefn
            f.LoggerFactory
            HostImage.runtimeDirs
            f.BaseClassTypes
            ty
            (ImmutableArray.CreateRange environment)
            ImmutableArray.Empty
            f.Analysed
            f.Assemblies

    /// `List<ty>`, as an instantiation spells it.
    let private listOf (f : UnmodelledImage.Fixture) (ty : TypeDefn) : TypeDefn =
        TypeDefn.GenericInstantiation (spelled f f.Shapes.List, ImmutableArray.Create ty)

    /// The argument `List<ty>` is instantiated at, once substituted.
    let private substitutedArgument
        (f : UnmodelledImage.Fixture)
        (environment : TypeDefn list)
        (ty : TypeDefn)
        : TypeDefn
        =
        match resolveIn f environment (listOf f ty) with
        | _, Ok (_, info) -> info.Generics |> Seq.exactlyOne
        | _, Error failure -> failwith $"expected List<%O{ty}> to bind, got %s{TypeBindingFailure.describe failure}"

    [<Test>]
    let ``substitution walks a function pointer, keeping its modifiers and its void return`` () : unit =
        let f = UnmodelledImage.fixture ()
        let bct = f.BaseClassTypes

        let named (info : TypeInfo<GenericParamFromMetadata, TypeDefn>) =
            LoadedTypeInfo.typeInfoToTypeDefn' bct f.Assemblies info

        let objectRef = spelled f f.Shapes.Object
        let typedref = TypeDefn.PrimitiveType PrimitiveType.TypedReference

        // The modifier comes back naming its definition, as every other type in the argument does.
        let boundModified (ty : TypeDefn) =
            TypeDefn.Modified
                {
                    Unmodified = ty
                    Modifier = named bct.Object
                    IsRequired = true
                }

        let spelledPointer =
            TypeDefn.FunctionPointer
                { signature [ modified int32 objectRef ; typedref ; TypeDefn.Pinned int32 ] with
                    ReturnType = MethodReturnType.Returns (modified TypeDefn.Void objectRef)
                }

        let substitutedPointer =
            TypeDefn.FunctionPointer
                { signature
                      [
                          boundModified (named bct.Int32)
                          named bct.TypedReference
                          TypeDefn.Pinned (named bct.Int32)
                      ] with
                    ReturnType = MethodReturnType.Returns (boundModified TypeDefn.Void)
                }

        substitutedArgument f [] (TypeDefn.OneDimensionalArrayLowerBoundZero spelledPointer)
        |> shouldEqual (TypeDefn.OneDimensionalArrayLowerBoundZero substitutedPointer)

        // A parameter is substituted inside one, as anywhere else.
        substitutedArgument
            f
            [ int32 ]
            (TypeDefn.OneDimensionalArrayLowerBoundZero (
                TypeDefn.FunctionPointer (signature [ TypeDefn.GenericTypeParameter 0 ])
            ))
        |> shouldEqual (
            TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.FunctionPointer (signature [ named bct.Int32 ]))
        )

        // A modifier on the argument itself is kept, and does not stop it binding.
        substitutedArgument f [] (modified int32 objectRef)
        |> shouldEqual (boundModified (named bct.Int32))

        // `typedref` names CoreLib's `System.TypedReference`, as every other primitive names its type.
        match resolveIn f [] typedref with
        | _, Ok (_, info) -> info.Identity |> shouldEqual bct.TypedReference.Identity
        | _, Error failure -> failwith $"expected typedref to bind, got %s{TypeBindingFailure.describe failure}"

    [<Test>]
    let ``substitution fails where the first part of a function pointer that does not bind is`` () : unit =
        let f = UnmodelledImage.fixture ()
        let absent = spelled f f.Shapes.Absent
        let unbound = spelled f f.Shapes.Unbound

        let vectorOfPointer (returns : TypeDefn) (parameters : TypeDefn list) =
            TypeDefn.OneDimensionalArrayLowerBoundZero (
                TypeDefn.FunctionPointer
                    { signature parameters with
                        ReturnType = MethodReturnType.Returns returns
                    }
            )

        let resolve (ty : TypeDefn) = resolveIn f [] (listOf f ty) |> snd

        // CoreCLR loads the return type first, then each parameter in turn.
        resolve (vectorOfPointer absent [ unbound ]) |> isAbsentGone
        resolve (vectorOfPointer unbound [ absent ]) |> isNowhere
        resolve (vectorOfPointer int32 [ int32 ; absent ; unbound ]) |> isAbsentGone

        resolve (TypeDefn.OneDimensionalArrayLowerBoundZero genericFunctionPointer)
        |> unmodelledSaying "BadImageFormatException"

        // CoreCLR never loads a custom modifier, so one that does not bind is unmodelled, not
        // absent; and the type it modifies is loaded, and fails, first.
        for modifier in [ absent ; unbound ] do
            resolve (TypeDefn.OneDimensionalArrayLowerBoundZero (modified int32 modifier))
            |> unmodelledSaying "custom modifier"

        resolve (TypeDefn.OneDimensionalArrayLowerBoundZero (modified unbound absent))
        |> isNowhere

    [<Test>]
    let ``an instantiation at a byref, pointer, function pointer, typedref or void is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()
        let bct = f.BaseClassTypes
        let functionPointer = TypeDefn.FunctionPointer (signature [ int32 ])

        let malformed =
            [
                TypeDefn.Byref int32
                TypeDefn.Pointer int32
                functionPointer
                TypeDefn.PrimitiveType PrimitiveType.TypedReference
                TypeDefn.Void
                TypeDefn.FromDefinition (bct.TypedReference.Identity, SignatureTypeKind.ValueType)
                TypeDefn.FromDefinition (bct.Void.Identity, SignatureTypeKind.ValueType)
                // CoreCLR reads the argument's element type past its modifiers and `pinned`.
                modified (TypeDefn.Pointer int32) (spelled f f.Shapes.Object)
                TypeDefn.Pinned (TypeDefn.Byref int32)
            ]

        for argument in malformed do
            resolveIn f [] (listOf f argument)
            |> snd
            |> unmodelledSaying "IDS_CLASSLOAD_INVALIDINSTANTIATION"

        // The argument is checked once substituted.
        resolveIn f [ TypeDefn.Pointer int32 ] (listOf f (TypeDefn.GenericTypeParameter 0))
        |> snd
        |> unmodelledSaying "IDS_CLASSLOAD_INVALIDINSTANTIATION"

        // Only the argument's own element type counts.
        for argument in
            [
                TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.Pointer int32)
                TypeDefn.Array (functionPointer, 2)
                listOf f (TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.Byref int32))
                modified int32 (spelled f f.Shapes.Object)
            ] do
            match resolveIn f [] (listOf f argument) |> snd with
            | Ok _ -> ()
            | Error failure ->
                failwith $"expected List<%O{argument}> to bind, got %s{TypeBindingFailure.describe failure}"

    /// One layer a generated generic parameter is spelled inside.
    [<RequireQualifiedAccess>]
    type private Wrapper =
        | InList
        | InVector
        | InMatrix

    /// A generated spelling of a generic parameter: which one, and what it sits inside, innermost
    /// first.
    type private ParameterSpelling =
        {
            IsMethod : bool
            Index : int
            Wrappers : Wrapper list
        }

    [<RequireQualifiedAccess>]
    type private InRange =
        | Binds
        | OutOfRange

    let private parameterSpellingGen : Gen<ParameterSpelling * int * int> =
        gen {
            let! isMethod = Gen.elements [ true ; false ]
            let! index = Gen.choose (0, 3)

            let! wrappers =
                Gen.choose (0, 2)
                |> Gen.bind (fun n ->
                    Gen.listOfLength n (Gen.elements [ Wrapper.InList ; Wrapper.InVector ; Wrapper.InMatrix ])
                )

            let! typeArgs = Gen.choose (0, 3)
            let! methodArgs = Gen.choose (0, 3)

            return
                {
                    IsMethod = isMethod
                    Index = index
                    Wrappers = wrappers
                },
                typeArgs,
                methodArgs
        }

    /// The parameter `spelling` names, inside the wrappers it lists.
    let private spellParameter (f : UnmodelledImage.Fixture) (spelling : ParameterSpelling) : TypeDefn =
        let parameter =
            if spelling.IsMethod then
                TypeDefn.GenericMethodParameter spelling.Index
            else
                TypeDefn.GenericTypeParameter spelling.Index

        spelling.Wrappers
        |> List.fold
            (fun inner wrapper ->
                match wrapper with
                | Wrapper.InList ->
                    TypeDefn.GenericInstantiation (spelled f f.Shapes.List, ImmutableArray.Create inner)
                | Wrapper.InVector -> TypeDefn.OneDimensionalArrayLowerBoundZero inner
                | Wrapper.InMatrix -> TypeDefn.Array (inner, 2)
            )
            parameter

    /// Whether `failure` is how the Loader reports `spelling` out of an environment of `size`: as
    /// unmodelled, naming the parameter.
    let private loaderOutOfRange (spelling : ParameterSpelling) (size : int) (failure : TypeBindingFailure) : bool =
        let name =
            if spelling.IsMethod then
                $"!!%d{spelling.Index}"
            else
                $"!%d{spelling.Index}"

        match failure with
        | TypeBindingFailure.Unmodelled unmodelled ->
            unmodelled.Diagnostic.Contains
                $"generic parameter %s{name} is out of range of an environment of %d{size} argument(s)"
        | _ -> false

    /// Whether `failure` is how concretisation reports `spelling` out of an environment of `size`: as
    /// CoreCLR's refusal of the signature.
    let private concretisationOutOfRange
        (spelling : ParameterSpelling)
        (size : int)
        (failure : TypeBindingFailure)
        : bool
        =
        let expected =
            if spelling.IsMethod then
                TypeLoadRefusal.MethodParameterOutOfScope (spelling.Index, size)
            else
                TypeLoadRefusal.TypeParameterOutOfScope (spelling.Index, size)

        failure = TypeBindingFailure.Refused expected

    /// Classify `result` as binding or out of range, failing unless that is what an environment of
    /// `typeArgs` and `methodArgs` arguments makes of `spelling`; `outOfRange` says how the side
    /// under test reports a parameter out of range.
    let private checkInRange
        (outOfRange : ParameterSpelling -> int -> TypeBindingFailure -> bool)
        (spelling : ParameterSpelling)
        (typeArgs : int)
        (methodArgs : int)
        (result : Result<'a, TypeBindingFailure>)
        : InRange
        =
        let size = if spelling.IsMethod then methodArgs else typeArgs

        match result with
        | Ok _ when spelling.Index < size -> InRange.Binds
        | Error failure when spelling.Index >= size && outOfRange spelling size failure -> InRange.OutOfRange
        | other -> failwith $"%A{spelling} in (%d{typeArgs}, %d{methodArgs}): got %A{other}"

    [<Test>]
    let ``the Loader binds a generic parameter exactly when its environment holds it`` () : unit =
        let f = UnmodelledImage.fixture ()

        let property (cover : InRange -> unit) (spelling : ParameterSpelling, typeArgs : int, methodArgs : int) : bool =
            TypeResolution.resolveTypeFromDefn
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                (spellParameter f spelling)
                (ImmutableArray.CreateRange (List.replicate typeArgs int32))
                (ImmutableArray.CreateRange (List.replicate methodArgs int32))
                f.Analysed
                f.Assemblies
            |> snd
            |> checkInRange loaderOutOfRange spelling typeArgs methodArgs
            |> cover

            true

        let coverage =
            CoverageSample.check
                (Config.QuickThrowOnFailure.WithMaxTest 500)
                (Arb.fromGen parameterSpellingGen)
                property

        coverage.Count InRange.Binds |> shouldBeGreaterThan 100
        coverage.Count InRange.OutOfRange |> shouldBeGreaterThan 100

    /// `ty` concretised in an environment of `typeArgs` and `methodArgs` copies of `int`.
    let private concretizeIn
        (f : UnmodelledImage.Fixture)
        (typeArgs : int)
        (methodArgs : int)
        (ty : TypeDefn)
        : Result<ConcreteTypeHandle, TypeBindingFailure>
        =
        let ctx = context f
        let assembly = f.Analysed.DefinitionFullName

        match
            TypeConcretization.concretizeType ctx (loader f) assembly ImmutableArray.Empty ImmutableArray.Empty int32
        with
        | Error failure, _ -> failwith $"int did not concretise: %s{TypeBindingFailure.describe failure}"
        | Ok intHandle, ctx ->
            TypeConcretization.concretizeType
                ctx
                (loader f)
                assembly
                (ImmutableArray.CreateRange (List.replicate typeArgs intHandle))
                (ImmutableArray.CreateRange (List.replicate methodArgs intHandle))
                ty
            |> fst

    [<Test>]
    let ``concretising a TypeSpec's generic parameter outside its environment is refused`` () : unit =
        let f = UnmodelledImage.fixture ()

        for handle, parameter, refusal in
            [
                f.Shapes.TypeParameter, "!0", TypeLoadRefusal.TypeParameterOutOfScope (0, 0)
                f.Shapes.ListOfTypeParameter, "!0", TypeLoadRefusal.TypeParameterOutOfScope (0, 0)
                f.Shapes.MethodParameter, "!!0", TypeLoadRefusal.MethodParameterOutOfScope (0, 0)
                f.Shapes.ListOfMethodParameter, "!!0", TypeLoadRefusal.MethodParameterOutOfScope (0, 0)
            ] do
            let signature = f.Analysed.TypeSpecs.[handle].Signature

            concretizeIn f 0 0 signature
            |> shouldEqual (Error (TypeBindingFailure.Refused refusal))

            match concretizeIn f 1 1 signature with
            | Ok _ -> ()
            | Error failure ->
                failwith $"expected %s{parameter} to bind in [int32], got %s{TypeBindingFailure.describe failure}"

    [<Test>]
    let ``concretisation binds a generic parameter exactly when its environment holds it`` () : unit =
        let f = UnmodelledImage.fixture ()

        let property (cover : InRange -> unit) (spelling : ParameterSpelling, typeArgs : int, methodArgs : int) : bool =
            concretizeIn f typeArgs methodArgs (spellParameter f spelling)
            |> checkInRange concretisationOutOfRange spelling typeArgs methodArgs
            |> cover

            true

        let coverage =
            CoverageSample.check
                (Config.QuickThrowOnFailure.WithMaxTest 500)
                (Arb.fromGen parameterSpellingGen)
                property

        coverage.Count InRange.Binds |> shouldBeGreaterThan 100
        coverage.Count InRange.OutOfRange |> shouldBeGreaterThan 100

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
        | RefusedBare
        | RefusedArity
        | RefusedArgument

    /// A leaf of a generated spelling: what it is, and how it fails to concretise if it does.
    type private Leaf =
        {
            Spelling : UnmodelledImage.Fixture -> TypeDefn
            Fails : Expected option
            /// A generic definition named without an instantiation, which the Loader binds as the open
            /// definition except as an instantiation's argument or inside one.
            BareDefinition : bool
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
                BareDefinition = false
            }
            {
                Spelling = fun f -> spelled f f.Shapes.Object
                Fails = None
                BareDefinition = false
            }
            {
                Spelling = fun f -> spelled f f.Shapes.Absent
                Fails = Some Expected.Absent
                BareDefinition = false
            }
            {
                Spelling = fun f -> spelled f f.Shapes.Unbound
                Fails = Some Expected.Unavailable
                BareDefinition = false
            }
            {
                Spelling = fun f -> spelled f f.Shapes.ModuleScoped
                Fails = Some Expected.Unmodelled
                BareDefinition = false
            }
            {
                // `List`1` named without an instantiation.
                Spelling = fun f -> spelled f f.Shapes.List
                Fails = Some Expected.RefusedBare
                BareDefinition = true
            }
            {
                Spelling = fun _ -> genericFunctionPointer
                Fails = Some Expected.Unmodelled
                BareDefinition = false
            }
            {
                Spelling = fun _ -> TypeDefn.PrimitiveType PrimitiveType.TypedReference
                Fails = None
                BareDefinition = false
            }
            {
                Spelling = fun _ -> TypeDefn.Void
                Fails = None
                BareDefinition = false
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

    /// Whether CoreCLR's `Generics::CheckInstantiation` refuses this spelling as an instantiation's
    /// argument: by its element type, read past custom modifiers and `pinned`.
    let rec private malformedArgument (f : UnmodelledImage.Fixture) (spelling : Spelling) : bool =
        match spelling with
        | Spelling.Leaf leaf ->
            match leaf.Spelling f with
            | TypeDefn.PrimitiveType PrimitiveType.TypedReference
            | TypeDefn.Void -> true
            | _ -> false
        | Spelling.Byref _
        | Spelling.Pointer _
        | Spelling.FunctionPointer _ -> true
        | Spelling.Pinned element
        | Spelling.Modified element -> malformedArgument f element
        | Spelling.Array _
        | Spelling.Vector _
        | Spelling.ListOf _ -> false

    /// The reference implementation: the first part of the spelling, in the order a runtime type is
    /// built from it, that does not bind. An instantiation is refused outright where an argument
    /// carries a custom modifier, before any argument is bound; otherwise the generic definition,
    /// which here always binds and is spelled as the class it is, comes first, then its arguments
    /// in order, then the arity (its one generic parameter), then the kinds of the arguments.
    let rec private expected (f : UnmodelledImage.Fixture) (spelling : Spelling) : Expected option =
        let firstOf (parts : Spelling list) : Expected option = parts |> List.tryPick (expected f)

        match spelling with
        | Spelling.Leaf leaf -> leaf.Fails
        | Spelling.Array (element, _)
        | Spelling.Vector element
        | Spelling.Byref element
        | Spelling.Pointer element
        | Spelling.Pinned element
        | Spelling.Modified element -> expected f element
        | Spelling.ListOf arguments ->
            if arguments |> List.exists carriesModifier then
                Some Expected.Unmodelled
            else
                match firstOf arguments with
                | Some failure -> Some failure
                | None when arguments.IsEmpty -> Some Expected.Unmodelled
                | None when arguments.Length <> 1 -> Some Expected.RefusedArity
                | None when arguments |> List.exists (malformedArgument f) -> Some Expected.RefusedArgument
                | None -> None
        | Spelling.FunctionPointer (returns, parameters) -> firstOf (Option.toList returns @ parameters)

    /// Spellings built over these leaves.
    let private genSpellingOver (leaves : Leaf list) : Gen<Spelling> =
        let leaf = Gen.elements leaves |> Gen.map Spelling.Leaf

        // An instantiation at the wrong arity is refused only once every argument binds, which
        // arbitrary arguments rarely all do.
        let wrongArity =
            Gen.elements [ 0 ; 2 ; 3 ]
            |> Gen.bind (fun n ->
                Gen.listOfLength n (leaves |> List.filter (fun leaf -> leaf.Fails.IsNone) |> Gen.elements)
            )
            |> Gen.map (List.map Spelling.Leaf >> Spelling.ListOf)

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
                        1, wrongArity
                        2,
                        Gen.map2
                            (fun returns parameters -> Spelling.FunctionPointer (returns, parameters))
                            (Gen.optionOf smaller)
                            (Gen.choose (0, 3) |> Gen.bind (fun n -> Gen.listOfLength n smaller))
                    ]

        Gen.sized (fun size -> go (min size 16))

    let private genSpelling : Gen<Spelling> = genSpellingOver leaves

    /// Spellings every part of which binds as a type on its own, though not necessarily in the
    /// position it is spelled in.
    let private genBindingParts : Gen<Spelling> =
        genSpellingOver (leaves |> List.filter (fun leaf -> leaf.Fails.IsNone))

    /// `genSpelling`, now and then an instantiation at an argument no instantiation may have, which
    /// is refused only once everything else binds, and which arbitrary spellings rarely reach.
    let private genSpellingWithMalformedArguments : Gen<Spelling> =
        let malformedArgument =
            leaves
            |> List.filter (fun leaf -> leaf.Fails.IsNone)
            |> Gen.elements
            |> Gen.map Spelling.Leaf
            |> Gen.bind (fun leaf ->
                Gen.elements
                    [
                        Spelling.Byref leaf
                        Spelling.Pointer leaf
                        Spelling.Pinned (Spelling.Pointer leaf)
                        Spelling.FunctionPointer (Some leaf, [])
                    ]
            )
            |> Gen.map (fun argument -> Spelling.ListOf [ argument ])

        Gen.frequency [ 12, genSpelling ; 1, malformedArgument ]

    [<RequireQualifiedAccess>]
    type private Shown =
        | Binds
        | FailsAbsent
        | FailsUnavailable
        | FailsUnmodelled
        | RefusedForAModifier
        | RefusedForArity
        | RefusedBare
        | RefusedArgument

    [<Test>]
    let ``concretisation fails exactly where the first part that does not bind is`` () : unit =
        let f = UnmodelledImage.fixture ()

        let property (cover : Shown -> unit) (spelling : Spelling) : bool =
            let actual = concretize f (toTypeDefn f spelling)

            match expected f spelling, actual with
            | None, Ok _ -> cover Shown.Binds
            | Some Expected.Absent, Error (TypeBindingFailure.TypeAbsent _) -> cover Shown.FailsAbsent
            | Some Expected.Unavailable, Error (TypeBindingFailure.AssemblyUnavailable _) ->
                cover Shown.FailsUnavailable
            | Some Expected.Unmodelled, Error (TypeBindingFailure.Unmodelled unmodelled) ->
                if unmodelled.Diagnostic.Contains "custom modifier" then
                    cover Shown.RefusedForAModifier

                cover Shown.FailsUnmodelled
            | Some Expected.RefusedArity, Error (TypeBindingFailure.Refused (TypeLoadRefusal.WrongArity (_, 1, _))) ->
                cover Shown.RefusedForArity
            | Some Expected.RefusedBare, Error (TypeBindingFailure.Refused (TypeLoadRefusal.UninstantiatedDefinition _)) ->
                cover Shown.RefusedBare
            | Some Expected.RefusedArgument, Error (TypeBindingFailure.Refused (TypeLoadRefusal.InvalidInstantiation _)) ->
                cover Shown.RefusedArgument
            | expected, actual -> failwith $"expected %A{expected}, got %A{actual}"

            true

        let coverage =
            CoverageSample.check
                (Config.QuickThrowOnFailure.WithMaxTest 1200)
                (Arb.fromGen genSpellingWithMalformedArguments)
                property

        coverage.Count Shown.Binds |> shouldBeGreaterThan 100
        coverage.Count Shown.FailsAbsent |> shouldBeGreaterThan 100
        coverage.Count Shown.FailsUnavailable |> shouldBeGreaterThan 100
        coverage.Count Shown.FailsUnmodelled |> shouldBeGreaterThan 100
        coverage.Count Shown.RefusedForAModifier |> shouldBeGreaterThan 20
        coverage.Count Shown.RefusedForArity |> shouldBeGreaterThan 20
        coverage.Count Shown.RefusedBare |> shouldBeGreaterThan 20
        coverage.Count Shown.RefusedArgument |> shouldBeGreaterThan 20

    /// Where in a spelling the Loader reads a part, which decides how it binds that part.
    [<RequireQualifiedAccess>]
    type private Position =
        /// The spelling itself, which the Loader must answer with one `TypeInfo`.
        | Whole
        /// Inside an array the spelling names, which the Loader walks only to load what it names.
        | ArrayElement
        /// Inside an instantiation's argument, which the Loader substitutes and keeps.
        | Argument

    /// The reference implementation of the Loader: the first part of the spelling that does not
    /// bind, as CoreCLR loads the parts: an array's element before the array, a function pointer's
    /// return type before its parameters, and an instantiation's arguments, in order, before the
    /// generic definition, which here always binds but refuses any number of arguments other than its
    /// one generic parameter, and then refuses an argument `Generics::CheckInstantiation` does.
    let rec private loaderExpected
        (f : UnmodelledImage.Fixture)
        (position : Position)
        (spelling : Spelling)
        : Expected option
        =
        let firstOf (position : Position) (parts : Spelling list) : Expected option =
            parts |> List.tryPick (loaderExpected f position)

        match spelling with
        | Spelling.Leaf leaf when leaf.BareDefinition ->
            match position with
            | Position.Argument -> Some Expected.Unmodelled
            | Position.Whole
            | Position.ArrayElement -> None
        | Spelling.Leaf leaf -> leaf.Fails
        | Spelling.Array (element, _)
        | Spelling.Vector element ->
            match position with
            | Position.Whole -> loaderExpected f Position.ArrayElement element
            | Position.ArrayElement
            | Position.Argument -> loaderExpected f position element
        // A `TypeInfo` cannot say what any of these is.
        | Spelling.Byref _
        | Spelling.Pointer _
        | Spelling.Pinned _
        | Spelling.Modified _
        | Spelling.FunctionPointer _ when position = Position.Whole -> Some Expected.Unmodelled
        | Spelling.Byref element
        | Spelling.Pointer element
        | Spelling.Pinned element
        | Spelling.Modified element -> loaderExpected f position element
        | Spelling.FunctionPointer (returns, parameters) -> firstOf position (Option.toList returns @ parameters)
        | Spelling.ListOf arguments ->
            match firstOf Position.Argument arguments with
            | Some failure -> Some failure
            | None when arguments.Length <> 1 -> Some Expected.Unmodelled
            | None when malformedArgument f arguments.Head -> Some Expected.Unmodelled
            | None -> None

    let rec private mentionsFunctionPointer (spelling : Spelling) : bool =
        match spelling with
        | Spelling.Leaf _ -> false
        | Spelling.FunctionPointer _ -> true
        | Spelling.Array (element, _)
        | Spelling.Vector element
        | Spelling.Byref element
        | Spelling.Pointer element
        | Spelling.Pinned element
        | Spelling.Modified element -> mentionsFunctionPointer element
        | Spelling.ListOf arguments -> arguments |> List.exists mentionsFunctionPointer

    let rec private instantiatesOverFunctionPointer (spelling : Spelling) : bool =
        match spelling with
        | Spelling.Leaf _ -> false
        | Spelling.Array (element, _)
        | Spelling.Vector element
        | Spelling.Byref element
        | Spelling.Pointer element
        | Spelling.Pinned element
        | Spelling.Modified element -> instantiatesOverFunctionPointer element
        | Spelling.FunctionPointer (returns, parameters) ->
            Option.toList returns @ parameters
            |> List.exists instantiatesOverFunctionPointer
        | Spelling.ListOf arguments ->
            arguments
            |> List.exists (fun argument ->
                mentionsFunctionPointer argument || instantiatesOverFunctionPointer argument
            )

    [<RequireQualifiedAccess>]
    type private Resolved =
        | Binds
        | BindsAnInstantiationOverAFunctionPointer
        | FailsAbsent
        | FailsUnavailable
        | FailsUnmodelled
        | RefusedForABareArgument
        | RefusedForAMalformedArgument

    [<Test>]
    let ``the Loader fails exactly where the first part that does not bind is`` () : unit =
        let f = UnmodelledImage.fixture ()

        let property (cover : Resolved -> unit) (spelling : Spelling) : bool =
            match loaderExpected f Position.Whole spelling, resolveIn f [] (toTypeDefn f spelling) |> snd with
            | None, Ok _ ->
                if instantiatesOverFunctionPointer spelling then
                    cover Resolved.BindsAnInstantiationOverAFunctionPointer

                cover Resolved.Binds
            | Some Expected.Absent, Error (TypeBindingFailure.TypeAbsent _) -> cover Resolved.FailsAbsent
            | Some Expected.Unavailable, Error (TypeBindingFailure.AssemblyUnavailable _) ->
                cover Resolved.FailsUnavailable
            | Some Expected.Unmodelled, Error (TypeBindingFailure.Unmodelled unmodelled) ->
                if unmodelled.Diagnostic.Contains "names the generic definition" then
                    cover Resolved.RefusedForABareArgument

                if unmodelled.Diagnostic.Contains "IDS_CLASSLOAD_INVALIDINSTANTIATION" then
                    cover Resolved.RefusedForAMalformedArgument

                cover Resolved.FailsUnmodelled
            | expected, actual -> failwith $"expected %A{expected}, got %A{actual}"

            true

        // An instantiation over a function pointer binds only where every part of it does, which
        // arbitrary spellings rarely all do.
        let gen =
            Gen.frequency
                [
                    2, genSpelling
                    1, Gen.map (fun argument -> Spelling.ListOf [ argument ]) genSpelling
                    1, Gen.map (fun element -> Spelling.ListOf [ Spelling.Vector element ]) genBindingParts
                ]

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 2000) (Arb.fromGen gen) property

        coverage.Count Resolved.Binds |> shouldBeGreaterThan 100

        coverage.Count Resolved.BindsAnInstantiationOverAFunctionPointer
        |> shouldBeGreaterThan 20

        coverage.Count Resolved.FailsAbsent |> shouldBeGreaterThan 100
        coverage.Count Resolved.FailsUnavailable |> shouldBeGreaterThan 100
        coverage.Count Resolved.FailsUnmodelled |> shouldBeGreaterThan 100
        coverage.Count Resolved.RefusedForABareArgument |> shouldBeGreaterThan 5
        coverage.Count Resolved.RefusedForAMalformedArgument |> shouldBeGreaterThan 20

    /// The type with every custom modifier inside it deleted.
    let rec private withoutModifiers (ty : TypeDefn) : TypeDefn =
        match ty with
        | TypeDefn.Modified modified -> withoutModifiers modified.Unmodified
        | TypeDefn.Array (element, rank) -> TypeDefn.Array (withoutModifiers element, rank)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
            TypeDefn.OneDimensionalArrayLowerBoundZero (withoutModifiers element)
        | TypeDefn.Pinned element -> TypeDefn.Pinned (withoutModifiers element)
        | TypeDefn.Pointer element -> TypeDefn.Pointer (withoutModifiers element)
        | TypeDefn.Byref element -> TypeDefn.Byref (withoutModifiers element)
        | TypeDefn.GenericInstantiation (generic, args) ->
            TypeDefn.GenericInstantiation (
                withoutModifiers generic,
                args |> Seq.map withoutModifiers |> ImmutableArray.CreateRange
            )
        | TypeDefn.FunctionPointer signature ->
            TypeDefn.FunctionPointer (
                TypeMethodSignature.map () (fun () ty -> (), withoutModifiers ty) signature
                |> snd
            )
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> ty

    [<RequireQualifiedAccess>]
    type private ArgumentVerdict =
        | Admitted
        | AdmittedOverAFunctionPointer
        | Refused
        | NotAnArgument

    /// The Loader refuses an instantiation's argument exactly where the type system's
    /// `AllConcreteTypes.malformedArgument` finds it malformed, and an argument it admits it
    /// substitutes to one that concretises as the argument spelled does.
    [<Test>]
    let ``the Loader refuses an argument exactly where the type system finds it malformed`` () : unit =
        let f = UnmodelledImage.fixture ()
        let bct = f.BaseClassTypes

        let concretizeIn (state : TypeSystemState) (ty : TypeDefn) =
            TypeSystemState.concretizeType
                f.LoggerFactory
                HostImage.runtimeDirs
                bct
                state
                f.Analysed.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                ty

        let property (cover : ArgumentVerdict -> unit) (spelling : Spelling) : bool =
            if loaderExpected f Position.Argument spelling <> None then
                cover ArgumentVerdict.NotAnArgument
                true
            else

            let argument = toTypeDefn f spelling
            let assemblies, loaded = resolveIn f [] (listOf f argument)

            let state =
                { TypeSystemState.Empty with
                    _LoadedAssemblies = assemblies
                }

            // Concretisation refuses an instantiation whose argument carries a modifier, which has
            // nothing to do with whether the argument is well formed.
            let state, malformed =
                match concretizeIn state (withoutModifiers argument) with
                | state, Ok handle -> state, AllConcreteTypes.malformedArgument bct state.ConcreteTypes [ handle ]
                | _, Error failure ->
                    failwith $"expected %O{argument} to concretise, got %s{TypeBindingFailure.describe failure}"

            match loaded, malformed with
            | Ok (_, info), None ->
                let substituted = info.Generics |> Seq.exactlyOne

                match concretizeIn state argument with
                | state, Ok spelledHandle ->
                    match concretizeIn state substituted with
                    | _, Ok substitutedHandle -> substitutedHandle |> shouldEqual spelledHandle
                    | _, Error failure ->
                        failwith
                            $"%O{argument} concretises, and %O{substituted}, its substitution, does not: %s{TypeBindingFailure.describe failure}"
                | state, Error _ ->
                    match concretizeIn state substituted with
                    | _, Error _ -> ()
                    | _, Ok handle ->
                        failwith
                            $"%O{argument} does not concretise, and %O{substituted}, its substitution, does: %O{handle}"

                if mentionsFunctionPointer spelling then
                    cover ArgumentVerdict.AdmittedOverAFunctionPointer

                cover ArgumentVerdict.Admitted
            | Error (TypeBindingFailure.Unmodelled unmodelled), Some _ when
                unmodelled.Diagnostic.Contains "IDS_CLASSLOAD_INVALIDINSTANTIATION"
                ->
                cover ArgumentVerdict.Refused
            | loaded, malformed ->
                failwith $"for List<%O{argument}> the Loader says %A{loaded} and the type system %A{malformed}"

            true

        let gen =
            Gen.frequency [ 2, genBindingParts ; 1, Gen.map Spelling.Vector genBindingParts ]

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 1000) (Arb.fromGen gen) property

        coverage.Count ArgumentVerdict.Admitted |> shouldBeGreaterThan 100

        coverage.Count ArgumentVerdict.AdmittedOverAFunctionPointer
        |> shouldBeGreaterThan 20

        coverage.Count ArgumentVerdict.Refused |> shouldBeGreaterThan 100

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

    [<RequireQualifiedAccess>]
    type private ArrayAgreement =
        | Binds
        | Absent
        | Unavailable
        | Unmodelled
        | Refused

    /// Concretisation loads an array's element type before the array, as CoreCLR does; the Loader,
    /// which answers only with `System.Array`, must still fail where the element does.
    [<Test>]
    let ``the Loader binds an array exactly where concretisation binds it`` () : unit =
        let f = UnmodelledImage.fixture ()

        let kind (result : Result<'a, TypeBindingFailure>) : ArrayAgreement =
            match result with
            | Ok _ -> ArrayAgreement.Binds
            | Error (TypeBindingFailure.TypeAbsent _) -> ArrayAgreement.Absent
            | Error (TypeBindingFailure.AssemblyUnavailable _) -> ArrayAgreement.Unavailable
            | Error (TypeBindingFailure.BaseTypeAbsent _)
            | Error (TypeBindingFailure.Unmodelled _) -> ArrayAgreement.Unmodelled
            | Error (TypeBindingFailure.Refused _) -> ArrayAgreement.Refused
            | Error (TypeBindingFailure.MethodImplDeclarationAbsent miss) ->
                failwith
                    $"Neither concretisation nor the Loader binds a MethodImpl's declaration, but one reported %O{miss}"

        let genArray : Gen<Spelling> =
            Gen.oneof
                [
                    Gen.map Spelling.Vector genSpelling
                    Gen.map2 (fun element rank -> Spelling.Array (element, rank)) genSpelling (Gen.choose (1, 3))
                ]

        let property (cover : ArrayAgreement -> unit) (spelling : Spelling) : bool =
            let ty = toTypeDefn f spelling
            let concretised = kind (concretize f ty)

            let loaded =
                TypeResolution.resolveTypeFromDefn
                    f.LoggerFactory
                    HostImage.runtimeDirs
                    f.BaseClassTypes
                    ty
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    f.Analysed
                    f.Assemblies
                |> snd
                |> kind

            match concretised, loaded with
            // Each refuses shapes the other models, so an unmodelled answer constrains nothing; nor
            // does a type CoreCLR refuses to load, whose rules the Loader does not check.
            | ArrayAgreement.Unmodelled, _
            | _, ArrayAgreement.Unmodelled -> cover ArrayAgreement.Unmodelled
            | ArrayAgreement.Refused, _
            | _, ArrayAgreement.Refused -> cover ArrayAgreement.Refused
            | concretised, loaded when concretised = loaded -> cover concretised
            | concretised, loaded ->
                failwith $"concretisation says %A{concretised} and the Loader %A{loaded} for %A{ty}"

            true

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 1500) (Arb.fromGen genArray) property

        coverage.Count ArrayAgreement.Binds |> shouldBeGreaterThan 100
        coverage.Count ArrayAgreement.Absent |> shouldBeGreaterThan 100
        coverage.Count ArrayAgreement.Unavailable |> shouldBeGreaterThan 100

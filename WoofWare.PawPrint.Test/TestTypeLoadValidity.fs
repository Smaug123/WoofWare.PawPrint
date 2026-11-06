namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.Loader
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The class loader's rules for every type, checked against CoreCLR over raw TypeSpec signatures:
/// no byref, TypedReference, void, pointer or function-pointer argument in an instantiation; a
/// CLASS/VALUETYPE spelling that matches its definition; the right number of type arguments; no
/// generic definition named bare; and no type variable out of the context's scope. The first rule
/// holds for a generic method's instantiation too, which CoreCLR refuses with
/// `BadImageFormatException` rather than `TypeLoadException`.
///
/// The host oracle is `Module.ResolveType` on the TypeSpec's token, which loads every part of the
/// signature in full, in the generic context given, and `Module.ResolveMethod` on a MethodSpec's. That sidesteps the places CoreCLR loads lazily
/// (a reference-type argument of a parent at `CLASS_LOAD_APPROXPARENTS`, say), which are a question
/// about where a type is named, not about the type. The heads are a struct, a generic enum and two
/// classes, so that the rules are seen to hold for every kind of type alike.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTypeLoadValidity =

    let private loggerFactory = snd (LoggerFactory.makeTest ())
    let private corelib = Assembly.readFile loggerFactory HostImage.coreLibPath
    let private bct = BaseClassTypes.ofCorelib corelib

    let private baseState : Lazy<TypeSystemState> =
        lazy
            (let state = TypeSystemState.Empty.WithLoadedAssembly corelib

             { state with
                 ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
             })

    /// A generic definition of the fixture's image.
    [<RequireQualifiedAccess>]
    type private Head =
        /// `Holder`1`, a struct.
        | Holder
        /// `Outer`1+E`, an enum inheriting its enclosing type's one parameter.
        | Enum
        /// `Klass`1`, a class.
        | Klass
        /// `Pair`2`, a class.
        | Pair
        /// CoreLib's `System.Tuple`1`, a class named by a TypeRef.
        | Tuple
        /// `Orphan`1`, a class whose base type, CoreLib's `System.Absent`, does not exist.
        | Orphan

    [<RequireQualifiedAccess>]
    type private Named =
        /// `Plain`, a non-generic struct of the fixture's image.
        | Plain
        /// `Ref`, a non-generic class of the fixture's image.
        | Ref
        /// A type of CoreLib's `System` namespace.
        | Core of string

    [<RequireQualifiedAccess>]
    type private Target =
        | Head of Head
        | Named of Named

    [<RequireQualifiedAccess>]
    type private Signature =
        | Primitive of byte
        | Nominal of isValueType : bool * Target
        | Generic of isValueType : bool * Head * Signature list
        | Pointer of Signature
        | Byref of Signature
        | Vector of Signature
        | FunctionReturning of Signature
        | FunctionTaking of Signature
        | TypeParameter of int
        | MethodParameter of int

    let private arity (head : Head) : int =
        match head with
        | Head.Holder
        | Head.Enum
        | Head.Klass
        | Head.Tuple
        | Head.Orphan -> 1
        | Head.Pair -> 2

    let private isValueType (target : Target) : bool =
        match target with
        | Target.Head Head.Holder
        | Target.Head Head.Enum
        | Target.Named Named.Plain -> true
        | Target.Head Head.Klass
        | Target.Head Head.Pair
        | Target.Head Head.Tuple
        | Target.Head Head.Orphan
        | Target.Named Named.Ref -> false
        | Target.Named (Named.Core name) ->
            match name with
            | "Int32"
            | "TypedReference"
            | "Void" -> true
            | "String"
            | "Object"
            | "Enum"
            | "ValueType" -> false
            | other -> failwith $"test bug: no kind recorded for System.%s{other}"

    let private int32 : Signature = Signature.Primitive 0x08uy

    /// A generic method `static void M<T0, T1>()` of the fixture's image.
    [<RequireQualifiedAccess>]
    type private Callee =
        /// `Calls.M`, of a class that loads.
        | Calls
        /// `Stray.M`, of a class whose base type, CoreLib's `System.Absent`, does not exist.
        | Stray

    /// A MethodSpec: `callee` at `arguments`, however many there are.
    type private Instantiation =
        {
            Callee : Callee
            Arguments : Signature list
        }

    /// `Calls.M` at `first` and `second`.
    let private ofCalls (first : Signature, second : Signature) : Instantiation =
        {
            Callee = Callee.Calls
            Arguments = [ first ; second ]
        }

    /// The image: `Holder`1`, `Klass`1`, `Pair`2`, `Plain`, `Outer`1` (enclosing the enum `E`),
    /// `Ref`, `Orphan`1`, and `Calls` and `Stray`, which each declare `static void M<T0, T1>()`; then
    /// one TypeSpec per signature, and one MethodSpec per instantiation, whose tokens are returned in
    /// order. A `legacy` image's
    /// metadata table stream says it is version 1.0, for which CoreCLR overlooks a mismatched
    /// CLASS/VALUETYPE spelling of a type named directly. Version 1.0 has another schema for generic
    /// parameters, so a legacy image declares none: its own types are not generic, and its
    /// signatures may instantiate only `Head.Tuple`, and it declares no `M` to instantiate.
    let private emit
        (name : string)
        (legacy : bool)
        (signatures : Signature list)
        (instantiations : Instantiation list)
        : byte[] * int list * int list
        =
        if legacy && not instantiations.IsEmpty then
            failwith "test bug: a legacy image declares no generic method to instantiate"

        let md = MetadataBuilder ()
        let text (s : string) : StringHandle = md.GetOrAddString s

        md.AddModule (0, text name, md.GetOrAddGuid (Guid.NewGuid ()), GuidHandle (), GuidHandle ())
        |> ignore

        md.AddAssembly (
            text name,
            Version (1, 0, 0, 0),
            StringHandle (),
            BlobHandle (),
            enum 0,
            AssemblyHashAlgorithm.None
        )
        |> ignore

        let coreName = typeof<obj>.Assembly.GetName ()

        let coreRef : EntityHandle =
            md.AddAssemblyReference (
                text coreName.Name,
                coreName.Version,
                StringHandle (),
                md.GetOrAddBlob (coreName.GetPublicKeyToken ()),
                enum 0,
                BlobHandle ()
            )
            |> AssemblyReferenceHandle.op_Implicit

        let coreTypes = Dictionary<string, EntityHandle> ()

        let core (name : string) : EntityHandle =
            match coreTypes.TryGetValue name with
            | true, handle -> handle
            | false, _ ->
                let handle : EntityHandle =
                    md.AddTypeReference (coreRef, text "System", text name)
                    |> TypeReferenceHandle.op_Implicit

                coreTypes.Add (name, handle)
                handle

        let typeDef (row : int) : EntityHandle =
            MetadataTokens.TypeDefinitionHandle row |> TypeDefinitionHandle.op_Implicit

        let mutable nextField = 1
        let mutable nextMethod = 1
        // Each owner of generic parameters, with how many it declares. The GenericParam table is
        // sorted by owner's coded index, which interleaves the types' rows with the methods', so the
        // parameters are added once every owner is known.
        let parameterOwners = ResizeArray<EntityHandle * int> ()

        let addType (name : string) (attrs : TypeAttributes) (parent : EntityHandle) (parameters : int) =
            let parameters = if legacy then 0 else parameters

            let handle =
                md.AddTypeDefinition (
                    attrs,
                    StringHandle (),
                    text name,
                    parent,
                    MetadataTokens.FieldDefinitionHandle nextField,
                    MetadataTokens.MethodDefinitionHandle nextMethod
                )

            parameterOwners.Add (TypeDefinitionHandle.op_Implicit handle, parameters)
            handle

        let sealedStruct =
            TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.SequentialLayout

        addType "<Module>" TypeAttributes.NotPublic (EntityHandle ()) 0 |> ignore
        addType "Holder`1" sealedStruct (core "ValueType") 1 |> ignore // row 2
        addType "Klass`1" TypeAttributes.Public (core "Object") 1 |> ignore // row 3
        addType "Pair`2" TypeAttributes.Public (core "Object") 2 |> ignore // row 4
        addType "Plain" sealedStruct (core "ValueType") 0 |> ignore // row 5
        let outer = addType "Outer`1" TypeAttributes.Public (core "Object") 1 // row 6

        let enumType =
            addType "E" (TypeAttributes.NestedPublic ||| TypeAttributes.Sealed) (core "Enum") 1 // row 7

        md.AddNestedType (enumType, outer)

        md.AddFieldDefinition (
            FieldAttributes.Public
            ||| FieldAttributes.SpecialName
            ||| FieldAttributes.RTSpecialName,
            text "value__",
            md.GetOrAddBlob [| 0x06uy ; 0x08uy |]
        )
        |> ignore

        nextField <- 2
        addType "Ref" TypeAttributes.Public (core "Object") 0 |> ignore // row 8
        addType "Orphan`1" TypeAttributes.Public (core "Absent") 1 |> ignore // row 9

        // Every type before `Calls` declares no method: its method list starts where the next
        // type's does.
        let bodies = BlobBuilder ()
        let bodyEncoder = MethodBodyStreamEncoder bodies

        let addM () : MethodDefinitionHandle option =
            if legacy then
                None
            else
                let il = InstructionEncoder (BlobBuilder ())
                il.OpCode ILOpCode.Ret

                let m =
                    md.AddMethodDefinition (
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        MethodImplAttributes.IL,
                        text "M",
                        // A generic method of two parameters, returning void and taking nothing.
                        md.GetOrAddBlob [| 0x10uy ; 2uy ; 0uy ; 0x01uy |],
                        bodyEncoder.AddMethodBody il,
                        MetadataTokens.ParameterHandle 1
                    )

                parameterOwners.Add (MethodDefinitionHandle.op_Implicit m, 2)
                nextMethod <- nextMethod + 1
                Some m

        let staticClass =
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed

        addType "Calls" staticClass (core "Object") 0 |> ignore // row 10
        let callsM = addM ()
        addType "Stray" staticClass (core "Absent") 0 |> ignore // row 11
        let strayM = addM ()

        for owner, parameters in parameterOwners |> Seq.sortBy (fst >> CodedIndex.TypeOrMethodDef) do
            for parameter in 0 .. parameters - 1 do
                md.AddGenericParameter (owner, GenericParameterAttributes.None, text $"T%d{parameter}", parameter)
                |> ignore

        let target (target : Target) : EntityHandle =
            match target with
            | Target.Head Head.Holder -> typeDef 2
            | Target.Head Head.Klass -> typeDef 3
            | Target.Head Head.Pair -> typeDef 4
            | Target.Named Named.Plain -> typeDef 5
            | Target.Head Head.Enum -> typeDef 7
            | Target.Named Named.Ref -> typeDef 8
            | Target.Head Head.Orphan -> typeDef 9
            | Target.Head Head.Tuple -> core "Tuple`1"
            | Target.Named (Named.Core name) -> core name

        let rec encode (builder : BlobBuilder) (signature : Signature) : unit =
            let nominal (isValueType : bool) (ty : Target) : unit =
                builder.WriteByte (if isValueType then 0x11uy else 0x12uy)
                builder.WriteCompressedInteger (CodedIndex.TypeDefOrRef (target ty))

            match signature with
            | Signature.Primitive code -> builder.WriteByte code
            | Signature.Nominal (isValueType, ty) -> nominal isValueType ty
            | Signature.Generic (isValueType, head, args) ->
                builder.WriteByte 0x15uy
                nominal isValueType (Target.Head head)
                builder.WriteCompressedInteger args.Length
                args |> List.iter (encode builder)
            | Signature.Pointer inner ->
                builder.WriteByte 0x0fuy
                encode builder inner
            | Signature.Byref inner ->
                builder.WriteByte 0x10uy
                encode builder inner
            | Signature.Vector inner ->
                builder.WriteByte 0x1duy
                encode builder inner
            | Signature.FunctionReturning inner ->
                builder.WriteBytes [| 0x1buy ; 0uy ; 0uy |]
                encode builder inner
            | Signature.FunctionTaking inner ->
                builder.WriteBytes [| 0x1buy ; 0uy ; 1uy ; 0x01uy |]
                encode builder inner
            | Signature.TypeParameter index ->
                builder.WriteByte 0x13uy
                builder.WriteCompressedInteger index
            | Signature.MethodParameter index ->
                builder.WriteByte 0x1euy
                builder.WriteCompressedInteger index

        let tokens =
            signatures
            |> List.map (fun signature ->
                let builder = BlobBuilder ()
                encode builder signature

                let handle : EntityHandle =
                    md.AddTypeSpecification (md.GetOrAddBlob builder)
                    |> TypeSpecificationHandle.op_Implicit

                MetadataTokens.GetToken handle
            )

        let methodTokens =
            instantiations
            |> List.map (fun instantiation ->
                let builder = BlobBuilder ()
                builder.WriteByte 0x0auy
                builder.WriteCompressedInteger instantiation.Arguments.Length
                instantiation.Arguments |> List.iter (encode builder)

                let m =
                    match instantiation.Callee with
                    | Callee.Calls -> callsM
                    | Callee.Stray -> strayM

                let handle : EntityHandle =
                    md.AddMethodSpecification (MethodDefinitionHandle.op_Implicit m.Value, md.GetOrAddBlob builder)
                    |> MethodSpecificationHandle.op_Implicit

                MetadataTokens.GetToken handle
            )

        let pe =
            ManagedPEBuilder (PEHeaderBuilder.CreateLibraryHeader (), MetadataRootBuilder md, bodies)

        let image = BlobBuilder ()
        pe.Serialize image |> ignore
        let image = image.ToArray ()

        if legacy then
            // ECMA-335 II.24.2.1-2: the metadata root, then stream headers {Offset, Size, Name}, each
            // name null-terminated and padded to four bytes; the `#~` stream's MajorVersion and
            // MinorVersion are its bytes 4 and 5.
            let root =
                use reader = new PEReader (ImmutableArray.CreateRange image)
                reader.PEHeaders.MetadataStartOffset

            let int32At (offset : int) : int = BitConverter.ToInt32 (image, offset)
            let versionLength = int32At (root + 12)
            let streams = int (BitConverter.ToUInt16 (image, root + 16 + versionLength + 2))

            let rec find (header : int) (remaining : int) : int =
                if remaining = 0 then
                    failwith "test bug: the image has no #~ stream"
                else
                    let nameStart = header + 8
                    let nameEnd = Array.IndexOf (image, 0uy, nameStart)
                    let name = Text.Encoding.ASCII.GetString (image, nameStart, nameEnd - nameStart)

                    if name = "#~" then
                        root + int32At header
                    else
                        find (nameStart + ((nameEnd - nameStart) / 4 + 1) * 4) (remaining - 1)

            let tables = find (root + 16 + versionLength + 4) streams
            image.[tables + 4] <- 1uy
            image.[tables + 5] <- 0uy

        image, tokens, methodTokens

    /// The class loader's rule that refused a type.
    [<RequireQualifiedAccess>]
    type private Rule =
        | InvalidInstantiation
        | KindMismatch
        | WrongArity
        /// `IDS_CLASSLOAD_GENERAL`, which CoreCLR raises both for a generic definition named bare and
        /// for a type not declared where the metadata says; the type it names tells them apart.
        | General

    [<RequireQualifiedAccess>]
    type private Outcome =
        | Loads
        /// A `TypeLoadException` under this rule, naming this type as its `TypeName` does.
        | TypeLoad of Rule * typeName : string
        | BadImageFormat

    let private resourceId : FieldInfo =
        typeof<TypeLoadException>.GetField ("_resourceId", BindingFlags.NonPublic ||| BindingFlags.Instance)

    /// What the host makes of the token, by `resolve`, which is `Module.ResolveType` or
    /// `Module.ResolveMethod` in the context `typeArgs` and `methodArgs`.
    let private hostOutcome
        (resolve : int * Type[] * Type[] -> unit)
        (token : int)
        (typeArgs : Type[])
        (methodArgs : Type[])
        : Outcome
        =
        let orNull (args : Type[]) : Type[] = if args.Length = 0 then null else args

        try
            resolve (token, orNull typeArgs, orNull methodArgs)
            Outcome.Loads
        with
        | :? TypeLoadException as e ->
            let rule =
                match resourceId.GetValue e :?> int with
                | 0x1a59 -> Rule.InvalidInstantiation // IDS_CLASSLOAD_INVALIDINSTANTIATION
                | 0x203f -> Rule.KindMismatch // BFA_CLASSLOAD_VALUETYPEMISMATCH
                | 0x1a4b -> Rule.WrongArity // IDS_CLASSLOAD_TYPEWRONGNUMGENERICARGS
                | 0x80131522 -> Rule.General // IDS_CLASSLOAD_GENERAL
                | other -> failwith $"host raised a TypeLoadException with unexpected resource 0x%x{other}: %O{e}"

            Outcome.TypeLoad (rule, e.TypeName)
        | :? ArgumentException as e when (e.InnerException :? BadImageFormatException) -> Outcome.BadImageFormat

    let private typeName (assemblies : LoadedAssemblies) (identity : ResolvedTypeIdentity) : string =
        let definition =
            assemblies.ByDefinitionName(identity.AssemblyFullName).TypeDefs.[identity.TypeDefinition.Get]

        if String.IsNullOrEmpty definition.Namespace then
            definition.Name
        else
            $"%s{definition.Namespace}.%s{definition.Name}"

    /// What PawPrint made of a signature, by concretization or the Loader's walk, in the load context
    /// `assemblies` that walk left. The fixture's image names no nested type that is absent.
    let private pawPrintOutcome (assemblies : LoadedAssemblies) (result : Result<'a, TypeBindingFailure>) : Outcome =
        match result with
        | Ok _ -> Outcome.Loads
        | Error (TypeBindingFailure.Refused refusal) ->
            match refusal with
            | TypeLoadRefusal.InvalidInstantiation (definition, _, _) ->
                Outcome.TypeLoad (Rule.InvalidInstantiation, typeName assemblies definition)
            | TypeLoadRefusal.KindMismatch (named, _) -> Outcome.TypeLoad (Rule.KindMismatch, typeName assemblies named)
            | TypeLoadRefusal.WrongArity (definition, _, _) ->
                Outcome.TypeLoad (Rule.WrongArity, typeName assemblies definition)
            | TypeLoadRefusal.UninstantiatedDefinition definition ->
                Outcome.TypeLoad (Rule.General, typeName assemblies definition)
            | TypeLoadRefusal.InvalidMethodInstantiation _
            | TypeLoadRefusal.TypeParameterOutOfScope _
            | TypeLoadRefusal.MethodParameterOutOfScope _ -> Outcome.BadImageFormat
        | Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, ns, name)))
        | Error (TypeBindingFailure.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, ns, name))) ->
            match ns with
            | Some ns -> Outcome.TypeLoad (Rule.General, $"%s{ns}.%s{name}")
            | None -> Outcome.TypeLoad (Rule.General, name)
        | Error failure -> failwith $"expected a binding or a refusal, got %s{TypeBindingFailure.describe failure}"

    /// Whether the Loader's walk answers `signature` with the type it names. It has no `TypeInfo` for
    /// a pointer, a byref or a function pointer.
    let private loaderAnswers (signature : Signature) : bool =
        match signature with
        | Signature.Pointer _
        | Signature.Byref _
        | Signature.FunctionReturning _
        | Signature.FunctionTaking _ -> false
        | Signature.Primitive _
        | Signature.Nominal _
        | Signature.Generic _
        | Signature.Vector _
        | Signature.TypeParameter _
        | Signature.MethodParameter _ -> true

    /// One signature's outcome on the host, under concretization, and under the Loader's walk where
    /// that walk answers it (see `loaderAnswers`).
    type private Outcomes =
        {
            Host : Outcome
            Concretized : Outcome
            Loaded : Outcome option
        }

    /// Each signature's outcomes under a context instantiating `typeArity` type parameters and
    /// `methodArity` method parameters, and then each instantiation's of `M`, which PawPrint binds
    /// as the interpreter binds a MethodSpec: each argument concretized in the context, and then the
    /// method at them. PawPrint's state, and the Loader's load context, are each threaded through
    /// the signatures in order, so a refusal must leave one the next can use.
    let private check
        (name : string)
        (legacy : bool)
        (typeArity : int)
        (methodArity : int)
        (signatures : Signature list)
        (instantiations : Instantiation list)
        : Outcomes list * Outcomes list
        =
        let image, tokens, methodTokens = emit name legacy signatures instantiations
        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some (name + ".dll")) stream
        let context = AssemblyLoadContext (name, isCollectible = true)

        try
            use hostStream = new MemoryStream (image)
            let host = (context.LoadFromStream hostStream).ManifestModule
            let typeArgs = [| typeof<int> ; typeof<string> |] |> Array.take typeArity
            let methodArgs = [| typeof<string> ; typeof<int> |] |> Array.take methodArity

            let mutable state = baseState.Force().WithLoadedAssembly dumped

            let concretizeAll (primitives : PrimitiveType list) : ImmutableArray<ConcreteTypeHandle> =
                [
                    for primitive in primitives do
                        let next, handle =
                            TypeSystemState.concretizeType
                                loggerFactory
                                HostImage.runtimeDirs
                                bct
                                state
                                corelib.DefinitionFullName
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                (TypeDefn.PrimitiveType primitive)
                            |> BindingResult.bound

                        state <- next
                        yield handle
                ]
                |> ImmutableArray.CreateRange

            let typeGenerics =
                concretizeAll ([ PrimitiveType.Int32 ; PrimitiveType.String ] |> List.take typeArity)

            let methodGenerics =
                concretizeAll ([ PrimitiveType.String ; PrimitiveType.Int32 ] |> List.take methodArity)

            let primitives (primitives : PrimitiveType list) : ImmutableArray<TypeDefn> =
                primitives |> List.map TypeDefn.PrimitiveType |> ImmutableArray.CreateRange

            let loaderTypeGenerics =
                primitives ([ PrimitiveType.Int32 ; PrimitiveType.String ] |> List.take typeArity)

            let loaderMethodGenerics =
                primitives ([ PrimitiveType.String ; PrimitiveType.Int32 ] |> List.take methodArity)

            let mutable assemblies = state._LoadedAssemblies

            let resolveType (token : int, typeArgs : Type[], methodArgs : Type[]) : unit =
                host.ResolveType (token, typeArgs, methodArgs) |> ignore<Type>

            let resolveMethod (token : int, typeArgs : Type[], methodArgs : Type[]) : unit =
                host.ResolveMethod (token, typeArgs, methodArgs) |> ignore<MethodBase>

            [
                for signatureSpelled, token in List.zip signatures tokens do
                    let expected = hostOutcome resolveType token typeArgs methodArgs

                    let handle = MetadataTokens.TypeSpecificationHandle (token &&& 0xffffff)
                    let signature = dumped.TypeSpecs.[handle].Signature

                    let next, result =
                        TypeSystemState.concretizeType
                            loggerFactory
                            HostImage.runtimeDirs
                            bct
                            state
                            dumped.DefinitionFullName
                            typeGenerics
                            methodGenerics
                            signature

                    state <- next

                    let loaded =
                        if loaderAnswers signatureSpelled then
                            let next, result =
                                TypeResolution.resolveTypeFromSpec
                                    loggerFactory
                                    HostImage.runtimeDirs
                                    bct
                                    handle
                                    dumped
                                    loaderTypeGenerics
                                    loaderMethodGenerics
                                    assemblies

                            assemblies <- next
                            Some (pawPrintOutcome next result)
                        else
                            None

                    yield
                        {
                            Host = expected
                            Concretized = pawPrintOutcome next._LoadedAssemblies result
                            Loaded = loaded
                        }
            ],
            [
                for token in methodTokens do
                    let expected = hostOutcome resolveMethod token typeArgs methodArgs

                    let spec =
                        dumped.MethodSpecs.[MetadataTokens.MethodSpecificationHandle (token &&& 0xffffff)]

                    let m =
                        match spec.Method with
                        | MetadataToken.MethodDef handle -> dumped.Methods.[handle]
                        | other -> failwith $"test bug: M's MethodSpec names %O{other}"

                    let rec concretizeArguments
                        (handles : ConcreteTypeHandle list)
                        (arguments : TypeDefn list)
                        : Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure>
                        =
                        match arguments with
                        | [] -> Ok (handles |> List.rev |> ImmutableArray.CreateRange)
                        | argument :: arguments ->
                            let next, result =
                                TypeSystemState.concretizeType
                                    loggerFactory
                                    HostImage.runtimeDirs
                                    bct
                                    state
                                    dumped.DefinitionFullName
                                    typeGenerics
                                    methodGenerics
                                    argument

                            state <- next

                            match result with
                            | Ok handle -> concretizeArguments (handle :: handles) arguments
                            | Error failure -> Error failure

                    let result =
                        match concretizeArguments [] (List.ofSeq spec.Signature) with
                        | Error failure -> Error failure
                        | Ok arguments ->
                            let next, result =
                                MethodConcretisation.concretizeMethodWithAllGenerics
                                    loggerFactory
                                    HostImage.runtimeDirs
                                    bct
                                    ImmutableArray.Empty
                                    m
                                    arguments
                                    state

                            state <- next
                            result

                    yield
                        {
                            Host = expected
                            Concretized = pawPrintOutcome state._LoadedAssemblies result
                            Loaded = None
                        }
            ]
        finally
            context.Unload ()

    let private assertAgree (signatures : Signature list) (outcomes : Outcomes list) : unit =
        for signature, outcome in List.zip signatures outcomes do
            if outcome.Host <> outcome.Concretized then
                failwith $"%A{signature}: the host gives %A{outcome.Host}, concretization %A{outcome.Concretized}"

            match outcome.Loaded with
            | Some loaded when loaded <> outcome.Host ->
                failwith $"%A{signature}: the host gives %A{outcome.Host}, the Loader %A{loaded}"
            | Some _
            | None -> ()

    /// Signatures the host was measured on while designing these checks, with what it gave under
    /// a context of one type parameter (`int`) and one method parameter (`string`).
    let private measured : (Signature * Outcome) list =
        let invalid (name : string) =
            Outcome.TypeLoad (Rule.InvalidInstantiation, name)

        let mismatch (name : string) =
            Outcome.TypeLoad (Rule.KindMismatch, name)

        let absent = Outcome.TypeLoad (Rule.General, "System.Absent")

        let generic (head : Head) (args : Signature list) =
            Signature.Generic (isValueType (Target.Head head), head, args)

        let klass (arg : Signature) = generic Head.Klass [ arg ]

        let core (isValueType : bool) (name : string) =
            Signature.Nominal (isValueType, Target.Named (Named.Core name))

        [
            generic Head.Holder [ int32 ], Outcome.Loads
            generic Head.Holder [ Signature.Pointer int32 ], invalid "Holder`1"
            generic Head.Holder [ Signature.Byref int32 ], invalid "Holder`1"
            generic Head.Holder [ Signature.Primitive 0x16uy ], invalid "Holder`1"
            generic Head.Holder [ Signature.Primitive 0x01uy ], invalid "Holder`1"
            generic Head.Holder [ Signature.FunctionReturning int32 ], invalid "Holder`1"
            generic Head.Holder [ core true "TypedReference" ], invalid "Holder`1"
            generic Head.Holder [ core true "Void" ], invalid "Holder`1"
            generic Head.Enum [ Signature.Pointer int32 ], invalid "E"
            klass (Signature.Pointer int32), invalid "Klass`1"
            klass (klass (Signature.Pointer int32)), invalid "Klass`1"
            klass (Signature.Vector (Signature.Pointer int32)), Outcome.Loads
            klass (Signature.Pointer (Signature.Primitive 0x01uy)), invalid "Klass`1"
            klass (Signature.FunctionTaking (core true "String")), mismatch "System.String"
            Signature.Generic (true, Head.Klass, [ int32 ]), mismatch "Klass`1"
            Signature.Generic (false, Head.Holder, [ int32 ]), mismatch "Holder`1"
            Signature.Generic (false, Head.Enum, [ int32 ]), mismatch "E"
            Signature.Generic (true, Head.Klass, [ klass (Signature.Pointer int32) ]), mismatch "Klass`1"
            klass (core true "String"), mismatch "System.String"
            klass (core false "Int32"), mismatch "System.Int32"
            klass (core true "Int32"), Outcome.Loads
            klass (core true "Enum"), mismatch "System.Enum"
            klass (core false "ValueType"), Outcome.Loads
            klass (Signature.Nominal (false, Target.Named Named.Plain)), mismatch "Plain"
            klass (Signature.Nominal (true, Target.Named Named.Ref)), mismatch "Ref"
            Signature.Vector (core true "String"), mismatch "System.String"
            Signature.Pointer (core true "String"), mismatch "System.String"
            klass (Signature.TypeParameter 0), Outcome.Loads
            klass (Signature.TypeParameter 5), Outcome.BadImageFormat
            klass (Signature.MethodParameter 0), Outcome.Loads
            klass (Signature.MethodParameter 5), Outcome.BadImageFormat
            Signature.TypeParameter 5, Outcome.BadImageFormat
            Signature.Nominal (false, Target.Head Head.Klass), Outcome.TypeLoad (Rule.General, "Klass`1")
            Signature.Nominal (true, Target.Head Head.Klass), Outcome.TypeLoad (Rule.General, "Klass`1")
            klass (Signature.Nominal (false, Target.Head Head.Klass)), Outcome.TypeLoad (Rule.General, "Klass`1")
            klass (Signature.Nominal (true, Target.Head Head.Klass)), Outcome.TypeLoad (Rule.General, "Klass`1")
            // CoreCLR loads a generic definition named bare, base chain and all, before it refuses it.
            Signature.Nominal (false, Target.Head Head.Orphan), absent
            Signature.Nominal (true, Target.Head Head.Orphan), absent
            klass (Signature.Nominal (false, Target.Head Head.Orphan)), absent
            Signature.Vector (Signature.Nominal (false, Target.Head Head.Orphan)), absent
            generic Head.Orphan [ int32 ], absent
            generic Head.Klass [ int32 ; int32 ], Outcome.TypeLoad (Rule.WrongArity, "Klass`1")
            generic Head.Pair [ Signature.Pointer int32 ], Outcome.TypeLoad (Rule.WrongArity, "Pair`2")
            generic Head.Klass [ Signature.Pointer int32 ; int32 ], Outcome.TypeLoad (Rule.WrongArity, "Klass`1")
            generic Head.Klass [ int32 ; Signature.TypeParameter 5 ], Outcome.BadImageFormat
            generic Head.Klass [ core true "String" ; int32 ], mismatch "System.String"
            generic Head.Pair [ Signature.Pointer int32 ; Signature.TypeParameter 5 ], Outcome.BadImageFormat
            generic Head.Pair [ Signature.Pointer int32 ; core true "String" ], mismatch "System.String"
            generic Head.Pair [ klass (Signature.Pointer int32) ; Signature.TypeParameter 5 ], invalid "Klass`1"
            generic Head.Pair [ Signature.TypeParameter 0 ; Signature.Pointer int32 ], invalid "Pair`2"
            Signature.FunctionTaking (Signature.Byref int32), Outcome.Loads
            Signature.Byref int32, Outcome.Loads
        ]

    [<Test>]
    let ``Measured signatures agree with CoreCLR`` () : unit =
        let signatures = measured |> List.map fst
        let outcomes, _ = check "MeasuredTypeLoads" false 1 1 signatures []

        for (signature, measured), outcome in List.zip (List.zip signatures (List.map snd measured)) outcomes do
            if outcome.Host <> measured then
                failwith
                    $"%A{signature}: measured %A{measured} while designing this test, but the host now gives %A{outcome.Host}"

        assertAgree signatures outcomes

    /// In an image whose table stream is version 1.0, CoreCLR overlooks a mismatched spelling of a
    /// type named directly, but not of an instantiation's head.
    [<Test>]
    let ``A version 1.0 table stream waives the spelling check, as CoreCLR does`` () : unit =
        let tuple (arg : Signature) =
            Signature.Generic (false, Head.Tuple, [ arg ])

        let core (isValueType : bool) (name : string) =
            Signature.Nominal (isValueType, Target.Named (Named.Core name))

        let measured =
            [
                tuple (core true "String"), Outcome.Loads
                tuple (core false "Int32"), Outcome.Loads
                Signature.Vector (Signature.Nominal (false, Target.Named Named.Plain)), Outcome.Loads
                Signature.Pointer (Signature.Nominal (true, Target.Named Named.Ref)), Outcome.Loads
                Signature.Generic (true, Head.Tuple, [ int32 ]), Outcome.TypeLoad (Rule.KindMismatch, "System.Tuple`1")
                tuple (Signature.Generic (true, Head.Tuple, [ int32 ])),
                Outcome.TypeLoad (Rule.KindMismatch, "System.Tuple`1")
                tuple (Signature.Pointer int32), Outcome.TypeLoad (Rule.InvalidInstantiation, "System.Tuple`1")
            ]

        let signatures = measured |> List.map fst
        let outcomes, _ = check "LegacyTypeLoads" true 1 1 signatures []

        for (signature, measured), outcome in List.zip measured outcomes do
            if outcome.Host <> measured then
                failwith
                    $"%A{signature}: measured %A{measured} while designing this test, but the host now gives %A{outcome.Host}"

        assertAgree signatures outcomes

    /// Instantiations of `M<T0, T1>` the host was measured on while designing these checks, with what
    /// it gave under a context of one type parameter (`int`) and one method parameter (`string`).
    /// Every argument is loaded before the instantiation is checked, and the method's declaring type
    /// is loaded too, so an argument or a declaring type that fails to load is the failure wherever
    /// it is.
    let private measuredInstantiations : (Instantiation * Outcome) list =
        let klass (arg : Signature) =
            Signature.Generic (false, Head.Klass, [ arg ])

        let calls (arguments : Signature list) : Instantiation =
            {
                Callee = Callee.Calls
                Arguments = arguments
            }

        let stray (arguments : Signature list) : Instantiation =
            {
                Callee = Callee.Stray
                Arguments = arguments
            }

        let absent = Outcome.TypeLoad (Rule.General, "System.Absent")

        [
            ofCalls (int32, int32), Outcome.Loads
            ofCalls (Signature.TypeParameter 0, Signature.MethodParameter 0), Outcome.Loads
            ofCalls (Signature.Pointer int32, int32), Outcome.BadImageFormat
            ofCalls (int32, Signature.Byref int32), Outcome.BadImageFormat
            ofCalls (Signature.Primitive 0x16uy, int32), Outcome.BadImageFormat
            ofCalls (Signature.Primitive 0x01uy, int32), Outcome.BadImageFormat
            ofCalls (Signature.Nominal (true, Target.Named (Named.Core "TypedReference")), int32),
            Outcome.BadImageFormat
            ofCalls (Signature.Nominal (true, Target.Named (Named.Core "Void")), int32), Outcome.BadImageFormat
            ofCalls (int32, Signature.FunctionReturning int32), Outcome.BadImageFormat
            ofCalls (Signature.Vector (Signature.Pointer int32), klass (Signature.Vector (Signature.Pointer int32))),
            Outcome.Loads
            ofCalls (Signature.Pointer int32, klass (Signature.Pointer int32)),
            Outcome.TypeLoad (Rule.InvalidInstantiation, "Klass`1")
            ofCalls (Signature.Pointer int32, Signature.Nominal (false, Target.Head Head.Orphan)), absent
            // The wrong number of arguments.
            calls [ int32 ], Outcome.BadImageFormat
            calls [ int32 ; int32 ; int32 ], Outcome.BadImageFormat
            calls [ Signature.Pointer int32 ], Outcome.BadImageFormat
            calls [ int32 ; int32 ; Signature.Pointer int32 ], Outcome.BadImageFormat
            calls [ klass (Signature.Pointer int32) ], Outcome.TypeLoad (Rule.InvalidInstantiation, "Klass`1")
            calls [ int32 ; int32 ; klass (Signature.Pointer int32) ],
            Outcome.TypeLoad (Rule.InvalidInstantiation, "Klass`1")
            calls [ int32 ; int32 ; Signature.Nominal (false, Target.Head Head.Orphan) ], absent
            calls [ Signature.TypeParameter 5 ], Outcome.BadImageFormat
            // A declaring type that does not load.
            stray [ int32 ; int32 ], absent
            stray [ int32 ], absent
            stray [ Signature.Pointer int32 ; int32 ], absent
            stray [ int32 ; int32 ; klass (Signature.Pointer int32) ],
            Outcome.TypeLoad (Rule.InvalidInstantiation, "Klass`1")
        ]

    [<Test>]
    let ``Measured method instantiations agree with CoreCLR`` () : unit =
        let instantiations = measuredInstantiations |> List.map fst
        let _, outcomes = check "MeasuredMethodInstantiations" false 1 1 [] instantiations

        for (instantiation, measured), outcome in List.zip measuredInstantiations outcomes do
            if outcome.Host <> measured then
                failwith
                    $"%A{instantiation}: measured %A{measured} while designing this test, but the host now gives %A{outcome.Host}"

            if outcome.Concretized <> outcome.Host then
                failwith $"%A{instantiation}: the host gives %A{outcome.Host}, PawPrint %A{outcome.Concretized}"

    /// A type argument no instantiation may have.
    let private malformed : Signature list =
        [
            Signature.Pointer int32
            Signature.Pointer (Signature.Primitive 0x01uy)
            Signature.Byref int32
            Signature.Primitive 0x16uy
            Signature.Primitive 0x01uy
            Signature.Nominal (true, Target.Named (Named.Core "TypedReference"))
            Signature.Nominal (true, Target.Named (Named.Core "Void"))
            Signature.FunctionReturning int32
        ]

    let private heads : Head list =
        [ Head.Holder ; Head.Enum ; Head.Klass ; Head.Pair ; Head.Tuple ]

    let private named : Named list =
        [
            Named.Plain
            Named.Ref
            Named.Core "Int32"
            Named.Core "String"
            Named.Core "Object"
            Named.Core "Enum"
            Named.Core "ValueType"
        ]

    /// Mostly the spelling the definition's kind calls for, sometimes the other.
    let private spelling (target : Target) : Gen<bool> =
        Gen.frequency
            [
                5, Gen.constant (isValueType target)
                1, Gen.constant (not (isValueType target))
            ]

    let private leaf (heads : Head list) : Gen<Signature> =
        Gen.frequency
            [
                4, Gen.elements [ int32 ; Signature.Primitive 0x0euy ; Signature.Primitive 0x1cuy ]
                4,
                gen {
                    let! target = Gen.elements named |> Gen.map Target.Named
                    let! isValueType = spelling target
                    return Signature.Nominal (isValueType, target)
                }
                2, Gen.choose (0, 2) |> Gen.map Signature.TypeParameter
                2, Gen.choose (0, 2) |> Gen.map Signature.MethodParameter
                1,
                gen {
                    let! target = Gen.elements heads |> Gen.map Target.Head
                    let! isValueType = Gen.elements [ false ; true ]
                    return Signature.Nominal (isValueType, target)
                }
            ]

    /// A signature of a type: never a byref, `TypedReference` or `void`, so that it is admissible as an
    /// array element and a pointer target, and the only rules it can break are the ones under test.
    let rec private typeSignature (heads : Head list) (size : int) : Gen<Signature> =
        let leaf = leaf heads
        let typeSignature = typeSignature heads
        let argument = argument heads

        if size <= 0 then
            leaf
        else
            let smaller = size / 2

            let functionPart =
                Gen.frequency
                    [
                        5, typeSignature smaller
                        1, typeSignature smaller |> Gen.map Signature.Byref
                        1, Gen.constant (Signature.Primitive 0x16uy)
                    ]

            Gen.frequency
                [
                    2, leaf
                    4,
                    gen {
                        let! head = Gen.elements heads
                        let! isValueType = spelling (Target.Head head)

                        let! count =
                            Gen.frequency
                                [
                                    12, Gen.constant (arity head)
                                    1, Gen.constant (arity head + 1)
                                    1, Gen.constant (max 1 (arity head - 1))
                                ]

                        let! args = Gen.listOfLength count (argument smaller)
                        return Signature.Generic (isValueType, head, args)
                    }
                    1,
                    Gen.frequency [ 5, typeSignature smaller ; 1, Gen.constant (Signature.Primitive 0x01uy) ]
                    |> Gen.map Signature.Pointer
                    1, typeSignature smaller |> Gen.map Signature.Vector
                    1, functionPart |> Gen.map Signature.FunctionTaking
                    1, functionPart |> Gen.map Signature.FunctionReturning
                ]

    and private argument (heads : Head list) (size : int) : Gen<Signature> =
        Gen.frequency [ 8, typeSignature heads size ; 1, Gen.elements malformed ]

    let rec private mentionsParameter (signature : Signature) : bool =
        match signature with
        | Signature.TypeParameter _
        | Signature.MethodParameter _ -> true
        | Signature.Primitive _
        | Signature.Nominal _ -> false
        | Signature.Generic (_, _, args) -> args |> List.exists mentionsParameter
        | Signature.Pointer inner
        | Signature.Byref inner
        | Signature.Vector inner
        | Signature.FunctionReturning inner
        | Signature.FunctionTaking inner -> mentionsParameter inner

    [<Test>]
    let ``Generated method instantiations agree with CoreCLR`` () : unit =
        let generator =
            gen {
                let! typeArity = Gen.choose (0, 2)
                let! methodArity = Gen.choose (0, 2)
                let! count = Gen.choose (1, 8)
                let argument = Gen.choose (0, 8) |> Gen.bind (argument heads)

                let instantiation =
                    gen {
                        let! callee = Gen.frequency [ 6, Gen.constant Callee.Calls ; 1, Gen.constant Callee.Stray ]

                        let! arity = Gen.frequency [ 6, Gen.constant 2 ; 1, Gen.constant 1 ; 1, Gen.constant 3 ]

                        let! arguments = Gen.listOfLength arity argument

                        return
                            {
                                Callee = callee
                                Arguments = arguments
                            }
                    }

                let! instantiations = Gen.listOfLength count instantiation
                return typeArity, methodArity, instantiations
            }

        let seen = HashSet<Outcome> ()
        let mutable refusedForAnArgument = 0
        let mutable refusedForArity = 0
        let mutable cases = 0

        let property (typeArity : int, methodArity : int, instantiations : Instantiation list) : unit =
            cases <- cases + 1

            let _, outcomes =
                check $"GeneratedMethodInstantiations%d{cases}" false typeArity methodArity [] instantiations

            for instantiation, outcome in List.zip instantiations outcomes do
                if outcome.Concretized <> outcome.Host then
                    failwith $"%A{instantiation}: the host gives %A{outcome.Host}, PawPrint %A{outcome.Concretized}"

                seen.Add (
                    match outcome.Host with
                    | Outcome.TypeLoad (rule, _) -> Outcome.TypeLoad (rule, "")
                    | other -> other
                )
                |> ignore

                // With no type variable to be out of scope, the instantiation itself is refused.
                if
                    outcome.Host = Outcome.BadImageFormat
                    && not (instantiation.Arguments |> List.exists mentionsParameter)
                then
                    if instantiation.Arguments.Length = 2 then
                        refusedForAnArgument <- refusedForAnArgument + 1
                    else
                        refusedForArity <- refusedForArity + 1

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen generator) property)

        seen |> shouldContain Outcome.Loads
        seen |> shouldContain Outcome.BadImageFormat
        seen |> shouldContain (Outcome.TypeLoad (Rule.InvalidInstantiation, ""))
        seen |> shouldContain (Outcome.TypeLoad (Rule.General, ""))
        // Over 25 unseeded runs, measured between 76 and 128, and between 44 and 75.
        refusedForAnArgument |> shouldBeGreaterThan 40
        refusedForArity |> shouldBeGreaterThan 20

    [<Test>]
    let ``Generated signatures agree with CoreCLR, whatever kind of type they name`` () : unit =
        let generator =
            gen {
                let! legacy = Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ]
                let! typeArity = Gen.choose (0, 2)
                let! methodArity = Gen.choose (0, 2)
                let! count = Gen.choose (1, 8)
                // Only CoreLib's generic types can be instantiated in a legacy image.
                let heads = if legacy then [ Head.Tuple ] else heads

                let! signatures = Gen.listOfLength count (Gen.choose (0, 12) |> Gen.bind (typeSignature heads))
                return legacy, typeArity, methodArity, signatures
            }

        let seen = HashSet<Outcome> ()
        let seenByTheLoader = HashSet<Outcome> ()
        let refusingHeads = HashSet<string> ()
        let mutable cases = 0

        let property (legacy : bool, typeArity : int, methodArity : int, signatures : Signature list) : unit =
            cases <- cases + 1

            let outcomes, _ =
                check $"GeneratedTypeLoads%d{cases}" legacy typeArity methodArity signatures []

            assertAgree signatures outcomes

            for outcome in outcomes do
                let kind =
                    match outcome.Host with
                    | Outcome.TypeLoad (rule, _) -> Outcome.TypeLoad (rule, "")
                    | other -> other

                seen.Add kind |> ignore

                if outcome.Loaded.IsSome then
                    seenByTheLoader.Add kind |> ignore

                match outcome.Host with
                | Outcome.TypeLoad (Rule.InvalidInstantiation, name) -> refusingHeads.Add name |> ignore
                | _ -> ()

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen generator) property)

        // Every outcome, under concretization and the Loader alike, and an invalid instantiation of a
        // struct, an enum and a class.
        seen.Count |> shouldEqual 6
        seenByTheLoader.Count |> shouldEqual 6

        refusingHeads
        |> Set.ofSeq
        |> Set.isSubset (Set.ofList [ "Holder`1" ; "E" ; "Klass`1" ])
        |> shouldEqual true

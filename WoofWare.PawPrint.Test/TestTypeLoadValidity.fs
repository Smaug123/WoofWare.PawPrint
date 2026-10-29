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
/// generic definition named bare; and no type variable out of the context's scope.
///
/// The host oracle is `Module.ResolveType` on the TypeSpec's token, which loads every part of the
/// signature in full, in the generic context given. That sidesteps the places CoreCLR loads lazily
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
        | Head.Tuple -> 1
        | Head.Pair -> 2

    let private isValueType (target : Target) : bool =
        match target with
        | Target.Head Head.Holder
        | Target.Head Head.Enum
        | Target.Named Named.Plain -> true
        | Target.Head Head.Klass
        | Target.Head Head.Pair
        | Target.Head Head.Tuple
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

    /// The image: `Holder`1`, `Klass`1`, `Pair`2`, `Plain`, `Outer`1` (enclosing the enum `E`) and
    /// `Ref`, then one TypeSpec per signature, whose tokens are returned in order. A `legacy` image's
    /// metadata table stream says it is version 1.0, for which CoreCLR overlooks a mismatched
    /// CLASS/VALUETYPE spelling of a type named directly. Version 1.0 has another schema for generic
    /// parameters, so a legacy image declares none: its own types are not generic, and its
    /// signatures may instantiate only `Head.Tuple`.
    let private emit (name : string) (legacy : bool) (signatures : Signature list) : byte[] * int list =
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

        let addType (name : string) (attrs : TypeAttributes) (parent : EntityHandle) (parameters : int) =
            let parameters = if legacy then 0 else parameters

            let handle =
                md.AddTypeDefinition (
                    attrs,
                    StringHandle (),
                    text name,
                    parent,
                    MetadataTokens.FieldDefinitionHandle nextField,
                    MetadataTokens.MethodDefinitionHandle 1
                )

            for parameter in 0 .. parameters - 1 do
                md.AddGenericParameter (
                    TypeDefinitionHandle.op_Implicit handle,
                    GenericParameterAttributes.None,
                    text $"T%d{parameter}",
                    parameter
                )
                |> ignore

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

        let target (target : Target) : EntityHandle =
            match target with
            | Target.Head Head.Holder -> typeDef 2
            | Target.Head Head.Klass -> typeDef 3
            | Target.Head Head.Pair -> typeDef 4
            | Target.Named Named.Plain -> typeDef 5
            | Target.Head Head.Enum -> typeDef 7
            | Target.Named Named.Ref -> typeDef 8
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

        let pe =
            ManagedPEBuilder (PEHeaderBuilder.CreateLibraryHeader (), MetadataRootBuilder md, BlobBuilder ())

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

        image, tokens

    /// The class loader's rule that refused a type.
    [<RequireQualifiedAccess>]
    type private Rule =
        | InvalidInstantiation
        | KindMismatch
        | WrongArity
        | UninstantiatedDefinition

    [<RequireQualifiedAccess>]
    type private Outcome =
        | Loads
        /// A `TypeLoadException` under this rule, naming this type as its `TypeName` does.
        | TypeLoad of Rule * typeName : string
        | BadImageFormat

    let private resourceId : FieldInfo =
        typeof<TypeLoadException>.GetField ("_resourceId", BindingFlags.NonPublic ||| BindingFlags.Instance)

    let private hostOutcome (modul : Module) (token : int) (typeArgs : Type[]) (methodArgs : Type[]) : Outcome =
        let orNull (args : Type[]) : Type[] = if args.Length = 0 then null else args

        try
            modul.ResolveType (token, orNull typeArgs, orNull methodArgs) |> ignore<Type>
            Outcome.Loads
        with
        | :? TypeLoadException as e ->
            let rule =
                match resourceId.GetValue e :?> int with
                | 0x1a59 -> Rule.InvalidInstantiation // IDS_CLASSLOAD_INVALIDINSTANTIATION
                | 0x203f -> Rule.KindMismatch // BFA_CLASSLOAD_VALUETYPEMISMATCH
                | 0x1a4b -> Rule.WrongArity // IDS_CLASSLOAD_TYPEWRONGNUMGENERICARGS
                | 0x80131522 -> Rule.UninstantiatedDefinition // IDS_CLASSLOAD_GENERAL
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
    /// `assemblies` that walk left.
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
                Outcome.TypeLoad (Rule.UninstantiatedDefinition, typeName assemblies definition)
            | TypeLoadRefusal.TypeParameterOutOfScope _
            | TypeLoadRefusal.MethodParameterOutOfScope _ -> Outcome.BadImageFormat
        | Error failure -> failwith $"expected a binding or a refusal, got %s{TypeBindingFailure.describe failure}"

    /// Whether the Loader's walk answers `signature` with the type it names. It has no `TypeInfo` for
    /// a pointer, a byref or a function pointer, and it binds a generic definition named bare as the
    /// open definition, which a token naming one denotes.
    let private loaderAnswers (signature : Signature) : bool =
        match signature with
        | Signature.Pointer _
        | Signature.Byref _
        | Signature.FunctionReturning _
        | Signature.FunctionTaking _
        | Signature.Nominal (_, Target.Head _) -> false
        | Signature.Primitive _
        | Signature.Nominal (_, Target.Named _)
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
    /// `methodArity` method parameters. PawPrint's state, and the Loader's load context, are each
    /// threaded through the signatures in order, so a refusal must leave one the next can use.
    let private check
        (name : string)
        (legacy : bool)
        (typeArity : int)
        (methodArity : int)
        (signatures : Signature list)
        : Outcomes list
        =
        let image, tokens = emit name legacy signatures
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

            [
                for signatureSpelled, token in List.zip signatures tokens do
                    let expected = hostOutcome host token typeArgs methodArgs

                    let signature =
                        dumped.TypeSpecs.[MetadataTokens.TypeSpecificationHandle (token &&& 0xffffff)].Signature

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
                                TypeResolution.resolveTypeFromDefn
                                    loggerFactory
                                    HostImage.runtimeDirs
                                    bct
                                    signature
                                    loaderTypeGenerics
                                    loaderMethodGenerics
                                    dumped
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
            klass (Signature.Nominal (false, Target.Head Head.Klass)),
            Outcome.TypeLoad (Rule.UninstantiatedDefinition, "Klass`1")
            klass (Signature.Nominal (true, Target.Head Head.Klass)),
            Outcome.TypeLoad (Rule.UninstantiatedDefinition, "Klass`1")
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
        let outcomes = check "MeasuredTypeLoads" false 1 1 signatures

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
        let outcomes = check "LegacyTypeLoads" true 1 1 signatures

        for (signature, measured), outcome in List.zip measured outcomes do
            if outcome.Host <> measured then
                failwith
                    $"%A{signature}: measured %A{measured} while designing this test, but the host now gives %A{outcome.Host}"

        assertAgree signatures outcomes

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

            let outcomes =
                check $"GeneratedTypeLoads%d{cases}" legacy typeArity methodArity signatures

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

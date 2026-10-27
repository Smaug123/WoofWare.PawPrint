namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open System.Runtime.Loader
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Enum backing types checked against CoreCLR over raw field signatures. Primitive and nominal
/// spellings of the same CoreLib type are deliberately distinct inputs. The host's load decision
/// and Enum.GetUnderlyingType provide an oracle independent of PawPrint's resolver.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEnumBackingType =

    let private loggerFactory = snd (LoggerFactory.makeTest ())
    let private corelib = Assembly.readFile loggerFactory HostImage.coreLibPath
    let private bct = BaseClassTypes.ofCorelib corelib

    let private baseState : Lazy<TypeSystemState> =
        lazy
            (let state = TypeSystemState.Empty.WithLoadedAssembly corelib

             { state with
                 ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
             })

    [<RequireQualifiedAccess>]
    type private Target =
        | Core of string
        | Enum of int
        | Plain
        | Holder
        | ExternalStruct
        | MissingType
        | Missing

    [<RequireQualifiedAccess>]
    type private Signature =
        | Primitive of byte
        | Nominal of isValueType : bool * Target
        | Generic of isValueType : bool * Target * Signature list
        | Modified of required : bool * Signature
        | Pointer of Signature
        | Byref of Signature
        | Vector of Signature
        | Array of Signature
        | Pinned of Signature
        | FunctionPointer
        | FunctionReturning of Signature
        | FunctionTaking of Signature
        | TypeParameter
        | TypeParameterAt of int
        | MethodParameter

    type private Field =
        {
            Name : string
            Attributes : FieldAttributes
            Signature : Signature
        }

    type private Spec =
        {
            Layout : TypeAttributes
            Fields : Field list
            Nested : bool
            GenericArity : int
        }

    let private primitives : (string * byte * PrimitiveType) list =
        [
            "Boolean", 2uy, PrimitiveType.Boolean
            "Char", 3uy, PrimitiveType.Char
            "SByte", 4uy, PrimitiveType.SByte
            "Byte", 5uy, PrimitiveType.Byte
            "Int16", 6uy, PrimitiveType.Int16
            "UInt16", 7uy, PrimitiveType.UInt16
            "Int32", 8uy, PrimitiveType.Int32
            "UInt32", 9uy, PrimitiveType.UInt32
            "Int64", 10uy, PrimitiveType.Int64
            "UInt64", 11uy, PrimitiveType.UInt64
            "Single", 12uy, PrimitiveType.Single
            "Double", 13uy, PrimitiveType.Double
            "IntPtr", 24uy, PrimitiveType.IntPtr
            "UIntPtr", 25uy, PrimitiveType.UIntPtr
        ]

    let private field (signature : Signature) : Field =
        {
            Name = "value__"
            Attributes =
                FieldAttributes.Public
                ||| FieldAttributes.SpecialName
                ||| FieldAttributes.RTSpecialName
            Signature = signature
        }

    let private spec (signatures : Signature list) : Spec =
        {
            Layout = TypeAttributes.AutoLayout
            Fields = List.map field signatures
            Nested = false
            GenericArity = 0
        }

    let private primitive (code : byte) : Signature = Signature.Primitive code

    let private enumRef (index : int) : Signature =
        Signature.Nominal (true, Target.Enum index)

    let private emit (name : string) (specs : Spec list) : byte[] =
        let md = MetadataBuilder ()
        let text (s : string) : StringHandle = md.GetOrAddString s
        let bytes (b : byte[]) : BlobHandle = md.GetOrAddBlob b

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

        let assemblyRef (name : AssemblyName) : EntityHandle =
            md.AddAssemblyReference (
                text name.Name,
                name.Version,
                StringHandle (),
                bytes (name.GetPublicKeyToken () |> Option.ofObj |> Option.defaultValue [||]),
                enum 0,
                BlobHandle ()
            )
            |> AssemblyReferenceHandle.op_Implicit

        let coreRef = assemblyRef (typeof<obj>.Assembly.GetName ())
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

        let externalStruct : Lazy<EntityHandle> =
            lazy
                (md.AddTypeReference (
                    assemblyRef (typeof<ImmutableArray<int>>.Assembly.GetName ()),
                    text "System.Collections.Immutable",
                    text "ImmutableArray`1"
                 )
                 |> TypeReferenceHandle.op_Implicit)

        let missing : Lazy<EntityHandle> =
            lazy
                (md.AddTypeReference (
                    assemblyRef (AssemblyName "EnumBackingTypeMissing, Version=1.0.0.0"),
                    text "Missing",
                    text "E"
                 )
                 |> TypeReferenceHandle.op_Implicit)

        let missingType : Lazy<EntityHandle> =
            lazy
                (md.AddTypeReference (
                    assemblyRef (typeof<ImmutableArray<int>>.Assembly.GetName ()),
                    text "System.Collections.Immutable",
                    text "NoSuchEnumBackingType"
                 )
                 |> TypeReferenceHandle.op_Implicit)

        let target (target : Target) : EntityHandle =
            match target with
            | Target.Core name -> core name
            | Target.Enum index -> typeDef (5 + index)
            | Target.Plain -> typeDef 4
            | Target.Holder -> typeDef 3
            | Target.ExternalStruct -> externalStruct.Force ()
            | Target.MissingType -> missingType.Force ()
            | Target.Missing -> missing.Force ()

        let rec signature (builder : BlobBuilder) (value : Signature) : unit =
            let nominal (isValueType : bool) (ty : Target) : unit =
                builder.WriteByte (if isValueType then 0x11uy else 0x12uy)
                builder.WriteCompressedInteger (CodedIndex.TypeDefOrRef (target ty))

            match value with
            | Signature.Primitive code -> builder.WriteByte code
            | Signature.Nominal (isValueType, ty) -> nominal isValueType ty
            | Signature.Generic (isValueType, ty, args) ->
                builder.WriteByte 0x15uy
                nominal isValueType ty
                builder.WriteCompressedInteger args.Length
                args |> List.iter (signature builder)
            | Signature.Modified (required, inner) ->
                builder.WriteByte (if required then 0x1fuy else 0x20uy)
                builder.WriteCompressedInteger (CodedIndex.TypeDefOrRef (core "Int32"))
                signature builder inner
            | Signature.Pointer inner ->
                builder.WriteByte 0x0fuy
                signature builder inner
            | Signature.Byref inner ->
                builder.WriteByte 0x10uy
                signature builder inner
            | Signature.Vector inner ->
                builder.WriteByte 0x1duy
                signature builder inner
            | Signature.Array inner ->
                builder.WriteByte 0x14uy
                signature builder inner
                builder.WriteBytes [| 2uy ; 0uy ; 2uy ; 0uy ; 0uy |]
            | Signature.Pinned inner ->
                builder.WriteByte 0x45uy
                signature builder inner
            | Signature.FunctionPointer -> builder.WriteBytes [| 0x1buy ; 0uy ; 0uy ; 1uy |]
            | Signature.FunctionReturning inner ->
                builder.WriteBytes [| 0x1buy ; 0uy ; 0uy |]
                signature builder inner
            | Signature.FunctionTaking inner ->
                builder.WriteBytes [| 0x1buy ; 0uy ; 1uy ; 1uy |]
                signature builder inner
            | Signature.TypeParameter -> builder.WriteBytes [| 0x13uy ; 0uy |]
            | Signature.TypeParameterAt index ->
                builder.WriteByte 0x13uy
                builder.WriteCompressedInteger index
            | Signature.MethodParameter -> builder.WriteBytes [| 0x1euy ; 0uy |]

        let mutable nextField = 1
        let encodedFields = ResizeArray<FieldDefinitionHandle * byte[]> ()

        let addField (field : Field) : unit =
            let builder = BlobBuilder ()
            builder.WriteByte 6uy
            signature builder field.Signature
            let blob = builder.ToArray ()
            let handle = md.AddFieldDefinition (field.Attributes, text field.Name, bytes blob)
            encodedFields.Add (handle, blob)

            if field.Attributes.HasFlag FieldAttributes.Literal then
                md.AddConstant (FieldDefinitionHandle.op_Implicit handle, 0) |> ignore

            nextField <- nextField + 1

        let addType
            (name : string)
            (ns : string)
            (attrs : TypeAttributes)
            (parent : EntityHandle)
            : TypeDefinitionHandle
            =
            md.AddTypeDefinition (
                attrs,
                text ns,
                text name,
                parent,
                MetadataTokens.FieldDefinitionHandle nextField,
                MetadataTokens.MethodDefinitionHandle 1
            )

        addType "<Module>" "" TypeAttributes.NotPublic (EntityHandle ()) |> ignore

        let outer =
            addType
                "Outer`1"
                ""
                (TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
                (core "Object")

        md.AddGenericParameter (TypeDefinitionHandle.op_Implicit outer, GenericParameterAttributes.None, text "T", 0)
        |> ignore

        let holder =
            addType
                "Holder`1"
                ""
                (TypeAttributes.Public
                 ||| TypeAttributes.Sealed
                 ||| TypeAttributes.SequentialLayout)
                (core "ValueType")

        md.AddGenericParameter (TypeDefinitionHandle.op_Implicit holder, GenericParameterAttributes.None, text "T", 0)
        |> ignore

        addType
            "Int32"
            "System"
            (TypeAttributes.Public
             ||| TypeAttributes.Sealed
             ||| TypeAttributes.SequentialLayout)
            (core "ValueType")
        |> ignore

        addField (field (primitive 8uy))

        specs
        |> List.iteri (fun index spec ->
            let visibility =
                if spec.Nested then
                    TypeAttributes.NestedPublic
                else
                    TypeAttributes.Public

            let handle =
                addType $"E%d{index}" "" (visibility ||| TypeAttributes.Sealed ||| spec.Layout) (core "Enum")

            if spec.Nested then
                md.AddNestedType (handle, outer)

            for parameter in 0 .. spec.GenericArity - 1 do
                md.AddGenericParameter (
                    TypeDefinitionHandle.op_Implicit handle,
                    GenericParameterAttributes.None,
                    text $"T%d{parameter}",
                    parameter
                )
                |> ignore

            spec.Fields |> List.iter addField
        )

        let builder =
            ManagedPEBuilder (PEHeaderBuilder.CreateLibraryHeader (), MetadataRootBuilder md, BlobBuilder ())

        let image = BlobBuilder ()
        builder.Serialize image |> ignore
        let image = image.ToArray ()
        use reader = new PEReader (ImmutableArray.CreateRange image)
        let metadata = reader.GetMetadataReader ()

        for handle, expected in encodedFields do
            let actual = metadata.GetBlobBytes (metadata.GetFieldDefinition(handle).Signature)
            actual |> shouldEqual expected

        image

    let private concretize
        (state : TypeSystemState)
        (image : DumpedAssembly)
        (index : int)
        (argument : PrimitiveType)
        : TypeSystemState * ConcreteTypeHandle
        =
        let ty = image.TypeDefs.[MetadataTokens.TypeDefinitionHandle (5 + index)]
        let signature = TypeDefn.FromDefinition (ty.Identity, SignatureTypeKind.ValueType)

        let signature =
            if ty.Generics.IsEmpty then
                signature
            else
                TypeDefn.GenericInstantiation (
                    signature,
                    ImmutableArray.CreateRange (Seq.replicate ty.Generics.Length (TypeDefn.PrimitiveType argument))
                )

        TypeSystemState.concretizeType
            loggerFactory
            HostImage.runtimeDirs
            bct
            state
            image.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            signature
        |> BindingResult.bound

    let private hostAnswer
        (assembly : System.Reflection.Assembly)
        (index : int)
        (spec : Spec)
        (argument : Type)
        : PrimitiveType option
        =
        try
            let name = (if spec.Nested then "Outer`1+" else "") + $"E%d{index}"
            let ty = assembly.GetType (name, throwOnError = true)

            let ty =
                if spec.GenericArity = 0 then
                    ty
                else
                    ty.MakeGenericType (Array.create spec.GenericArity argument)

            let underlying = Enum.GetUnderlyingType ty

            primitives
            |> List.find (fun (name, _, _) -> underlying = typeof<obj>.Assembly.GetType ("System." + name))
            |> fun (_, _, primitive) -> Some primitive
        with
        | :? TypeLoadException
        | :? BadImageFormatException -> None

    let private check (name : string) (specs : Spec list) : PrimitiveType option list =
        let image = emit name specs
        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some (name + ".dll")) stream
        let context = AssemblyLoadContext (name, isCollectible = true)

        try
            use hostStream = new MemoryStream (image)
            let host = context.LoadFromStream hostStream
            let mutable state = baseState.Force().WithLoadedAssembly dumped

            [
                for index, spec in List.indexed specs do
                    for hostArg, argument in
                        (if spec.GenericArity = 0 then
                             [ typeof<int>, PrimitiveType.Int32 ]
                         else
                             [
                                 typeof<int>, PrimitiveType.Int32
                                 typeof<uint32>, PrimitiveType.UInt32
                                 typeof<string>, PrimitiveType.String
                             ]) do
                        let expected = hostAnswer host index spec hostArg
                        let next, handle = concretize state dumped index argument

                        let next, actual =
                            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct next handle

                        state <- next

                        match expected, actual with
                        | Some expected, Ok (Some actual) -> actual |> shouldEqual expected
                        | None, Error _ -> ()
                        | _ ->
                            failwith
                                $"%s{name} E%d{index}<%O{argument}>: host=%A{expected}, actual=%A{actual}, specs=%A{specs}"

                        yield expected
            ]
        finally
            context.Unload ()

    [<Test>]
    let ``Raw primitive and nominal backing signatures agree with CoreCLR`` () : unit =
        let cases =
            [
                for name, code, _ in primitives do
                    yield spec [ primitive code ]
                    yield spec [ Signature.Nominal (true, Target.Core name) ]
                    yield spec [ Signature.Nominal (false, Target.Core name) ]
                    yield spec [ Signature.Modified (true, primitive code) ]
                    yield spec [ Signature.Modified (false, primitive code) ]
                for name in
                    [
                        "Void"
                        "TypedReference"
                        "String"
                        "Object"
                        "RuntimeArgumentHandle"
                        "RuntimeMethodHandleInternal"
                        "RuntimeFieldHandleInternal"
                        "Decimal"
                        "Int128"
                    ] do
                    yield spec [ Signature.Nominal (true, Target.Core name) ]
                for signature in
                    [
                        primitive 14uy
                        primitive 22uy
                        primitive 28uy
                        Signature.Pointer (primitive 8uy)
                        Signature.Byref (primitive 8uy)
                        Signature.Vector (primitive 8uy)
                        Signature.Pinned (primitive 8uy)
                        Signature.FunctionPointer
                        Signature.MethodParameter
                        Signature.Nominal (true, Target.Plain)
                    ] do
                    yield spec [ signature ]
            ]

        let answers = check "EnumSignatures" cases

        answers
        |> List.filter Option.isSome
        |> List.length
        |> shouldEqual (primitives.Length * 3)

    [<Test>]
    let ``Enum field chains and metadata shape agree with CoreCLR`` () : unit =
        let ignored (attributes : FieldAttributes) : Field =
            { field (primitive 8uy) with
                Name = "Ignored"
                Attributes = FieldAttributes.Public ||| attributes
            }

        let cases =
            [
                spec [ primitive 9uy ]
                spec [ enumRef 0 ]
                spec [ enumRef 1 ]
                spec [ Signature.Nominal (false, Target.Enum 0) ]
                spec [ enumRef 4 ]
                spec [ enumRef 6 ]
                spec [ enumRef 5 ]
                spec []
                spec [ primitive 8uy ; primitive 8uy ]
                { spec [ primitive 8uy ] with
                    Fields =
                        [
                            { field (primitive 8uy) with
                                Name = "storage"
                                Attributes = FieldAttributes.Private
                            }
                        ]
                }
                { spec [ primitive 8uy ] with
                    Fields =
                        [
                            field (primitive 8uy)
                            ignored FieldAttributes.Static
                            ignored FieldAttributes.Literal
                        ]
                }
                { spec [] with
                    Fields = [ ignored FieldAttributes.Literal ; ignored FieldAttributes.Static ]
                }
                { spec [ primitive 8uy ] with
                    Layout = TypeAttributes.SequentialLayout
                }
                { spec [ primitive 8uy ] with
                    Layout = TypeAttributes.ExplicitLayout
                }
                { spec [ primitive 8uy ] with
                    Layout = TypeAttributes.LayoutMask
                }
                { spec [ primitive 8uy ] with
                    Nested = true
                    GenericArity = 1
                }
                { spec [ Signature.TypeParameter ] with
                    Nested = true
                    GenericArity = 1
                }
                spec [ Signature.Generic (true, Target.Enum 15, [ primitive 9uy ]) ]
                { spec [ Signature.Generic (true, Target.Enum 15, [ Signature.TypeParameter ]) ] with
                    Nested = true
                    GenericArity = 1
                }
                { spec [ primitive 8uy ] with
                    GenericArity = 1
                }
                { spec [ primitive 8uy ] with
                    Nested = true
                    GenericArity = 2
                }
                spec [ Signature.Nominal (true, Target.Core "DayOfWeek") ]
                spec [ Signature.Nominal (false, Target.Core "DayOfWeek") ]
            ]

        check "EnumShapes" cases |> ignore

    let private genericEnum : Spec =
        { spec [ primitive 8uy ] with
            Nested = true
            GenericArity = 1
        }

    let private genericBacking (argument : Signature) : Spec =
        spec [ Signature.Generic (true, Target.Enum 0, [ argument ]) ]

    let private illegalArguments : Signature list =
        [
            Signature.Pointer (primitive 8uy)
            Signature.Byref (primitive 8uy)
            Signature.FunctionPointer
            primitive 22uy
            Signature.Nominal (true, Target.Core "TypedReference")
            Signature.Nominal (true, Target.Core "Void")
        ]

    [<Test>]
    let ``Nominal kinds inside generic backing arguments agree with CoreCLR`` () : unit =
        let arguments =
            [
                for target in [ Target.Core "String" ; Target.Core "Object" ] do
                    for isValueType in [ false ; true ] do
                        let nominal = Signature.Nominal (isValueType, target)
                        yield nominal
                        yield Signature.Generic (true, Target.Holder, [ nominal ])
                yield Signature.Nominal (true, Target.Core "Int32")
                yield Signature.Nominal (true, Target.Plain)
                yield Signature.Generic (true, Target.Holder, [ primitive 8uy ])
                for isValueType in [ false ; true ] do
                    yield Signature.Generic (isValueType, Target.Core "Tuple`1", [ primitive 8uy ])
            ]

        let answers =
            check "NominalEnumArguments" (genericEnum :: List.map genericBacking arguments)
            |> List.skip 3

        answers |> List.filter Option.isSome |> List.length |> shouldEqual 8
        answers |> List.filter Option.isNone |> List.length |> shouldEqual 5

    [<Test>]
    let ``Generated nested nominal kinds agree with CoreCLR`` () : unit =
        let generator =
            gen {
                let! name = Gen.elements [ "String" ; "Object" ; "Type" ]
                let! isValueType = Gen.elements [ false ; true ]
                let! depth = Gen.choose (0, 5)
                return name, isValueType, depth
            }

        let mutable cases = 0
        let mutable accepted = 0
        let mutable refused = 0
        let depths = HashSet<int> ()

        let property (name : string, isValueType : bool, depth : int) : unit =
            cases <- cases + 1
            depths.Add depth |> ignore

            let argument =
                List.replicate depth ()
                |> List.fold
                    (fun argument () -> Signature.Generic (true, Target.Holder, [ argument ]))
                    (Signature.Nominal (isValueType, Target.Core name))

            let answer =
                check $"GeneratedNominalEnum%d{cases}" [ genericEnum ; genericBacking argument ]
                |> List.last

            answer.IsSome |> shouldEqual (not isValueType)

            if answer.IsSome then
                accepted <- accepted + 1
            else
                refused <- refused + 1

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen generator) property)
        accepted |> shouldBeGreaterThan 50
        refused |> shouldBeGreaterThan 50
        depths.Count |> shouldEqual 6

    [<Test>]
    let ``Unsupported nominal kinds are checked throughout signatures with accumulated state`` () : unit =
        let arguments =
            [
                Signature.Nominal (false, Target.Plain)
                Signature.Nominal (false, Target.Core "Int32")
                Signature.Generic (false, Target.Holder, [ primitive 8uy ])
                Signature.Generic (false, Target.ExternalStruct, [ primitive 8uy ])
                for wrap in
                    [
                        Signature.Vector
                        Signature.Array
                        Signature.Pointer >> Signature.Vector
                        Signature.Byref >> Signature.Vector
                        Signature.FunctionReturning >> Signature.Vector
                        Signature.FunctionTaking >> Signature.Vector
                        Signature.Pinned >> Signature.Vector
                        (fun argument -> Signature.Modified (true, argument))
                    ] do
                    wrap (Signature.Nominal (true, Target.Core "String"))
            ]

        let image =
            emit "SkippedNominalEnumArguments" (genericEnum :: List.map genericBacking arguments)

        use stream = new MemoryStream (image)

        let dumped =
            Assembly.read loggerFactory (Some "SkippedNominalEnumArguments.dll") stream

        for index in 1 .. arguments.Length do
            let state, handle =
                concretize (baseState.Force().WithLoadedAssembly dumped) dumped index PrimitiveType.Int32

            let next, actual =
                EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle

            match actual with
            | Error (TypeBindingFailure.Unmodelled failure) ->
                Assert.That (failure.Diagnostic, Does.Contain "nominal kind inconsistent")
            | other -> failwith $"Expected an unsupported nominal spelling for %A{arguments.[index - 1]}, got %A{other}"

            next.LoadedAssembly dumped.DefinitionFullName
            |> Option.isSome
            |> shouldEqual true

            TypeSystemState.tryGetConcreteTypeInfo next handle
            |> Option.isSome
            |> shouldEqual true

            if index = 4 then
                next.LoadedAssembly typeof<ImmutableArray<int>>.Assembly.FullName
                |> Option.isSome
                |> shouldEqual true

    [<Test>]
    let ``Input enum generic argument kinds agree with CoreCLR`` () : unit =
        let image = emit "InputEnumArguments" [ genericEnum ]
        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some "InputEnumArguments.dll") stream
        let context = AssemblyLoadContext ("InputEnumArguments", isCollectible = true)

        try
            use hostStream = new MemoryStream (image)
            let host = context.LoadFromStream hostStream
            let definition = host.GetType ("Outer`1+E0", throwOnError = true)
            let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32

            for argument, hostArgument, legal in
                [
                    int32, typeof<int>, true
                    TypeDefn.PrimitiveType PrimitiveType.String, typeof<string>, true
                    TypeDefn.OneDimensionalArrayLowerBoundZero int32, typeof<int[]>, true
                    TypeDefn.Pointer int32, typeof<int>.MakePointerType (), false
                    TypeDefn.Byref int32, typeof<int>.MakeByRefType (), false
                    TypeDefn.FromDefinition (bct.Void.Identity, SignatureTypeKind.ValueType), typeof<Void>, false
                    TypeDefn.PrimitiveType PrimitiveType.TypedReference,
                    typeof<obj>.Assembly.GetType ("System.TypedReference", throwOnError = true),
                    false
                ] do
                let expected =
                    try
                        let closed = definition.MakeGenericType [| hostArgument |]
                        Enum.GetUnderlyingType closed |> shouldEqual typeof<int>
                        true
                    with
                    | :? ArgumentException
                    | :? TypeLoadException -> false

                expected |> shouldEqual legal

                let signature =
                    TypeDefn.GenericInstantiation (
                        TypeDefn.FromDefinition (
                            dumped.TypeDefs.[MetadataTokens.TypeDefinitionHandle 5].Identity,
                            SignatureTypeKind.ValueType
                        ),
                        ImmutableArray.Create argument
                    )

                let state, handle =
                    TypeSystemState.concretizeType
                        loggerFactory
                        HostImage.runtimeDirs
                        bct
                        (baseState.Force().WithLoadedAssembly dumped)
                        dumped.DefinitionFullName
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        signature
                    |> BindingResult.bound

                let next, actual =
                    EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle

                match expected, actual with
                | true, Ok (Some PrimitiveType.Int32)
                | false, Error (TypeBindingFailure.Unmodelled _) -> ()
                | _ -> failwith $"Input enum argument %O{argument}: host=%b{expected}, actual=%A{actual}"

                next.LoadedAssembly dumped.DefinitionFullName
                |> Option.isSome
                |> shouldEqual true

                TypeSystemState.tryGetConcreteTypeInfo next handle
                |> Option.isSome
                |> shouldEqual true
        finally
            context.Unload ()

    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    let ``Illegal generic backing arguments agree with CoreCLR`` (index : int) : unit =
        let argument = illegalArguments.[index]

        let answers =
            check
                $"IllegalEnumArgument%d{index}"
                [
                    genericEnum
                    genericBacking argument
                    genericBacking (Signature.Generic (true, Target.Holder, [ argument ]))
                ]

        answers
        |> shouldEqual
            [
                Some PrimitiveType.Int32
                Some PrimitiveType.Int32
                Some PrimitiveType.Int32
                None
                None
            ]

    [<TestCase(false)>]
    [<TestCase(true)>]
    let ``Out of scope parameters inside enum backing arguments agree with CoreCLR`` (methodParameter : bool) : unit =
        let parameter =
            if methodParameter then
                Signature.MethodParameter
            else
                Signature.TypeParameter

        let arguments =
            [ parameter ; Signature.Generic (true, Target.Holder, [ parameter ]) ]

        let answers =
            check $"EnumParameterScope%b{methodParameter}" (genericEnum :: List.map genericBacking arguments)

        answers
        |> List.take 3
        |> shouldEqual (List.replicate 3 (Some PrimitiveType.Int32))

        answers |> List.skip 3 |> List.iter (shouldEqual None)

    [<Test>]
    let ``Unresolvable nested backing signatures return failures with accumulated state`` () : unit =
        let cases =
            [
                genericEnum
                genericBacking (Signature.Generic (true, Target.ExternalStruct, [ Signature.Pointer (primitive 8uy) ]))
                for parameter in [ Signature.TypeParameter ; Signature.MethodParameter ] do
                    for argument in
                        [
                            Signature.Vector parameter
                            Signature.Array parameter
                            Signature.Pointer parameter
                            Signature.Byref parameter
                            Signature.Pinned parameter
                            Signature.Modified (true, parameter)
                            Signature.FunctionReturning parameter
                            Signature.FunctionTaking parameter
                        ] do
                        genericBacking argument
                for argument in illegalArguments do
                    genericBacking (Signature.Vector (Signature.Generic (true, Target.Holder, [ argument ])))
                { genericBacking (Signature.TypeParameterAt 1) with
                    Nested = true
                    GenericArity = 1
                }
            ]

        let image = emit "UnresolvableEnumArguments" cases
        use stream = new MemoryStream (image)

        let dumped =
            Assembly.read loggerFactory (Some "UnresolvableEnumArguments.dll") stream

        let mutable state = baseState.Force().WithLoadedAssembly dumped

        state.LoadedAssembly typeof<ImmutableArray<int>>.Assembly.FullName
        |> Option.isNone
        |> shouldEqual true

        // The first refusal loads an assembly while resolving its backing signature.
        // Subsequent preflight refusals must retain it too.
        for index in 1 .. cases.Length - 1 do
            let next, handle = concretize state dumped index PrimitiveType.Int32

            let next, result =
                EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct next handle

            match result with
            | Error (TypeBindingFailure.Unmodelled failure) ->
                Assert.That (failure.Diagnostic, Does.Contain "PawPrint cannot normalize")
            | other -> failwith $"Expected a refusal for %A{cases.[index]}, got %A{other}"

            next.LoadedAssembly dumped.DefinitionFullName
            |> Option.isSome
            |> shouldEqual true

            TypeSystemState.tryGetConcreteTypeInfo next handle
            |> Option.isSome
            |> shouldEqual true

            next.LoadedAssembly typeof<ImmutableArray<int>>.Assembly.FullName
            |> Option.isSome
            |> shouldEqual true

            state <- next

    [<Test>]
    let ``Valid array arguments in enum backing signatures agree with CoreCLR`` () : unit =
        let cases =
            [
                genericEnum
                for argument in
                    [
                        Signature.Vector (primitive 8uy)
                        Signature.Array (primitive 8uy)
                        Signature.Vector (Signature.Pointer (primitive 8uy))
                        Signature.Vector (Signature.Generic (true, Target.Holder, [ primitive 8uy ]))
                    ] do
                    genericBacking argument
            ]

        check "ValidArrayEnumArguments" cases
        |> List.iter (shouldEqual (Some PrimitiveType.Int32))

    [<Test>]
    let ``Generated generic backing signatures agree with CoreCLR`` () : unit =
        let rec wrap (depth : int) (argument : Signature) : Signature =
            if depth = 0 then
                argument
            else
                Signature.Generic (true, Target.Holder, [ wrap (depth - 1) argument ])

        let generator =
            gen {
                let! argument =
                    Gen.elements (
                        primitive 8uy
                        :: Signature.TypeParameter
                        :: Signature.MethodParameter
                        :: illegalArguments
                    )

                let! depth = Gen.choose (0, 5)
                let argument = wrap depth argument
                let! genericOwner = Gen.elements [ false ; true ]
                return argument, genericOwner
            }

        let mutable cases = 0
        let mutable accepted = 0
        let mutable refused = 0

        let property (argument : Signature, genericOwner : bool) : unit =
            cases <- cases + 1

            let owner =
                { genericBacking argument with
                    Nested = genericOwner
                    GenericArity = if genericOwner then 1 else 0
                }

            for answer in check $"GeneratedGenericEnums%d{cases}" [ genericEnum ; owner ] |> List.skip 3 do
                if answer.IsSome then
                    accepted <- accepted + 1
                else
                    refused <- refused + 1

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen generator) property)
        accepted |> shouldBeGreaterThan 20
        refused |> shouldBeGreaterThan 20

    let private genSpecs : Gen<Spec list> =
        gen {
            let! length = Gen.choose (3, 12)
            let! nestingWeight = Gen.choose (1, 8)
            let! invalidWeight = Gen.choose (1, 4)

            let! cases =
                [ 0 .. length - 1 ]
                |> List.map (fun index ->
                    gen {
                        let! name, code, _ = Gen.elements primitives

                        let! signature =
                            Gen.frequency
                                [
                                    5, Gen.constant (primitive code)
                                    invalidWeight,
                                    Gen.elements
                                        [
                                            Signature.Nominal (true, Target.Core name)
                                            Signature.Nominal (false, Target.Core name)
                                            primitive 28uy
                                            Signature.Nominal (true, Target.Plain)
                                        ]
                                    (if index = 0 then 0 else nestingWeight),
                                    Gen.choose (0, max 0 (index - 1)) |> Gen.map enumRef
                                ]

                        let! signature =
                            Gen.elements
                                [
                                    signature
                                    Signature.Modified (true, signature)
                                    Signature.Modified (false, signature)
                                ]

                        let! layout =
                            Gen.frequency
                                [
                                    8, Gen.constant TypeAttributes.AutoLayout
                                    1, Gen.constant TypeAttributes.SequentialLayout
                                    1, Gen.constant TypeAttributes.ExplicitLayout
                                ]

                        let! count = Gen.frequency [ 8, Gen.constant 1 ; 1, Gen.constant 0 ; 1, Gen.constant 2 ]
                        let! privateField = Gen.elements [ false ; true ]
                        let! staticField = Gen.elements [ false ; true ]
                        let! literalField = Gen.elements [ false ; true ]

                        let fields =
                            [
                                for i in 1..count do
                                    yield
                                        { field signature with
                                            Name = $"storage%d{i}"
                                            Attributes =
                                                if privateField then
                                                    FieldAttributes.Private
                                                else
                                                    FieldAttributes.Public
                                        }
                                if staticField then
                                    yield
                                        { field (primitive 8uy) with
                                            Name = "Static"
                                            Attributes = FieldAttributes.Public ||| FieldAttributes.Static
                                        }
                                if literalField then
                                    yield
                                        { field (primitive 8uy) with
                                            Name = "Literal"
                                            Attributes = FieldAttributes.Public ||| FieldAttributes.Literal
                                        }
                            ]

                        return
                            { spec [] with
                                Layout = layout
                                Fields = fields
                            }
                    }
                )
                |> Gen.sequenceToList

            return cases
        }

    let private shrinkSpecs (specs : Spec list) : Spec list seq =
        seq {
            // Generated references point backwards, so dropping the last enum keeps them valid.
            if specs.Length > 1 then
                yield List.take (specs.Length - 1) specs

            for index, spec in List.indexed specs do
                for field in 0 .. spec.Fields.Length - 1 do
                    yield
                        List.updateAt
                            index
                            { spec with
                                Fields = List.removeAt field spec.Fields
                            }
                            specs
        }

    [<Test>]
    let ``Generated enum graphs agree with host backing types and refusals`` () : unit =
        let mutable cases = 0
        let mutable accepted = 0
        let mutable refused = 0
        let seen = HashSet<PrimitiveType> ()

        let property (specs : Spec list) : unit =
            cases <- cases + 1

            for answer in check $"GeneratedEnums%d{cases}" specs do
                match answer with
                | Some primitive ->
                    accepted <- accepted + 1
                    seen.Add primitive |> ignore
                | None -> refused <- refused + 1

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGenShrink (genSpecs, shrinkSpecs)) property
        )

        accepted |> shouldBeGreaterThan 100
        refused |> shouldBeGreaterThan 100
        seen.Count |> shouldEqual primitives.Length

        TestContext.WriteLine
            $"%s{RuntimeInformation.FrameworkDescription}: %d{cases} graphs, %d{accepted} accepted enums, %d{refused} refused enums, %d{seen.Count} distinct primitive results."

    [<Test>]
    let ``Non-enums return None without manufacturing an underlying type`` () : unit =
        for signature in
            [
                TypeDefn.PrimitiveType PrimitiveType.Int32
                TypeDefn.PrimitiveType PrimitiveType.String
                TypeDefn.Pointer (TypeDefn.PrimitiveType PrimitiveType.Int32)
                TypeDefn.Byref (TypeDefn.PrimitiveType PrimitiveType.Int32)
                TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.Int32)
            ] do
            let state, handle =
                TypeSystemState.concretizeType
                    loggerFactory
                    HostImage.runtimeDirs
                    bct
                    (baseState.Force ())
                    corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    signature
                |> BindingResult.bound

            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle
            |> snd
            |> shouldEqual (Ok None)

    [<Test>]
    let ``Binding failures are results and preserve assemblies already loaded`` () : unit =
        let image =
            emit
                "EnumBinding"
                [
                    spec [ Signature.Generic (true, Target.ExternalStruct, [ primitive 8uy ]) ]
                    spec [ Signature.Nominal (true, Target.Missing) ]
                    spec [ Signature.Nominal (true, Target.MissingType) ]
                ]

        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some "EnumBinding.dll") stream
        let state = baseState.Force().WithLoadedAssembly dumped
        let state, first = concretize state dumped 0 PrimitiveType.Int32

        let state, result =
            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state first

        match result with
        | Error (TypeBindingFailure.Unmodelled _) -> ()
        | other -> failwith $"Expected a non-enum backing-field refusal, got %A{other}"

        state.LoadedAssembly typeof<ImmutableArray<int>>.Assembly.FullName
        |> Option.isSome
        |> shouldEqual true

        let fresh, third =
            concretize (baseState.Force().WithLoadedAssembly dumped) dumped 2 PrimitiveType.Int32

        fresh.LoadedAssembly typeof<ImmutableArray<int>>.Assembly.FullName
        |> Option.isNone
        |> shouldEqual true

        let failedState, result =
            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct fresh third

        match result with
        | Error (TypeBindingFailure.Unmodelled _) -> failwith "A missing type must remain a binding failure"
        | Error _ -> ()
        | other -> failwith $"Expected a missing-type binding failure, got %A{other}"

        failedState.LoadedAssembly typeof<ImmutableArray<int>>.Assembly.FullName
        |> Option.isSome
        |> shouldEqual true

        let state, second = concretize state dumped 1 PrimitiveType.Int32

        let state, result =
            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state second

        match result with
        | Error (TypeBindingFailure.Unmodelled _) -> failwith "A missing assembly must remain a binding failure"
        | Error _ -> ()
        | other -> failwith $"Expected a missing-assembly binding failure, got %A{other}"

        state.LoadedAssembly typeof<ImmutableArray<int>>.Assembly.FullName
        |> Option.isSome
        |> shouldEqual true

    [<TestCase(false)>]
    [<TestCase(true)>]
    let ``Shared enum argument graphs are validated within a bounded allocation budget`` (malformed : bool) : unit =
        let image = emit "SharedEnumArguments" [ genericEnum ]
        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some "SharedEnumArguments.dll") stream

        let tuple =
            corelib.TryGetTopLevelTypeDef "System" "Tuple`2"
            |> Option.get
            |> fun ty -> TypeDefn.FromDefinition (ty.Identity, SignatureTypeKind.Class)

        let instantiate
            (state : TypeSystemState)
            (arguments : ImmutableArray<ConcreteTypeHandle>)
            (signature : TypeDefn)
            : TypeSystemState * ConcreteTypeHandle
            =
            TypeSystemState.concretizeType
                loggerFactory
                HostImage.runtimeDirs
                bct
                state
                dumped.DefinitionFullName
                arguments
                ImmutableArray.Empty
                signature
            |> BindingResult.bound

        let state, leaf =
            instantiate
                (baseState.Force().WithLoadedAssembly dumped)
                ImmutableArray.Empty
                (TypeDefn.PrimitiveType PrimitiveType.Int32)

        let shared =
            TypeDefn.GenericInstantiation (
                tuple,
                ImmutableArray.Create (TypeDefn.GenericTypeParameter 0, TypeDefn.GenericTypeParameter 0)
            )

        // Twenty nominal handles represent over a million paths to the same leaf.
        let state, argument =
            [ 1..20 ]
            |> List.fold
                (fun (state, previous) _ -> instantiate state (ImmutableArray.Create previous) shared)
                (state, leaf)

        let state, argument =
            if malformed then
                // Validate the shared subtree before discovering an illegal argument in a sibling.
                let invalid =
                    TypeDefn.GenericInstantiation (
                        tuple,
                        ImmutableArray.Create (
                            TypeDefn.PrimitiveType PrimitiveType.Int32,
                            TypeDefn.Pointer (TypeDefn.PrimitiveType PrimitiveType.Int32)
                        )
                    )

                instantiate
                    state
                    (ImmutableArray.Create argument)
                    (TypeDefn.GenericInstantiation (
                        tuple,
                        ImmutableArray.Create (TypeDefn.GenericTypeParameter 0, invalid)
                    ))
            else
                state, argument

        let state, handle =
            instantiate
                state
                (ImmutableArray.Create argument)
                (TypeDefn.GenericInstantiation (
                    TypeDefn.FromDefinition (
                        dumped.TypeDefs.[MetadataTokens.TypeDefinitionHandle 5].Identity,
                        SignatureTypeKind.ValueType
                    ),
                    ImmutableArray.Create (TypeDefn.GenericTypeParameter 0)
                ))

        let before = GC.GetAllocatedBytesForCurrentThread ()

        let _, result =
            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle

        let allocated = GC.GetAllocatedBytesForCurrentThread () - before
        TestContext.WriteLine $"Shared enum argument validation allocated %d{allocated} bytes"

        // Allocation detects repeated traversal without depending on machine speed or load.
        allocated |> shouldBeSmallerThan (4L * 1024L * 1024L)

        match malformed, result with
        | false, Ok (Some PrimitiveType.Int32) -> ()
        | true, Error (TypeBindingFailure.Unmodelled failure) ->
            Assert.That (failure.Diagnostic, Does.Contain "malformed generic instantiation")
        | _ -> failwith $"Unexpected shared argument validation result: malformed=%b{malformed}, result=%A{result}"

    [<Test>]
    let ``Shared expanding backing arguments reach the enum traversal limit`` () : unit =
        let expanding =
            { spec
                  [
                      Signature.Generic (
                          true,
                          Target.Enum 0,
                          [
                              Signature.Generic (
                                  false,
                                  Target.Core "Tuple`2",
                                  [ Signature.TypeParameter ; Signature.TypeParameter ]
                              )
                          ]
                      )
                  ] with
                Nested = true
                GenericArity = 1
            }

        let image = emit "SharedExpandingEnum" [ expanding ]
        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some "SharedExpandingEnum.dll") stream

        let state, handle =
            concretize (baseState.Force().WithLoadedAssembly dumped) dumped 0 PrimitiveType.Int32

        match
            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle
            |> snd
        with
        | Error (TypeBindingFailure.Unmodelled failure) -> Assert.That (failure.Diagnostic, Does.Contain "64")
        | other -> failwith $"Expected the shared expanding chain to reach the enum-depth limit, got %A{other}"

    [<Test>]
    let ``Enum backing cycles are reported separately from the traversal limit`` () : unit =
        let image =
            emit "EnumCycles" [ spec [ enumRef 0 ] ; spec [ enumRef 2 ] ; spec [ enumRef 1 ] ]

        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some "EnumCycles.dll") stream

        for index in 0..2 do
            let state, handle =
                concretize (baseState.Force().WithLoadedAssembly dumped) dumped index PrimitiveType.Int32

            match
                EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle
                |> snd
            with
            | Error (TypeBindingFailure.Unmodelled failure) -> Assert.That (failure.Diagnostic, Does.Contain "cycle")
            | other -> failwith $"Expected a backing-field cycle, got %A{other}"

    [<Test>]
    let ``Enum nesting is bounded independently of generic argument nesting`` () : unit =
        let chain =
            [
                for index in 0..64 ->
                    spec
                        [
                            if index = 64 then primitive 8uy else enumRef (index + 1)
                        ]
            ]

        let image = emit "EnumDepth" chain
        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some "EnumDepth.dll") stream
        let context = AssemblyLoadContext ("EnumDepth", isCollectible = true)

        try
            use hostStream = new MemoryStream (image)
            let host = context.LoadFromStream hostStream

            hostAnswer host 0 chain.[0] typeof<int>
            |> shouldEqual (Some PrimitiveType.Int32)
        finally
            context.Unload ()

        let state = baseState.Force().WithLoadedAssembly dumped
        let state, handle = concretize state dumped 0 PrimitiveType.Int32

        let _, result =
            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle

        match result with
        | Error (TypeBindingFailure.Unmodelled failure) -> Assert.That (failure.Diagnostic, Does.Contain "64")
        | other -> failwith $"Expected the documented enum-depth limit, got %A{other}"

        let state, handle = concretize state dumped 1 PrimitiveType.Int32

        EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle
        |> snd
        |> shouldEqual (Ok (Some PrimitiveType.Int32))

        let expanding =
            { spec
                  [
                      Signature.Generic (
                          true,
                          Target.Enum 0,
                          [ Signature.Generic (true, Target.Holder, [ Signature.TypeParameter ]) ]
                      )
                  ] with
                Nested = true
                GenericArity = 1
            }

        let image = emit "EnumExpanding" [ expanding ]
        use stream = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some "EnumExpanding.dll") stream

        let state, handle =
            concretize (baseState.Force().WithLoadedAssembly dumped) dumped 0 PrimitiveType.Int32

        let _, result =
            EnumBackingType.tryResolve loggerFactory HostImage.runtimeDirs bct state handle

        match result with
        | Error (TypeBindingFailure.Unmodelled failure) -> Assert.That (failure.Diagnostic, Does.Contain "64")
        | other -> failwith $"Expected the expanding chain to reach the enum-depth limit, got %A{other}"

        let rec nestedArgument (depth : int) : Signature =
            if depth = 0 then
                primitive 8uy
            else
                Signature.Generic (true, Target.Holder, [ nestedArgument (depth - 1) ])

        check
            "EnumFiniteArgument"
            [
                { spec [ primitive 9uy ] with
                    Nested = true
                    GenericArity = 1
                }
                spec [ Signature.Generic (true, Target.Enum 0, [ nestedArgument 10 ]) ]
            ]
        |> ignore

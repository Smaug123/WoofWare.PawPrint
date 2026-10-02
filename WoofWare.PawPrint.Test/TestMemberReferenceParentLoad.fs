namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MemberReferenceParent.resolve` against the real runtime's answer to whether a MemberRef's parent
/// can be loaded at all.
///
/// `MemberLoader::GetDescFromMemberRef` loads the parent fully before it looks for the member, so a
/// parent that cannot be loaded makes binding the reference throw `TypeLoadException` whatever the
/// member. Each case here is a class that would load but for one reference to `System.Gone`, which
/// CoreLib does not declare, placed somewhere a full load might or might not reach; and a reference
/// to the class's static `s : int32`, which exists, so that the only way binding can fail is the
/// parent's load. The oracle is `Module.ResolveField` on the same bytes.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMemberReferenceParentLoad =

    /// Where a case puts its reference to `System.Gone`.
    [<RequireQualifiedAccess>]
    type Shape =
        /// Nothing: the control.
        | Control
        /// `class P : System.Gone`.
        | Base
        /// `class P : G<System.Gone>`.
        | BaseArgument
        /// `class P : Mid`, with `class Mid : System.Gone`.
        | GrandBase
        /// `class P : System.Gone` as an interface.
        | Interface
        /// `class P : I<System.Gone>`.
        | InterfaceArgument
        /// An instance field `valuetype System.Gone`.
        | InstanceValueField
        /// An instance field `class System.Gone`.
        | InstanceClassField
        /// An instance field `valuetype V<System.Gone>`.
        | InstanceValueFieldArgument
        /// An instance field `class G<System.Gone>`.
        | InstanceClassFieldArgument
        /// An instance field `valuetype S`, where `struct S` has an instance field `valuetype System.Gone`.
        | InstanceValueFieldTransitive
        /// An instance field `valuetype System.Gone*`.
        | InstancePointerField
        /// A static field `valuetype System.Gone`.
        | StaticValueField
        /// A static field `class System.Gone`.
        | StaticClassField
        /// A method taking `class System.Gone`.
        | MethodParameter
        /// A nested `class N : System.Gone`.
        | NestedBase
        /// `struct P` with an instance field `class System.Gone`.
        | StructInstanceClassField
        /// `class P : G<S>`: an argument whose own layout names `System.Gone`.
        | BaseArgumentLayout
        /// `class P : G<Mid>`: an argument whose base is `System.Gone`.
        | BaseArgumentBase
        /// `class P : WithInterface`, with `class WithInterface : System.Gone` as an interface.
        | BaseInterface
        /// `class P : WithStatic`, with `class WithStatic` having a static field `valuetype System.Gone`.
        | BaseStaticValueField
        /// An instance field `valuetype SInterface`, with `struct SInterface : System.Gone` as an interface.
        | InstanceValueFieldInterface
        /// An instance field `valuetype SStatic`, with `struct SStatic` having a static field
        /// `valuetype System.Gone`.
        | InstanceValueFieldStatic
        /// An instance field `class G<S>`.
        | InstanceClassFieldArgumentLayout
        /// `struct P` with a static field of its own type, and no reference to `System.Gone`.
        | SelfStaticValueField
        /// `class P : System.Gone`, with `System.Gone2` as an interface: which is loaded first.
        | OrderBaseThenInterface
        /// `System.Gone2` as an interface, and an instance field `valuetype System.Gone`.
        | OrderInterfaceThenField
        /// A static field `valuetype System.Gone2`, then an instance field `valuetype System.Gone`.
        | OrderStaticThenInstanceField
        /// An instance field `valuetype System.Gone2`, then a static field `valuetype System.Gone`.
        | OrderInstanceThenStaticField
        /// `class P : G2<System.Gone2, System.Gone>`.
        | OrderArguments
        /// `class P : WithStatic`, with `System.Gone2` as an interface: the base's static field, or
        /// the type's own interface.
        | OrderBaseFieldThenInterface
        /// `class P : WithStatic`, with an instance field `valuetype System.Gone2`.
        | OrderBaseFieldThenField
        /// The parent is `GBad<System.Gone2>`, with `class GBad`1 : System.Gone`.
        | OrderArgumentThenDefinitionBase
        /// A literal static field `valuetype System.Gone`, which has no `FieldDesc`.
        | LiteralValueField
        /// A static field `valuetype E`, where enum `E` implements `System.Gone`.
        | StaticEnumField
        /// An instance field `valuetype E`, where enum `E` implements `System.Gone`.
        | InstanceEnumField
        /// A `[ThreadStatic]` field `valuetype System.Gone2`, then a static field `valuetype System.Gone`.
        | OrderThreadStaticThenStatic
        /// A static field `valuetype GN<System.Gone>.E`, an enum nested in a generic class.
        | StaticNestedEnumOfGeneric
        /// The parent is `G3<S2>`: `G3<T>` has a static field `valuetype System.Gone`, and the
        /// argument `S2` a static field `valuetype System.Gone2`.
        | TypeSpecParentArgumentFirst
        /// The parent is `System.Gone[]`.
        | ArrayOfMissing
        /// The parent is `S[]`, whose element's layout names `System.Gone`.
        | ArrayElementLayout
        /// The parent is `Mid[]`, whose element's base is `System.Gone`.
        | ArrayElementBase

    let shapes : Shape list =
        [
            Shape.Control
            Shape.Base
            Shape.BaseArgument
            Shape.GrandBase
            Shape.Interface
            Shape.InterfaceArgument
            Shape.InstanceValueField
            Shape.InstanceClassField
            Shape.InstanceValueFieldArgument
            Shape.InstanceClassFieldArgument
            Shape.InstanceValueFieldTransitive
            Shape.InstancePointerField
            Shape.StaticValueField
            Shape.StaticClassField
            Shape.MethodParameter
            Shape.NestedBase
            Shape.StructInstanceClassField
            Shape.BaseArgumentLayout
            Shape.BaseArgumentBase
            Shape.BaseInterface
            Shape.BaseStaticValueField
            Shape.InstanceValueFieldInterface
            Shape.InstanceValueFieldStatic
            Shape.InstanceClassFieldArgumentLayout
            Shape.SelfStaticValueField
            Shape.OrderBaseThenInterface
            Shape.OrderInterfaceThenField
            Shape.OrderStaticThenInstanceField
            Shape.OrderInstanceThenStaticField
            Shape.OrderArguments
            Shape.OrderBaseFieldThenInterface
            Shape.OrderBaseFieldThenField
            Shape.OrderArgumentThenDefinitionBase
            Shape.LiteralValueField
            Shape.StaticEnumField
            Shape.InstanceEnumField
            Shape.OrderThreadStaticThenStatic
            Shape.StaticNestedEnumOfGeneric
            Shape.TypeSpecParentArgumentFirst
            Shape.ArrayOfMissing
            Shape.ArrayElementLayout
            Shape.ArrayElementBase
        ]

    /// One type definition to emit.
    type private TypeSpecRow =
        {
            Name : string
            Attributes : TypeAttributes
            BaseType : EntityHandle
            Fields : (FieldAttributes * string * BlobHandle) list
            Interfaces : EntityHandle list
            /// Methods, each a name and a signature; every body is `ret`.
            Methods : (string * BlobHandle) list
            GenericParameters : string list
            EnclosingRow : int option
        }

    /// The image, and for each shape the MemberRef naming its case's `s`.
    let private emit () : byte[] * Map<Shape, MemberReferenceHandle> =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "ParentLoad.dll",
            metadata.GetOrAddGuid (Guid "5d0b8e21-7c3f-4a96-b1e2-9f4c6a3d8b17"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "ParentLoad",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let corelibType (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = corelibType "Object"
        let valueTypeRef = corelibType "ValueType"
        let goneRef = corelibType "Gone"
        let gone2Ref = corelibType "Gone2"

        let threadStaticCtor : EntityHandle =
            metadata.AddMemberReference (
                corelibType "ThreadStaticAttribute",
                metadata.GetOrAddString ".ctor",
                // HASTHIS, no parameters, returning void.
                metadata.GetOrAddBlob [| 0x20uy ; 0x00uy ; 0x01uy |]
            )
            |> MemberReferenceHandle.op_Implicit

        let fieldSig (encode : SignatureTypeEncoder -> unit) : BlobHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).Field().Type ())
            metadata.GetOrAddBlob blob

        let int32Sig = fieldSig (fun t -> t.Int32 ())

        let typeSpec (encode : SignatureTypeEncoder -> unit) : EntityHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).TypeSpecificationSignature ())
            TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        // Row numbers are fixed in advance so that types can name each other before they exist.
        // `<Module>` is row 1.
        let helperNames =
            [
                "G`1"
                "V`1"
                "I`1"
                "Mid"
                "S"
                "WithInterface"
                "WithStatic"
                "SInterface"
                "SStatic"
                "G2`2"
                "GBad`1"
                "E"
                "GN`1"
                "G3`1"
                "S2"
            ]

        let caseRow (shape : Shape) : int =
            2 + helperNames.Length + List.findIndex ((=) shape) shapes

        let nestedRow = 2 + helperNames.Length + shapes.Length

        let typeDef (row : int) : EntityHandle =
            MetadataTokens.TypeDefinitionHandle row |> TypeDefinitionHandle.op_Implicit

        let helperRow (name : string) : int =
            2 + List.findIndex ((=) name) helperNames

        let gHandle = typeDef (helperRow "G`1")
        let vHandle = typeDef (helperRow "V`1")
        let iHandle = typeDef (helperRow "I`1")
        let midHandle = typeDef (helperRow "Mid")
        let sHandle = typeDef (helperRow "S")
        let withInterfaceHandle = typeDef (helperRow "WithInterface")
        let withStaticHandle = typeDef (helperRow "WithStatic")
        let sInterfaceHandle = typeDef (helperRow "SInterface")
        let sStaticHandle = typeDef (helperRow "SStatic")
        let g2Handle = typeDef (helperRow "G2`2")
        let gBadHandle = typeDef (helperRow "GBad`1")
        let eHandle = typeDef (helperRow "E")
        let gnHandle = typeDef (helperRow "GN`1")
        let g3Handle = typeDef (helperRow "G3`1")
        let s2Handle = typeDef (helperRow "S2")
        let nestedEnumRow = nestedRow + 1

        let instantiateWith (generic : EntityHandle) (argument : EntityHandle) (argumentIsValueType : bool) =
            typeSpec (fun t ->
                t.GenericInstantiation(generic, 1, false).AddArgument().Type (argument, argumentIsValueType)
            )

        let instantiate (generic : EntityHandle) (isValueType : bool) : EntityHandle =
            typeSpec (fun t -> t.GenericInstantiation(generic, 1, isValueType).AddArgument().Type (goneRef, false))

        let publicStatic = FieldAttributes.Public ||| FieldAttributes.Static

        let structAttributes =
            TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.SequentialLayout

        let interfaceAttributes =
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract

        let classOf (name : string) : TypeSpecRow =
            {
                Name = name
                Attributes = TypeAttributes.Public
                BaseType = objectRef
                Fields = [ publicStatic, "s", int32Sig ]
                Interfaces = []
                Methods = []
                GenericParameters = []
                EnclosingRow = None
            }

        let helpers : TypeSpecRow list =
            [
                { classOf "G`1" with
                    Fields = []
                    GenericParameters = [ "T" ]
                }
                { classOf "V`1" with
                    Attributes = structAttributes
                    BaseType = valueTypeRef
                    Fields = [ FieldAttributes.Public, "x", int32Sig ]
                    GenericParameters = [ "T" ]
                }
                { classOf "I`1" with
                    Attributes = interfaceAttributes
                    BaseType = Unchecked.defaultof<EntityHandle>
                    Fields = []
                    GenericParameters = [ "T" ]
                }
                { classOf "Mid" with
                    BaseType = goneRef
                    Fields = []
                }
                { classOf "S" with
                    Attributes = structAttributes
                    BaseType = valueTypeRef
                    Fields = [ FieldAttributes.Public, "g", fieldSig (fun t -> t.Type (goneRef, true)) ]
                }
                { classOf "WithInterface" with
                    Fields = []
                    Interfaces = [ goneRef ]
                }
                { classOf "WithStatic" with
                    Fields = [ publicStatic, "g", fieldSig (fun t -> t.Type (goneRef, true)) ]
                }
                { classOf "SInterface" with
                    Attributes = structAttributes
                    BaseType = valueTypeRef
                    Fields = [ FieldAttributes.Public, "x", int32Sig ]
                    Interfaces = [ goneRef ]
                }
                { classOf "SStatic" with
                    Attributes = structAttributes
                    BaseType = valueTypeRef
                    Fields =
                        [
                            FieldAttributes.Public, "x", int32Sig
                            publicStatic, "g", fieldSig (fun t -> t.Type (goneRef, true))
                        ]
                }
                { classOf "G2`2" with
                    Fields = []
                    GenericParameters = [ "T" ; "U" ]
                }
                { classOf "GBad`1" with
                    BaseType = goneRef
                    GenericParameters = [ "T" ]
                }
                { classOf "E" with
                    Attributes = TypeAttributes.Public ||| TypeAttributes.Sealed
                    BaseType = corelibType "Enum"
                    Fields =
                        [
                            FieldAttributes.Public
                            ||| FieldAttributes.SpecialName
                            ||| FieldAttributes.RTSpecialName,
                            "value__",
                            int32Sig
                        ]
                    Interfaces = [ goneRef ]
                }
                { classOf "GN`1" with
                    Fields = []
                    GenericParameters = [ "T" ]
                }
                { classOf "G3`1" with
                    Fields =
                        [
                            publicStatic, "s", int32Sig
                            publicStatic, "g", fieldSig (fun t -> t.Type (goneRef, true))
                        ]
                    GenericParameters = [ "T" ]
                }
                { classOf "S2" with
                    Attributes = structAttributes
                    BaseType = valueTypeRef
                    Fields =
                        [
                            FieldAttributes.Public, "x", int32Sig
                            publicStatic, "g", fieldSig (fun t -> t.Type (gone2Ref, true))
                        ]
                }
            ]

        let caseOf (shape : Shape) : TypeSpecRow =
            let name = $"P%O{shape}"
            let row = classOf name

            let withField attributes fieldName signature =
                { row with
                    Fields = row.Fields @ [ attributes, fieldName, signature ]
                }

            match shape with
            | Shape.Control -> row
            | Shape.Base ->
                { row with
                    BaseType = goneRef
                }
            | Shape.BaseArgument ->
                { row with
                    BaseType = instantiate gHandle false
                }
            | Shape.GrandBase ->
                { row with
                    BaseType = midHandle
                }
            | Shape.Interface ->
                { row with
                    Interfaces = [ goneRef ]
                }
            | Shape.InterfaceArgument ->
                { row with
                    Interfaces = [ instantiate iHandle false ]
                }
            | Shape.InstanceValueField ->
                withField FieldAttributes.Public "f" (fieldSig (fun t -> t.Type (goneRef, true)))
            | Shape.InstanceClassField ->
                withField FieldAttributes.Public "f" (fieldSig (fun t -> t.Type (goneRef, false)))
            | Shape.InstanceValueFieldArgument ->
                withField
                    FieldAttributes.Public
                    "f"
                    (fieldSig (fun t -> t.GenericInstantiation(vHandle, 1, true).AddArgument().Type (goneRef, false)))
            | Shape.InstanceClassFieldArgument ->
                withField
                    FieldAttributes.Public
                    "f"
                    (fieldSig (fun t -> t.GenericInstantiation(gHandle, 1, false).AddArgument().Type (goneRef, false)))
            | Shape.InstanceValueFieldTransitive ->
                withField FieldAttributes.Public "f" (fieldSig (fun t -> t.Type (sHandle, true)))
            | Shape.InstancePointerField ->
                withField FieldAttributes.Public "f" (fieldSig (fun t -> t.Pointer().Type (goneRef, true)))
            | Shape.StaticValueField -> withField publicStatic "f" (fieldSig (fun t -> t.Type (goneRef, true)))
            | Shape.StaticClassField -> withField publicStatic "f" (fieldSig (fun t -> t.Type (goneRef, false)))
            | Shape.MethodParameter ->
                let signature =
                    let blob = BlobBuilder ()

                    BlobEncoder(blob)
                        .MethodSignature()
                        .Parameters (
                            1,
                            (fun (ret : ReturnTypeEncoder) -> ret.Void ()),
                            fun (parameters : ParametersEncoder) ->
                                parameters.AddParameter().Type().Type (goneRef, false)
                        )

                    metadata.GetOrAddBlob blob

                { row with
                    Methods = [ "M", signature ]
                }
            | Shape.NestedBase -> row
            | Shape.BaseArgumentLayout ->
                { row with
                    BaseType = instantiateWith gHandle sHandle true
                }
            | Shape.BaseArgumentBase ->
                { row with
                    BaseType = instantiateWith gHandle midHandle false
                }
            | Shape.BaseInterface ->
                { row with
                    BaseType = withInterfaceHandle
                }
            | Shape.BaseStaticValueField ->
                { row with
                    BaseType = withStaticHandle
                }
            | Shape.InstanceValueFieldInterface ->
                withField FieldAttributes.Public "f" (fieldSig (fun t -> t.Type (sInterfaceHandle, true)))
            | Shape.InstanceValueFieldStatic ->
                withField FieldAttributes.Public "f" (fieldSig (fun t -> t.Type (sStaticHandle, true)))
            | Shape.InstanceClassFieldArgumentLayout ->
                withField
                    FieldAttributes.Public
                    "f"
                    (fieldSig (fun t -> t.GenericInstantiation(gHandle, 1, false).AddArgument().Type (sHandle, true)))
            | Shape.OrderBaseThenInterface ->
                { row with
                    BaseType = goneRef
                    Interfaces = [ gone2Ref ]
                }
            | Shape.OrderInterfaceThenField ->
                { row with
                    Interfaces = [ gone2Ref ]
                    Fields =
                        row.Fields
                        @ [ FieldAttributes.Public, "f", fieldSig (fun t -> t.Type (goneRef, true)) ]
                }
            | Shape.OrderStaticThenInstanceField ->
                { row with
                    Fields =
                        row.Fields
                        @ [
                            publicStatic, "f", fieldSig (fun t -> t.Type (gone2Ref, true))
                            FieldAttributes.Public, "g", fieldSig (fun t -> t.Type (goneRef, true))
                        ]
                }
            | Shape.OrderInstanceThenStaticField ->
                { row with
                    Fields =
                        row.Fields
                        @ [
                            FieldAttributes.Public, "f", fieldSig (fun t -> t.Type (gone2Ref, true))
                            publicStatic, "g", fieldSig (fun t -> t.Type (goneRef, true))
                        ]
                }
            | Shape.OrderArguments ->
                { row with
                    BaseType =
                        typeSpec (fun t ->
                            let arguments = t.GenericInstantiation (g2Handle, 2, false)
                            arguments.AddArgument().Type (gone2Ref, false)
                            arguments.AddArgument().Type (goneRef, false)
                        )
                }
            | Shape.OrderBaseFieldThenInterface ->
                { row with
                    BaseType = withStaticHandle
                    Interfaces = [ gone2Ref ]
                }
            | Shape.OrderBaseFieldThenField ->
                { row with
                    BaseType = withStaticHandle
                    Fields =
                        row.Fields
                        @ [ FieldAttributes.Public, "f", fieldSig (fun t -> t.Type (gone2Ref, true)) ]
                }
            | Shape.OrderArgumentThenDefinitionBase
            | Shape.LiteralValueField ->
                withField
                    (publicStatic ||| FieldAttributes.Literal ||| FieldAttributes.HasDefault)
                    "f"
                    (fieldSig (fun t -> t.Type (goneRef, true)))
            | Shape.StaticEnumField -> withField publicStatic "f" (fieldSig (fun t -> t.Type (eHandle, true)))
            | Shape.InstanceEnumField ->
                withField FieldAttributes.Public "f" (fieldSig (fun t -> t.Type (eHandle, true)))
            | Shape.OrderThreadStaticThenStatic ->
                { row with
                    Fields =
                        row.Fields
                        @ [
                            publicStatic, "ts_f", fieldSig (fun t -> t.Type (gone2Ref, true))
                            publicStatic, "g", fieldSig (fun t -> t.Type (goneRef, true))
                        ]
                }
            | Shape.StaticNestedEnumOfGeneric ->
                withField
                    publicStatic
                    "f"
                    (fieldSig (fun t ->
                        t.GenericInstantiation(typeDef nestedEnumRow, 1, true).AddArgument().Type (goneRef, false)
                    ))
            | Shape.TypeSpecParentArgumentFirst
            | Shape.ArrayOfMissing
            | Shape.ArrayElementLayout
            | Shape.ArrayElementBase -> row
            | Shape.SelfStaticValueField ->
                { row with
                    Attributes = structAttributes
                    BaseType = valueTypeRef
                    Fields =
                        row.Fields
                        @ [
                            publicStatic, "self", fieldSig (fun t -> t.Type (typeDef (caseRow shape), true))
                        ]
                }
            | Shape.StructInstanceClassField ->
                { row with
                    Attributes = structAttributes
                    BaseType = valueTypeRef
                    Fields =
                        row.Fields
                        @ [ FieldAttributes.Public, "f", fieldSig (fun t -> t.Type (goneRef, false)) ]
                }

        let nested : TypeSpecRow =
            { classOf "N" with
                Attributes = TypeAttributes.NestedPublic
                BaseType = goneRef
                Fields = []
                EnclosingRow = Some (caseRow Shape.NestedBase)
            }

        // `E`1`, an enum nested in `GN`1`, which as a type nested in a generic one redeclares its
        // generic parameter.
        let nestedEnum : TypeSpecRow =
            { classOf "E`1" with
                Attributes = TypeAttributes.NestedPublic ||| TypeAttributes.Sealed
                BaseType = corelibType "Enum"
                Fields =
                    [
                        FieldAttributes.Public
                        ||| FieldAttributes.SpecialName
                        ||| FieldAttributes.RTSpecialName,
                        "value__",
                        int32Sig
                    ]
                GenericParameters = [ "T" ]
                EnclosingRow = Some (helperRow "GN`1")
            }

        let rows = helpers @ List.map caseOf shapes @ [ nested ; nestedEnum ]

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        for index, row in List.indexed rows do
            let firstField =
                MetadataTokens.FieldDefinitionHandle (metadata.GetRowCount TableIndex.Field + 1)

            for attributes, name, signature in row.Fields do
                let field =
                    metadata.AddFieldDefinition (attributes, metadata.GetOrAddString name, signature)

                if attributes.HasFlag FieldAttributes.Literal then
                    metadata.AddConstant ((FieldDefinitionHandle.op_Implicit field : EntityHandle), box 0)
                    |> ignore<ConstantHandle>

                // A field named `ts_...` is `[ThreadStatic]`.
                if name.StartsWith ("ts_", StringComparison.Ordinal) then
                    metadata.AddCustomAttribute (
                        (FieldDefinitionHandle.op_Implicit field : EntityHandle),
                        threadStaticCtor,
                        metadata.GetOrAddBlob [| 0x01uy ; 0x00uy ; 0x00uy ; 0x00uy |]
                    )
                    |> ignore<CustomAttributeHandle>

            let firstMethod =
                MetadataTokens.MethodDefinitionHandle (metadata.GetRowCount TableIndex.MethodDef + 1)

            for name, signature in row.Methods do
                let il = InstructionEncoder (BlobBuilder ())
                il.OpCode ILOpCode.Ret

                metadata.AddMethodDefinition (
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    MethodImplAttributes.IL,
                    metadata.GetOrAddString name,
                    signature,
                    bodies.AddMethodBody il,
                    MetadataTokens.ParameterHandle 1
                )
                |> ignore<MethodDefinitionHandle>

            let handle =
                metadata.AddTypeDefinition (
                    row.Attributes,
                    (if row.EnclosingRow.IsSome then
                         Unchecked.defaultof<StringHandle>
                     else
                         metadata.GetOrAddString "W"),
                    metadata.GetOrAddString row.Name,
                    row.BaseType,
                    firstField,
                    firstMethod
                )

            let emittedRow =
                MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit handle : EntityHandle)

            if emittedRow <> index + 2 then
                failwith $"%s{row.Name} was emitted as row %d{emittedRow}, not %d{index + 2}"

        // The sorted tables, each in its owner's row order.
        for index, row in List.indexed rows do
            let owner = typeDef (index + 2)

            for ordinal, name in List.indexed row.GenericParameters do
                metadata.AddGenericParameter (
                    owner,
                    GenericParameterAttributes.None,
                    metadata.GetOrAddString name,
                    ordinal
                )
                |> ignore<GenericParameterHandle>

        for index, row in List.indexed rows do
            for iface in row.Interfaces do
                metadata.AddInterfaceImplementation (MetadataTokens.TypeDefinitionHandle (index + 2), iface)
                |> ignore<InterfaceImplementationHandle>

        for index, row in List.indexed rows do
            match row.EnclosingRow with
            | None -> ()
            | Some enclosing ->
                metadata.AddNestedType (
                    MetadataTokens.TypeDefinitionHandle (index + 2),
                    MetadataTokens.TypeDefinitionHandle enclosing
                )

        let parentOf (shape : Shape) : EntityHandle =
            match shape with
            | Shape.OrderArgumentThenDefinitionBase -> instantiateWith gBadHandle gone2Ref false
            | Shape.TypeSpecParentArgumentFirst -> instantiateWith g3Handle s2Handle true
            | Shape.ArrayOfMissing -> typeSpec (fun t -> t.SZArray().Type (goneRef, false))
            | Shape.ArrayElementLayout -> typeSpec (fun t -> t.SZArray().Type (sHandle, true))
            | Shape.ArrayElementBase -> typeSpec (fun t -> t.SZArray().Type (midHandle, false))
            | _ -> typeDef (caseRow shape)

        let references =
            shapes
            |> List.map (fun shape ->
                shape, metadata.AddMemberReference (parentOf shape, metadata.GetOrAddString "s", int32Sig)
            )
            |> Map.ofList

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray (), references

    /// What binding a reference to the case's `s` does.
    [<RequireQualifiedAccess>]
    type private Answer =
        | Binds
        /// The parent loads, and has no such member: an array, whose members the runtime supplies.
        | NoSuchMember
        /// `TypeLoadException`, with this `TypeName`.
        | TypeLoad of typeName : string
        | Other of string

    let private askReflection (m : Module) (handle : MemberReferenceHandle) : Answer =
        try
            m.ResolveField (MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle))
            |> ignore<FieldInfo>

            Answer.Binds
        with
        | :? TypeLoadException as e -> Answer.TypeLoad e.TypeName
        // `RuntimeModule.ResolveField` turns the binder's `MissingFieldException` into this.
        | :? ArgumentOutOfRangeException -> Answer.NoSuchMember
        | e -> Answer.Other $"%s{e.GetType().Name}: %s{e.Message}"

    let private missName (miss : TypeResolutionMiss) : string =
        match miss with
        | TypeResolutionMiss.TopLevelTypeAbsent (_, None, name)
        | TypeResolutionMiss.TopLevelTypeAbsent (_, Some "", name) -> name
        | TypeResolutionMiss.TopLevelTypeAbsent (_, Some ns, name) -> $"%s{ns}.%s{name}"
        | TypeResolutionMiss.NestedTypeAbsent (_, _, name) -> name

    [<Test>]
    let ``a MemberRef parent that cannot be loaded is Unresolved exactly when the real runtime cannot load it``
        ()
        : unit
        =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let image, references = emit ()
        let context = AssemblyLoadContext ("ParentLoad", isCollectible = true)

        try
            let reflected = context.LoadFromStream (new MemoryStream (image))

            let analysed =
                Assembly.read loggerFactory (Some "ParentLoad.dll") (new MemoryStream (image))

            let baseClassTypes = BaseClassTypes.ofCorelib corelib
            let mutable assemblies = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]
            let rows = ResizeArray<string> ()
            let failures = ResizeArray<string> ()

            for shape in shapes do
                let handle = references.[shape]
                let oracle = askReflection reflected.ManifestModule handle

                let assemblies', ours =
                    MemberReferenceParent.resolve
                        loggerFactory
                        runtimeDirs
                        baseClassTypes
                        assemblies
                        (assemblies.ByDefinitionName analysed.DefinitionFullName)
                        handle

                assemblies <- assemblies'

                let ours =
                    match ours with
                    | MemberReferenceParent.Nominal _ -> Answer.Binds
                    | MemberReferenceParent.Array _ -> Answer.NoSuchMember
                    | MemberReferenceParent.Unresolved miss -> Answer.TypeLoad (missName miss)
                    | other -> Answer.Other $"%A{other}"

                rows.Add $"%O{shape}: runtime %A{oracle}, resolver %A{ours}"

                if oracle <> ours then
                    failures.Add $"%O{shape}: runtime %A{oracle}, resolver %A{ours}"

            for row in rows do
                TestContext.Progress.WriteLine row

            if failures.Count > 0 then
                failures |> String.concat Environment.NewLine |> failwith
        finally
            context.Unload ()

    /// A type a generated world names somewhere.
    [<RequireQualifiedAccess>]
    type private WorldRef =
        | Int32
        /// `System.GoneN`, which CoreLib does not declare, spelled as a value type or a class.
        | Missing of index : int * isValueType : bool
        /// One of the world's own types, spelled as what it is.
        | Own of index : int
        /// `G<argument>` (a class) or `V<argument>` (a value type), declared by the world.
        | Generic of isValueType : bool * argument : WorldRef
        /// `I<argument>`, an interface declared by the world.
        | GenericInterface of argument : WorldRef
        /// `!0`, the generic parameter of the world type it appears in.
        | Var
        /// One of the world's own generic types, instantiated at `argument`.
        | OwnInstance of index : int * argument : WorldRef

    [<RequireQualifiedAccess>]
    type private WorldKind =
        | Class
        | Struct
        | Interface
        /// An enum over `int32`.
        | Enum

    type private WorldField =
        {
            Static : bool
            /// A static literal, with a constant and no `FieldDesc`.
            Literal : bool
            /// A static field carrying `[ThreadStatic]`.
            ThreadStatic : bool
            Type : WorldRef
        }

    type private WorldType =
        {
            Kind : WorldKind
            /// Whether it has one generic parameter, which its members may mention as `Var`.
            IsGeneric : bool
            /// A class's base; `None` for `System.Object`, and for every other kind.
            Base : WorldRef option
            Interfaces : WorldRef list
            Fields : WorldField list
        }

    /// A world: its types, the last of which is the parent whose static `s : int32` is referenced,
    /// and, when the parent is generic, the argument the reference instantiates it at.
    type private World =
        {
            Types : WorldType list
            ParentArgument : WorldRef option
        }

    let private generateWorld (random : Random) : World =
        let count = random.Next (2, 7)
        let missingCount = random.Next (1, 4)

        let kindAt =
            Array.init
                count
                (fun index ->
                    if index = count - 1 then
                        WorldKind.Class
                    else
                        match random.Next 4 with
                        | 0 -> WorldKind.Class
                        | 1 -> WorldKind.Struct
                        | 2 -> WorldKind.Enum
                        | _ -> WorldKind.Interface
                )

        let genericAt =
            Array.init
                count
                (fun index ->
                    match kindAt.[index] with
                    | WorldKind.Enum -> false
                    | WorldKind.Class
                    | WorldKind.Struct
                    | WorldKind.Interface -> random.Next 3 = 0
                )

        let pick (options : 'a list) : 'a = options.[random.Next options.Length]

        let missing (isValueType : bool) =
            WorldRef.Missing (random.Next missingCount, isValueType)

        // An argument, for a member of type `index` (whose `!0` it may name if that is generic and
        // `allowVar`): anything that can stand in a generic instantiation, from earlier types.
        let rec argument (index : int) (allowVar : bool) (depth : int) : WorldRef =
            pick (
                [ WorldRef.Int32 ; missing true ; missing false ]
                @ (if allowVar && index < count && genericAt.[index] then
                       [ WorldRef.Var ]
                   else
                       [])
                @ (earlier index allowVar [ WorldKind.Class ; WorldKind.Struct ; WorldKind.Enum ] depth)
                @ (if depth < 2 then
                       [ WorldRef.Generic (random.Next 2 = 0, argument index allowVar (depth + 1)) ]
                   else
                       [])
            )

        // The earlier types of these kinds, a generic one instantiated.
        and earlier (index : int) (allowVar : bool) (kinds : WorldKind list) (depth : int) : WorldRef list =
            [ 0 .. (min index count) - 1 ]
            |> List.filter (fun other -> List.contains kindAt.[other] kinds)
            |> List.choose (fun other ->
                if not genericAt.[other] then
                    Some (WorldRef.Own other)
                elif depth < 2 then
                    Some (WorldRef.OwnInstance (other, argument index allowVar (depth + 1)))
                else
                    None
            )

        let classSpelling (index : int) : WorldRef =
            pick (
                [ missing false ; WorldRef.Generic (false, argument index true 0) ]
                @ earlier index true [ WorldKind.Class ] 0
            )

        let valueSpelling (index : int) (allowSelf : bool) : WorldRef =
            let self =
                if allowSelf && kindAt.[index] = WorldKind.Struct && not genericAt.[index] then
                    [ WorldRef.Own index ]
                else
                    []

            pick (
                [ missing true ; WorldRef.Generic (true, argument index true 0) ]
                @ earlier index true [ WorldKind.Struct ; WorldKind.Enum ] 0
                @ self
            )

        // Never naming the type's own parameter, so that no two of its interfaces can coincide once
        // it is instantiated, which is a load failure of a different kind.
        let interfaceSpelling (index : int) : WorldRef =
            pick (
                [ missing false ; WorldRef.GenericInterface (argument index false 0) ]
                @ earlier index false [ WorldKind.Interface ] 0
            )

        [
            for index in 0 .. count - 1 do
                let kind = kindAt.[index]

                let fields =
                    match kind with
                    | WorldKind.Interface
                    | WorldKind.Enum -> []
                    | WorldKind.Class
                    | WorldKind.Struct ->
                        List.init
                            (random.Next 4)
                            (fun _ ->
                                let isStatic = random.Next 2 = 0
                                let isLiteral = isStatic && random.Next 5 = 0
                                let isThreadStatic = isStatic && not isLiteral && random.Next 4 = 0

                                let ty =
                                    match random.Next 3 with
                                    | 0 -> WorldRef.Int32
                                    | 1 -> classSpelling index
                                    | _ -> valueSpelling index isStatic

                                {
                                    Static = isStatic
                                    Literal = isLiteral
                                    ThreadStatic = isThreadStatic
                                    Type = ty
                                }
                            )

                {
                    Kind = kind
                    IsGeneric = genericAt.[index]
                    Base =
                        match kind with
                        | WorldKind.Class when random.Next 2 = 0 -> Some (classSpelling index)
                        | _ -> None
                    Interfaces = List.init (random.Next 3) (fun _ -> interfaceSpelling index) |> List.distinct
                    Fields = fields
                }
        ]
        |> fun types ->
            {
                Types = types
                ParentArgument =
                    if genericAt.[count - 1] then
                        Some (argument count false 0)
                    else
                        None
            }

    /// The image for a world, and the MemberRef naming the parent's `s`.
    let private emitWorld (world : World) : byte[] * MemberReferenceHandle =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "World.dll",
            metadata.GetOrAddGuid (Guid "c41a7e90-2b5d-4f38-9e61-0d7b3a8c5f24"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "World",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let corelibType (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = corelibType "Object"
        let valueTypeRef = corelibType "ValueType"
        let missingRefs = Array.init 3 (fun index -> corelibType $"Gone%d{index}")
        let enumRef = corelibType "Enum"

        let threadStaticCtor : EntityHandle =
            metadata.AddMemberReference (
                corelibType "ThreadStaticAttribute",
                metadata.GetOrAddString ".ctor",
                // HASTHIS, no parameters, returning void.
                metadata.GetOrAddBlob [| 0x20uy ; 0x00uy ; 0x01uy |]
            )
            |> MemberReferenceHandle.op_Implicit

        // `<Module>` is row 1, `G`1` row 2, `V`1` row 3, `I`1` row 4, then the world's types.
        let typeDef (row : int) : EntityHandle =
            MetadataTokens.TypeDefinitionHandle row |> TypeDefinitionHandle.op_Implicit

        let gHandle = typeDef 2
        let vHandle = typeDef 3
        let iHandle = typeDef 4
        let ownHandle (index : int) = typeDef (5 + index)
        let worldArray = Array.ofList world.Types

        let rec encode (t : SignatureTypeEncoder) (ref : WorldRef) : unit =
            match ref with
            | WorldRef.Int32 -> t.Int32 ()
            | WorldRef.Missing (index, isValueType) -> t.Type (missingRefs.[index], isValueType)
            | WorldRef.Own index ->
                let isValueType =
                    match worldArray.[index].Kind with
                    | WorldKind.Struct
                    | WorldKind.Enum -> true
                    | WorldKind.Class
                    | WorldKind.Interface -> false

                t.Type (ownHandle index, isValueType)
            | WorldRef.Var -> t.GenericTypeParameter 0
            | WorldRef.OwnInstance (index, argument) ->
                let isValueType =
                    match worldArray.[index].Kind with
                    | WorldKind.Struct
                    | WorldKind.Enum -> true
                    | WorldKind.Class
                    | WorldKind.Interface -> false

                encode (t.GenericInstantiation(ownHandle index, 1, isValueType).AddArgument ()) argument
            | WorldRef.Generic (isValueType, argument) ->
                let root = if isValueType then vHandle else gHandle
                encode (t.GenericInstantiation(root, 1, isValueType).AddArgument ()) argument
            | WorldRef.GenericInterface argument ->
                encode (t.GenericInstantiation(iHandle, 1, false).AddArgument ()) argument

        let fieldSig (ref : WorldRef) : BlobHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).Field().Type ()) ref
            metadata.GetOrAddBlob blob

        // A base or interface: a TypeDef or TypeRef where it is one, a TypeSpec otherwise.
        let typeToken (ref : WorldRef) : EntityHandle =
            match ref with
            | WorldRef.Missing (index, _) -> missingRefs.[index]
            | WorldRef.Own index -> ownHandle index
            | WorldRef.Int32
            | WorldRef.Var
            | WorldRef.OwnInstance _
            | WorldRef.Generic _
            | WorldRef.GenericInterface _ ->
                let blob = BlobBuilder ()
                encode (BlobEncoder(blob).TypeSpecificationSignature ()) ref

                TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        let int32Sig = fieldSig WorldRef.Int32
        let publicStatic = FieldAttributes.Public ||| FieldAttributes.Static

        let structAttributes =
            TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.SequentialLayout

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let nextField () =
            MetadataTokens.FieldDefinitionHandle (metadata.GetRowCount TableIndex.Field + 1)

        let firstMethod = MetadataTokens.MethodDefinitionHandle 1

        let gFields = nextField ()

        metadata.AddTypeDefinition (
            TypeAttributes.Public,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "G`1",
            objectRef,
            gFields,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        let vFields = nextField ()

        metadata.AddFieldDefinition (FieldAttributes.Public, metadata.GetOrAddString "x", int32Sig)
        |> ignore<FieldDefinitionHandle>

        metadata.AddTypeDefinition (
            structAttributes,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "V`1",
            valueTypeRef,
            vFields,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "I`1",
            Unchecked.defaultof<EntityHandle>,
            nextField (),
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        for index, ty in List.indexed world.Types do
            let fields = nextField ()
            let isParent = index = world.Types.Length - 1

            if isParent then
                metadata.AddFieldDefinition (publicStatic, metadata.GetOrAddString "s", int32Sig)
                |> ignore<FieldDefinitionHandle>

            if ty.Kind = WorldKind.Enum then
                metadata.AddFieldDefinition (
                    FieldAttributes.Public
                    ||| FieldAttributes.SpecialName
                    ||| FieldAttributes.RTSpecialName,
                    metadata.GetOrAddString "value__",
                    int32Sig
                )
                |> ignore<FieldDefinitionHandle>

            for ordinal, field in List.indexed ty.Fields do
                let attributes =
                    if field.Literal then
                        publicStatic ||| FieldAttributes.Literal ||| FieldAttributes.HasDefault
                    elif field.Static then
                        publicStatic
                    else
                        FieldAttributes.Public

                let handle =
                    metadata.AddFieldDefinition (
                        attributes,
                        metadata.GetOrAddString $"f%d{ordinal}",
                        fieldSig field.Type
                    )

                if field.Literal then
                    metadata.AddConstant ((FieldDefinitionHandle.op_Implicit handle : EntityHandle), box 0)
                    |> ignore<ConstantHandle>

                if field.ThreadStatic then
                    metadata.AddCustomAttribute (
                        (FieldDefinitionHandle.op_Implicit handle : EntityHandle),
                        threadStaticCtor,
                        metadata.GetOrAddBlob [| 0x01uy ; 0x00uy ; 0x00uy ; 0x00uy |]
                    )
                    |> ignore<CustomAttributeHandle>

            let attributes, baseType =
                match ty.Kind with
                | WorldKind.Class ->
                    TypeAttributes.Public, (ty.Base |> Option.map typeToken |> Option.defaultValue objectRef)
                | WorldKind.Struct -> structAttributes, valueTypeRef
                | WorldKind.Enum -> TypeAttributes.Public ||| TypeAttributes.Sealed, enumRef
                | WorldKind.Interface ->
                    TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract,
                    Unchecked.defaultof<EntityHandle>

            metadata.AddTypeDefinition (
                attributes,
                metadata.GetOrAddString "W",
                metadata.GetOrAddString (if ty.IsGeneric then $"T%d{index}`1" else $"T%d{index}"),
                baseType,
                fields,
                firstMethod
            )
            |> ignore<TypeDefinitionHandle>

        for genericRow in [ 2 ; 3 ; 4 ] do
            metadata.AddGenericParameter (
                typeDef genericRow,
                GenericParameterAttributes.None,
                metadata.GetOrAddString "T",
                0
            )
            |> ignore<GenericParameterHandle>

        for index, ty in List.indexed world.Types do
            if ty.IsGeneric then
                metadata.AddGenericParameter (
                    ownHandle index,
                    GenericParameterAttributes.None,
                    metadata.GetOrAddString "T",
                    0
                )
                |> ignore<GenericParameterHandle>

        for index, ty in List.indexed world.Types do
            for iface in ty.Interfaces do
                metadata.AddInterfaceImplementation (MetadataTokens.TypeDefinitionHandle (5 + index), typeToken iface)
                |> ignore<InterfaceImplementationHandle>

        let reference =
            let parentIndex = world.Types.Length - 1

            let parent =
                match world.ParentArgument with
                | None -> ownHandle parentIndex
                | Some argument -> typeToken (WorldRef.OwnInstance (parentIndex, argument))

            metadata.AddMemberReference (parent, metadata.GetOrAddString "s", int32Sig)

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                BlobBuilder ()
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray (), reference

    [<Test>]
    let ``in generated worlds, a MemberRef parent is Unresolved exactly when the real runtime cannot load it``
        ()
        : unit
        =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let random = Random 20261001
        let failures = ResizeArray<string> ()
        let mutable typeLoads = 0
        let mutable binds = 0

        for _ in 1..1000 do
            let world = generateWorld random
            let image, reference = emitWorld world
            let context = AssemblyLoadContext ("World", isCollectible = true)

            try
                let reflected = context.LoadFromStream (new MemoryStream (image))

                let analysed =
                    Assembly.read loggerFactory (Some "World.dll") (new MemoryStream (image))

                let assemblies = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]
                let oracle = askReflection reflected.ManifestModule reference

                let _, ours =
                    MemberReferenceParent.resolve
                        loggerFactory
                        runtimeDirs
                        baseClassTypes
                        assemblies
                        (assemblies.ByDefinitionName analysed.DefinitionFullName)
                        reference

                let ours =
                    match ours with
                    | MemberReferenceParent.Nominal _ -> Answer.Binds
                    | MemberReferenceParent.Unresolved miss -> Answer.TypeLoad (missName miss)
                    | other -> Answer.Other $"%A{other}"

                match oracle with
                | Answer.TypeLoad typeName when not (typeName.StartsWith ("System.Gone", StringComparison.Ordinal)) ->
                    failwith
                        $"the generator made a world that fails to load for a reason other than a missing type (%s{typeName}): %A{world}"
                | Answer.TypeLoad _ -> typeLoads <- typeLoads + 1
                | Answer.Binds -> binds <- binds + 1
                | _ -> ()

                if oracle <> ours then
                    failures.Add $"runtime %A{oracle}, resolver %A{ours}, in %A{world}"
            finally
                context.Unload ()

        if failures.Count > 0 then
            failures |> Seq.truncate 5 |> String.concat Environment.NewLine |> failwith

        // Both answers must have been compared often enough to mean something.
        typeLoads |> shouldBeGreaterThan 300
        binds |> shouldBeGreaterThan 100

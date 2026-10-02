namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// A field instruction whose operand is a MemberRef, against the real runtime.
///
/// CoreCLR binds a field MemberRef with `MemberLoader::FindField`: the parent's own fields, never
/// an inherited or a literal one, matched by name and by `MetaSig::CompareFieldSigs` (the header
/// byte, then the type exactly, custom modifiers and symbolic type variables included), the first
/// in `FieldDesc` order winning. A reference that binds nothing throws `MissingFieldException`; one
/// whose parent cannot be loaded throws `TypeLoadException`. Either is raised when the method using
/// the reference is compiled, so each method here is the one access and nothing else, and the
/// driver catches around the call.
///
/// PawPrint does not yet raise either exception: where the real runtime throws, the test requires
/// that PawPrint refuses to run on, for the reason the real runtime throws, rather than binding a
/// field the real runtime would not.
///
/// C# emits a field MemberRef only across assemblies or through a generic instantiation, and only
/// for a field that existed when it compiled, so every reference here is hand-written metadata.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedFieldReference =

    /// Namespace `W`:
    /// - `Base`1<T>`, a struct declaring `f : !0`, `static s : int32` and `const k : int32 = 7`.
    /// - `Dups`, a struct declaring, in this order, `[ThreadStatic] static a`, `static a`, `a`,
    ///   `[ThreadStatic] static b` and `static b`, all `int32`.
    /// - `Odd`, a class declaring `g : int32 modopt(System.Gone)`, whose modifier names a type CoreLib
    ///   does not declare, and `h : int32` under the field signature header 0x26 (HASTHIS set).
    /// - `Holder`, a class declaring `static s : int32`, and `Sub : Holder`, declaring nothing.
    /// - `Broken : System.Gone`, a class declaring `static s : int32`, whose base type CoreLib does
    ///   not declare.
    /// - `UriHolder : System.Uri`, a class declaring `static s : int32`, whose base type is in an
    ///   assembly nothing else in the run loads.
    /// - `GoneHolder`, a class declaring `static v : valuetype System.Gone`, a value type CoreLib does
    ///   not declare, and `static safe : int32`.
    /// - `GoneRefHolder`, a class declaring `static w : class System.Gone`.
    /// - `GC`1`, a generic class declaring `static s : int32` and three methods: `Nine()`, and
    ///   `CallOpen()` and `ReadOpen()`, which call `Nine` and read `s` through MemberRefs whose
    ///   parent is `GC`1`'s own TypeDef, so the definition, not the instantiation running them.
    /// - `TypeSub`1`, an abstract generic class deriving from `System.Type`.
    /// - `N`, a static class of one `static int32 ()` method per case, each named after its case.
    let private fabricate () : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "FieldRefGuest.dll",
            metadata.GetOrAddGuid (Guid "3b8e0f6a-91c2-4d57-a0e4-6c2f9d1b7e53"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "FieldRefGuest",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        // Reference the host's own CoreLib, so the image loads on the real runtime as well.
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

        let corelibType (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = corelibType "System" "Object"
        let valueTypeRef = corelibType "System" "ValueType"
        let stringRef = corelibType "System" "String"
        let typeRef = corelibType "System" "Type"
        let runtimeTypeHandleRef = corelibType "System" "RuntimeTypeHandle"
        let isConstRef = corelibType "System.Runtime.CompilerServices" "IsConst"
        let goneRef = corelibType "System" "Gone"

        // A type nested in System.Object, which declares no nested types.
        let goneNestedRef : EntityHandle =
            metadata.AddTypeReference (
                objectRef,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "GoneNested"
            )
            |> TypeReferenceHandle.op_Implicit

        let uriRef : EntityHandle =
            let uriName = typeof<Uri>.Assembly.GetName ()

            let uriAssembly =
                metadata.AddAssemblyReference (
                    metadata.GetOrAddString uriName.Name,
                    uriName.Version,
                    Unchecked.defaultof<StringHandle>,
                    metadata.GetOrAddBlob (uriName.GetPublicKeyToken ()),
                    Unchecked.defaultof<AssemblyFlags>,
                    Unchecked.defaultof<BlobHandle>
                )

            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit uriAssembly : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Uri"
            )
            |> TypeReferenceHandle.op_Implicit

        let nowhereRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString "Nowhere",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let unboundRef : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit nowhereRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Gone"
            )
            |> TypeReferenceHandle.op_Implicit

        let fieldSignature (encode : SignatureTypeEncoder -> unit) : BlobHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).Field().Type ())
            metadata.GetOrAddBlob blob

        let int32Sig = fieldSignature (fun t -> t.Int32 ())
        let var0Sig = fieldSignature (fun t -> t.GenericTypeParameter 0)

        let modifiedVar0Sig =
            fieldSignature (fun t ->
                t.CustomModifiers().AddModifier (isConstRef, true)
                |> ignore<CustomModifiersEncoder>

                t.GenericTypeParameter 0
            )

        let modifiedByAbsentSig =
            fieldSignature (fun t ->
                t.CustomModifiers().AddModifier (goneRef, true)
                |> ignore<CustomModifiersEncoder>

                t.Int32 ()
            )

        let modifiedByUnboundSig =
            fieldSignature (fun t ->
                t.CustomModifiers().AddModifier (unboundRef, true)
                |> ignore<CustomModifiersEncoder>

                t.Int32 ()
            )

        let stringSig = fieldSignature (fun t -> t.String ())
        let stringAsClassSig = fieldSignature (fun t -> t.Type (stringRef, false))

        // FIELD with HASTHIS set, then ELEMENT_TYPE_I4.
        let int32WithHasThisSig = metadata.GetOrAddBlob [| 0x26uy ; 0x08uy |]

        // `<Module>` is row 1, then `Base`1`, `Dups`, `Odd`, `Holder`, `Sub`, `Broken`, `UriHolder`,
        // `GoneHolder`, `GoneRefHolder`, `GC`1`, `TypeSub`1` and `N`.
        let typeDef (row : int) : EntityHandle =
            MetadataTokens.TypeDefinitionHandle row |> TypeDefinitionHandle.op_Implicit

        let baseHandle = typeDef 2
        let dupsHandle = typeDef 3
        let oddHandle = typeDef 4
        let holderHandle = typeDef 5
        let subHandle = typeDef 6
        let brokenHandle = typeDef 7
        let uriHolderHandle = typeDef 8
        let goneHolderHandle = typeDef 9
        let goneRefHolderHandle = typeDef 10
        let gcHandle = typeDef 11
        let typeSubHandle = typeDef 12
        let nHandle = typeDef 13

        let typeSpec (encode : SignatureTypeEncoder -> unit) : EntityHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).TypeSpecificationSignature ())
            TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        let baseOfInt =
            typeSpec (fun encoder ->
                let args = encoder.GenericInstantiation (baseHandle, 1, true)
                args.AddArgument().Int32 ()
            )

        let vector = typeSpec (fun encoder -> encoder.SZArray().Int32 ())

        let baseOfGone =
            typeSpec (fun encoder ->
                let args = encoder.GenericInstantiation (baseHandle, 1, true)
                args.AddArgument().Type (goneRef, false)
            )

        let vectorOfGone = typeSpec (fun encoder -> encoder.SZArray().Type (goneRef, false))

        let addField (attributes : FieldAttributes) (name : string) (signature : BlobHandle) =
            metadata.AddFieldDefinition (attributes, metadata.GetOrAddString name, signature)

        let publicStatic = FieldAttributes.Public ||| FieldAttributes.Static

        // `Base`1`
        let baseFields = addField FieldAttributes.Public "f" var0Sig
        addField publicStatic "s" int32Sig |> ignore<FieldDefinitionHandle>

        let k =
            addField (publicStatic ||| FieldAttributes.Literal ||| FieldAttributes.HasDefault) "k" int32Sig

        metadata.AddConstant ((FieldDefinitionHandle.op_Implicit k : EntityHandle), box 7)
        |> ignore<ConstantHandle>

        // `Dups`
        let threadStaticCtor : EntityHandle =
            metadata.AddMemberReference (
                corelibType "System" "ThreadStaticAttribute",
                metadata.GetOrAddString ".ctor",
                // HASTHIS, no parameters, returning void.
                metadata.GetOrAddBlob [| 0x20uy ; 0x00uy ; 0x01uy |]
            )
            |> MemberReferenceHandle.op_Implicit

        let addThreadStatic (name : string) =
            let field = addField publicStatic name int32Sig

            metadata.AddCustomAttribute (
                (FieldDefinitionHandle.op_Implicit field : EntityHandle),
                threadStaticCtor,
                metadata.GetOrAddBlob [| 0x01uy ; 0x00uy ; 0x00uy ; 0x00uy |]
            )
            |> ignore<CustomAttributeHandle>

            field

        let threadStaticA = addThreadStatic "a"
        let staticA = addField publicStatic "a" int32Sig
        let instanceA = addField FieldAttributes.Public "a" int32Sig
        let threadStaticB = addThreadStatic "b"
        let staticB = addField publicStatic "b" int32Sig

        // `Odd`
        let oddFields = addField FieldAttributes.Public "g" modifiedByAbsentSig

        addField FieldAttributes.Public "h" int32WithHasThisSig
        |> ignore<FieldDefinitionHandle>

        // `Holder`
        let holderFields = addField publicStatic "s" int32Sig

        // `Broken`
        let brokenFields = addField publicStatic "s" int32Sig

        // `UriHolder`
        let uriHolderFields = addField publicStatic "s" int32Sig

        // `GoneHolder`
        let goneValueSig = fieldSignature (fun t -> t.Type (goneRef, true))
        let goneHolderFields = addField publicStatic "v" goneValueSig
        addField publicStatic "safe" int32Sig |> ignore<FieldDefinitionHandle>
        let goneClassSig = fieldSignature (fun t -> t.Type (goneRef, false))

        // `GoneRefHolder`
        let goneRefHolderFields = addField publicStatic "w" goneClassSig

        // `GC`1`
        let gcFields = addField publicStatic "s" int32Sig

        let noMoreFields =
            MetadataTokens.FieldDefinitionHandle (metadata.GetRowCount TableIndex.Field + 1)

        let reference (parent : EntityHandle) (name : string) (signature : BlobHandle) : EntityHandle =
            metadata.AddMemberReference (parent, metadata.GetOrAddString name, signature)
            |> MemberReferenceHandle.op_Implicit

        let fieldDef (handle : FieldDefinitionHandle) : EntityHandle =
            FieldDefinitionHandle.op_Implicit handle

        let localsOf (encode : SignatureTypeEncoder -> unit) : StandaloneSignatureHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).LocalVariableSignature(1).AddVariable().Type ())
            metadata.AddStandaloneSignature (metadata.GetOrAddBlob blob)

        let baseOfIntLocal =
            localsOf (fun t -> t.GenericInstantiation(baseHandle, 1, true).AddArgument().Int32 ())

        let dupsLocal = localsOf (fun t -> t.Type (dupsHandle, true))

        let returnsInt32 =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (0, (fun (ret : ReturnTypeEncoder) -> ret.Type().Int32 ()), ignore)

            metadata.GetOrAddBlob blob

        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream
        let mutable firstMethod = None

        let define (name : string) (locals : StandaloneSignatureHandle option) (body : InstructionEncoder -> unit) =
            let il = InstructionEncoder (BlobBuilder ())
            body il
            il.OpCode ILOpCode.Ret

            let bodyOffset =
                match locals with
                | None -> bodies.AddMethodBody il
                | Some locals -> bodies.AddMethodBody (il, 8, locals, MethodBodyAttributes.InitLocals)

            let handle =
                metadata.AddMethodDefinition (
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    MethodImplAttributes.IL,
                    metadata.GetOrAddString name,
                    returnsInt32,
                    bodyOffset,
                    MetadataTokens.ParameterHandle 1
                )

            if firstMethod.IsNone then
                firstMethod <- Some handle

        let op (il : InstructionEncoder) (opcode : ILOpCode) (token : EntityHandle) : unit =
            il.OpCode opcode
            il.Token token

        // Read, through `ldsfld`, a static field the reference names.
        let readStatic (name : string) (token : EntityHandle) =
            define name None (fun il -> op il ILOpCode.Ldsfld token)

        // Read, through `ldfld` off a null reference, an instance field the reference names. The
        // reference is bound when the method is compiled, so a reference that binds nothing throws
        // before the null is ever dereferenced.
        let readOffNull (name : string) (token : EntityHandle) =
            define
                name
                None
                (fun il ->
                    il.OpCode ILOpCode.Ldnull
                    op il ILOpCode.Ldfld token
                )

        // `GC`1`'s methods, which come first so that its method list precedes `N`'s.
        define "Nine" None (fun il -> il.LoadConstantI4 9)

        define
            "CallOpen"
            None
            (fun il ->
                il.OpCode ILOpCode.Call
                il.Token (reference gcHandle "Nine" returnsInt32)
            )

        define "ReadOpen" None (fun il -> op il ILOpCode.Ldsfld (reference gcHandle "s" int32Sig))

        // `Object.ReferenceEquals`, which `GC`1` inherits, through `GC`1`'s own TypeDef.
        define
            "CallInheritedOpen"
            None
            (fun il ->
                let objectsToBool =
                    let blob = BlobBuilder ()

                    BlobEncoder(blob)
                        .MethodSignature()
                        .Parameters (
                            2,
                            (fun (ret : ReturnTypeEncoder) -> ret.Type().Boolean ()),
                            fun (parameters : ParametersEncoder) ->
                                parameters.AddParameter().Type().Object ()
                                parameters.AddParameter().Type().Object ()
                        )

                    metadata.GetOrAddBlob blob

                il.OpCode ILOpCode.Ldnull
                il.OpCode ILOpCode.Ldnull
                il.OpCode ILOpCode.Call
                il.Token (reference gcHandle "ReferenceEquals" objectsToBool)
            )

        let nMethods =
            MetadataTokens.MethodDefinitionHandle (metadata.GetRowCount TableIndex.MethodDef + 1)

        let gcOfInt =
            typeSpec (fun encoder ->
                let args = encoder.GenericInstantiation (gcHandle, 1, false)
                args.AddArgument().Int32 ()
            )

        // A generic definition's member, named through its TypeDef by code running in an
        // instantiation of it: the definition is not that instantiation.
        define
            "CallOpenFromClosedCaller"
            None
            (fun il ->
                il.OpCode ILOpCode.Call
                il.Token (reference gcOfInt "CallOpen" returnsInt32)
            )

        define
            "CallInheritedOpenFromClosedCaller"
            None
            (fun il ->
                il.OpCode ILOpCode.Call
                il.Token (reference gcOfInt "CallInheritedOpen" returnsInt32)
            )

        define
            "ReadOpenFromClosedCaller"
            None
            (fun il ->
                il.OpCode ILOpCode.Call
                il.Token (reference gcOfInt "ReadOpen" returnsInt32)
            )

        // A function pointer to `Object.ToString`, which `GC`1` inherits, through `GC`1`'s TypeDef.
        define
            "LdftnInheritedOpen"
            None
            (fun il ->
                let returnsString =
                    let blob = BlobBuilder ()

                    BlobEncoder(blob)
                        .MethodSignature(isInstanceMethod = true)
                        .Parameters (0, (fun (ret : ReturnTypeEncoder) -> ret.Type().String ()), ignore)

                    metadata.GetOrAddBlob blob

                op il ILOpCode.Ldftn (reference gcHandle "ToString" returnsString)
                il.OpCode ILOpCode.Pop
                il.LoadConstantI4 0
            )

        // `typeof(int).IsValueType`, with `GetTypeFromHandle` named through the TypeDef of
        // `TypeSub`1`, which inherits it, so through a generic definition with no instantiation.
        define
            "TypeofThroughOpenDefinition"
            None
            (fun il ->
                let getTypeFromHandle =
                    let blob = BlobBuilder ()

                    BlobEncoder(blob)
                        .MethodSignature()
                        .Parameters (
                            1,
                            (fun (ret : ReturnTypeEncoder) -> ret.Type().Type (typeRef, false)),
                            fun (parameters : ParametersEncoder) ->
                                parameters.AddParameter().Type().Type (runtimeTypeHandleRef, true)
                        )

                    metadata.GetOrAddBlob blob

                let getIsValueType =
                    let blob = BlobBuilder ()

                    BlobEncoder(blob)
                        .MethodSignature(isInstanceMethod = true)
                        .Parameters (0, (fun (ret : ReturnTypeEncoder) -> ret.Type().Boolean ()), ignore)

                    metadata.GetOrAddBlob blob

                il.OpCode ILOpCode.Ldtoken
                il.Token typeRef
                il.OpCode ILOpCode.Pop
                il.OpCode ILOpCode.Ldtoken
                il.Token (corelibType "System" "Int32")
                il.OpCode ILOpCode.Call
                il.Token (reference typeSubHandle "GetTypeFromHandle" getTypeFromHandle)
                il.OpCode ILOpCode.Callvirt
                il.Token (reference typeRef "get_IsValueType" getIsValueType)
            )

        // A generic definition's field, named through its TypeDef by code outside it.
        readStatic "FieldOfOpenDefinition" (reference baseHandle "s" int32Sig)

        // Binds.
        define "Nine" None (fun il -> il.LoadConstantI4 9)

        // A method, reached through a MemberRef whose parent is the TypeDef declaring it.
        define
            "CallViaTypeDefinitionParent"
            None
            (fun il ->
                il.OpCode ILOpCode.Call
                il.Token (reference nHandle "Nine" returnsInt32)
            )

        let baseF = reference baseOfInt "f" var0Sig
        let baseS = reference baseOfInt "s" int32Sig

        define
            "BaseF"
            (Some baseOfIntLocal)
            (fun il ->
                il.LoadLocalAddress 0
                il.LoadConstantI4 5
                op il ILOpCode.Stfld baseF
                il.LoadLocalAddress 0
                op il ILOpCode.Ldfld baseF
            )

        define
            "BaseS"
            None
            (fun il ->
                il.LoadConstantI4 6
                op il ILOpCode.Stsfld baseS
                op il ILOpCode.Ldsfld baseS
            )

        // The instance `a` is the first `FieldDesc` of that name, whatever the metadata order.
        define
            "DupsA"
            (Some dupsLocal)
            (fun il ->
                il.LoadLocalAddress 0
                il.LoadConstantI4 1
                op il ILOpCode.Stfld (fieldDef instanceA)
                il.LoadConstantI4 2
                op il ILOpCode.Stsfld (fieldDef staticA)
                il.LoadConstantI4 3
                op il ILOpCode.Stsfld (fieldDef threadStaticA)
                il.LoadLocalAddress 0
                op il ILOpCode.Ldfld (reference dupsHandle "a" int32Sig)
            )

        // The ordinary static `b` precedes the `[ThreadStatic]` one.
        define
            "DupsB"
            None
            (fun il ->
                il.LoadConstantI4 2
                op il ILOpCode.Stsfld (fieldDef staticB)
                il.LoadConstantI4 3
                op il ILOpCode.Stsfld (fieldDef threadStaticB)
                op il ILOpCode.Ldsfld (reference dupsHandle "b" int32Sig)
            )

        define
            "StringEmpty"
            None
            (fun il ->
                op il ILOpCode.Ldsfld (reference stringRef "Empty" stringSig)
                il.OpCode ILOpCode.Ldnull
                il.OpCode ILOpCode.Cgt_un
            )

        // Bind nothing.
        define
            "StringEmptyAsClass"
            None
            (fun il ->
                op il ILOpCode.Ldsfld (reference stringRef "Empty" stringAsClassSig)
                il.OpCode ILOpCode.Ldnull
                il.OpCode ILOpCode.Cgt_un
            )

        define
            "BaseFAsInt32"
            (Some baseOfIntLocal)
            (fun il ->
                il.LoadLocalAddress 0
                op il ILOpCode.Ldfld (reference baseOfInt "f" int32Sig)
            )

        define
            "BaseFModified"
            (Some baseOfIntLocal)
            (fun il ->
                il.LoadLocalAddress 0
                op il ILOpCode.Ldfld (reference baseOfInt "f" modifiedVar0Sig)
            )

        readStatic "BaseKLiteral" (reference baseOfInt "k" int32Sig)
        readOffNull "OddGModifiedByUnbound" (reference oddHandle "g" modifiedByUnboundSig)
        readOffNull "OddHHeader" (reference oddHandle "h" int32Sig)
        readStatic "HolderAbsent" (reference holderHandle "absent" int32Sig)
        readStatic "SubInherited" (reference subHandle "s" int32Sig)
        // The parent exists, but loading it fails, so whether the field exists is never asked.
        readStatic "BrokenBaseFieldPresent" (reference brokenHandle "s" int32Sig)
        readStatic "BrokenBaseFieldAbsent" (reference brokenHandle "absent" int32Sig)
        // Loading the parent loads the assembly its base type is in.
        readStatic "BaseInUnloadedAssembly" (reference uriHolderHandle "s" int32Sig)

        // The reference spells the field's type with the very TypeRef the definition does, so the
        // two signatures agree without either naming a type; the field still cannot be loaded.
        // The field exists and its type loads, but its declaring type does not: a sibling field's
        // value type is absent.
        readStatic "SiblingValueFieldAbsent" (reference goneHolderHandle "safe" int32Sig)

        // A method a generic definition inherits, through a MemberRef whose parent is that
        // definition's TypeDef: its typical instantiation.
        define
            "LdtokenInheritedMethodOfGenericDefinition"
            None
            (fun il ->
                let returnsString =
                    let blob = BlobBuilder ()

                    BlobEncoder(blob)
                        .MethodSignature(isInstanceMethod = true)
                        .Parameters (0, (fun (ret : ReturnTypeEncoder) -> ret.Type().String ()), ignore)

                    metadata.GetOrAddBlob blob

                op il ILOpCode.Ldtoken (reference baseHandle "ToString" returnsString)
                il.OpCode ILOpCode.Pop
                il.LoadConstantI4 0
            )

        define
            "LdtokenReferenceFieldOfAbsentType"
            None
            (fun il ->
                op il ILOpCode.Ldtoken (reference goneRefHolderHandle "w" goneClassSig)
                il.OpCode ILOpCode.Pop
                il.LoadConstantI4 0
            )

        define
            "LdtokenFieldOfAbsentType"
            None
            (fun il ->
                op il ILOpCode.Ldtoken (reference goneHolderHandle "v" goneValueSig)
                il.OpCode ILOpCode.Pop
                il.LoadConstantI4 0
            )

        readStatic "VectorLength" (reference vector "Length" int32Sig)

        define
            "StsfldAbsent"
            None
            (fun il ->
                il.LoadConstantI4 1
                op il ILOpCode.Stsfld (reference holderHandle "absent" int32Sig)
                il.LoadConstantI4 0
            )

        define
            "LdsfldaAbsent"
            None
            (fun il ->
                op il ILOpCode.Ldsflda (reference holderHandle "absent" int32Sig)
                il.OpCode ILOpCode.Ldind_i4
            )

        define
            "LdfldaAbsent"
            None
            (fun il ->
                il.OpCode ILOpCode.Ldnull
                op il ILOpCode.Ldflda (reference oddHandle "absent" int32Sig)
                il.OpCode ILOpCode.Ldind_i4
            )

        define
            "LdtokenAbsent"
            None
            (fun il ->
                op il ILOpCode.Ldtoken (reference holderHandle "absent" int32Sig)
                il.OpCode ILOpCode.Pop
                il.LoadConstantI4 0
            )

        // The parent names no type.
        readStatic "ParentAbsent" (reference goneRef "f" int32Sig)
        readStatic "NestedParentAbsent" (reference goneNestedRef "f" int32Sig)
        // The parent's definition exists, but a type it is built from does not. CoreCLR loads the
        // whole parent before looking for the member, so whether the field exists is never asked.
        readStatic "ParentArgumentAbsentFieldPresent" (reference baseOfGone "s" int32Sig)
        readStatic "ParentArgumentAbsentFieldAbsent" (reference baseOfGone "absent" int32Sig)
        readStatic "ParentElementAbsent" (reference vectorOfGone "Length" int32Sig)

        let firstMethod = firstMethod.Value

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            baseFields,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        let structAttributes =
            TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.SequentialLayout

        let defineType
            (attributes : TypeAttributes)
            (name : string)
            (baseType : EntityHandle)
            (fields : FieldDefinitionHandle)
            : unit
            =
            metadata.AddTypeDefinition (
                attributes,
                metadata.GetOrAddString "W",
                metadata.GetOrAddString name,
                baseType,
                fields,
                firstMethod
            )
            |> ignore<TypeDefinitionHandle>

        defineType structAttributes "Base`1" valueTypeRef baseFields
        defineType structAttributes "Dups" valueTypeRef threadStaticA
        defineType TypeAttributes.Public "Odd" objectRef oddFields
        defineType TypeAttributes.Public "Holder" objectRef holderFields
        defineType TypeAttributes.Public "Sub" holderHandle brokenFields
        defineType TypeAttributes.Public "Broken" goneRef brokenFields
        defineType TypeAttributes.Public "UriHolder" uriRef uriHolderFields
        defineType TypeAttributes.Public "GoneHolder" objectRef goneHolderFields
        defineType TypeAttributes.Public "GoneRefHolder" objectRef goneRefHolderFields
        defineType TypeAttributes.Public "GC`1" objectRef gcFields

        // After `GC`1`, so its method list starts where `N`'s does: it declares none.
        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "TypeSub`1",
            typeRef,
            noMoreFields,
            nMethods
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "N",
            objectRef,
            noMoreFields,
            nMethods
        )
        |> ignore<TypeDefinitionHandle>

        for genericRow in [ 2 ; 11 ; 12 ] do
            metadata.AddGenericParameter (
                (TypeDefinitionHandle.op_Implicit (MetadataTokens.TypeDefinitionHandle genericRow) : EntityHandle),
                GenericParameterAttributes.None,
                metadata.GetOrAddString "T",
                0
            )
            |> ignore<GenericParameterHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                ilStream,
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    /// How the driver's call ended.
    [<RequireQualifiedAccess>]
    type private Expected =
        /// The method returned this.
        | Returns of int
        /// `MissingFieldException`, whose message is this C# expression.
        | MissingField of message : string
        /// `TypeLoadException`, whose message is this C# expression and whose `TypeName` is this.
        | TypeLoad of message : string * typeName : string

    let private fieldNotFound (qualified : string) : Expected =
        Expected.MissingField $"\"Field not found: '%s{qualified}'.\""

    let private goneTypeLoad : Expected =
        Expected.TypeLoad (
            "\"Could not load type 'System.Gone' from assembly '\" + typeof(object).Assembly.FullName + \"'.\"",
            "System.Gone"
        )

    /// A generic definition named without an instantiation, outside `ldtoken`.
    let private openDefinitionTypeLoad (typeName : string) : Expected =
        Expected.TypeLoad (
            $"\"Could not load type '%s{typeName}' from assembly 'FieldRefGuest, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null'.\"",
            typeName
        )

    let private cases : Map<string, Expected> =
        [
            "BaseF", Expected.Returns 5
            "Nine", Expected.Returns 9
            "CallViaTypeDefinitionParent", Expected.Returns 9
            "BaseS", Expected.Returns 6
            "DupsA", Expected.Returns 1
            "DupsB", Expected.Returns 2
            "StringEmpty", Expected.Returns 1
            // `ELEMENT_TYPE_STRING` and a `CLASS` naming System.String are different signatures.
            "StringEmptyAsClass", fieldNotFound "System.String.Empty"
            // Symbolic: `!0` is not the `int32` the instantiation puts there.
            "BaseFAsInt32", fieldNotFound "W.Base`1.f"
            // Custom modifiers are compared.
            "BaseFModified", fieldNotFound "W.Base`1.f"
            // A literal field has no FieldDesc for a reference to find.
            "BaseKLiteral", fieldNotFound "W.Base`1.k"
            // The definition's modifier is resolved first. It names no type, so nothing matches,
            // and the assembly the reference's modifier names is never bound.
            "OddGModifiedByUnbound", fieldNotFound "W.Odd.g"
            // The header byte is compared too.
            "OddHHeader", fieldNotFound "W.Odd.h"
            "HolderAbsent", fieldNotFound "W.Holder.absent"
            // Fields are not inherited.
            "SubInherited", fieldNotFound "W.Sub.s"
            "BrokenBaseFieldPresent", goneTypeLoad
            "BrokenBaseFieldAbsent", goneTypeLoad
            "BaseInUnloadedAssembly", Expected.Returns 0
            "LdtokenFieldOfAbsentType", goneTypeLoad
            // A field of a reference type is bound without loading its type.
            "LdtokenReferenceFieldOfAbsentType", Expected.Returns 0
            "CallOpenFromClosedCaller", openDefinitionTypeLoad "W.GC`1"
            "ReadOpenFromClosedCaller", openDefinitionTypeLoad "W.GC`1"
            "CallInheritedOpenFromClosedCaller", openDefinitionTypeLoad "W.GC`1"
            "LdftnInheritedOpen", openDefinitionTypeLoad "W.GC`1"
            "TypeofThroughOpenDefinition", openDefinitionTypeLoad "W.TypeSub`1"
            "FieldOfOpenDefinition", openDefinitionTypeLoad "W.Base`1"
            "SiblingValueFieldAbsent", goneTypeLoad
            "LdtokenInheritedMethodOfGenericDefinition", Expected.Returns 0
            // An array has no fields.
            "VectorLength", fieldNotFound "System.Int32[].Length"
            "StsfldAbsent", fieldNotFound "W.Holder.absent"
            "LdsfldaAbsent", fieldNotFound "W.Holder.absent"
            "LdfldaAbsent", fieldNotFound "W.Odd.absent"
            "LdtokenAbsent", fieldNotFound "W.Holder.absent"
            "ParentAbsent", goneTypeLoad
            // Named by the missing row alone, not by the type it is nested in.
            "NestedParentAbsent",
            Expected.TypeLoad (
                "\"Could not load type 'GoneNested' from assembly '\" + typeof(object).Assembly.FullName + \"'.\"",
                "GoneNested"
            )
            "ParentArgumentAbsentFieldPresent", goneTypeLoad
            "ParentArgumentAbsentFieldAbsent", goneTypeLoad
            "ParentElementAbsent", goneTypeLoad
        ]
        |> Map.ofList

    let caseNames : string list = cases |> Map.keys |> List.ofSeq

    [<Literal>]
    let private MissingFieldExit = 100

    [<Literal>]
    let private TypeLoadExit = 101

    /// Exits with what the call returned, or with `MissingFieldExit` or `TypeLoadExit` when it threw
    /// that exception with the expected message (and, for `TypeLoadException`, `TypeName`). A different message escapes, naming itself.
    let private driverSource
        (name : string)
        (missingFieldMessage : string)
        (typeLoadMessage : string)
        (typeName : string)
        : string
        =
        $$"""
using System;

public static class Driver
{
    private static void Check(string actual, string expected)
    {
        if (actual != expected)
        {
            throw new Exception("message was <" + actual + ">, expected <" + expected + ">");
        }
    }

    public static int Main(string[] args)
    {
        try
        {
            return W.N.{{name}}();
        }
        catch (MissingFieldException e)
        {
            Check(e.Message, {{missingFieldMessage}});
            return {{MissingFieldExit}};
        }
        catch (TypeLoadException e)
        {
            Check(e.Message, {{typeLoadMessage}});
            Check(e.TypeName, "{{typeName}}");
            return {{TypeLoadExit}};
        }
    }
}
"""

    /// The message of `e` and of every exception inside it.
    let rec private messages (e : exn) : string list =
        match e.InnerException with
        | null -> [ e.Message ]
        | inner -> e.Message :: messages inner

    [<TestCaseSource(nameof caseNames)>]
    let ``a field MemberRef binds as the real runtime binds it, or PawPrint refuses where it throws``
        (name : string)
        : unit
        =
        let unexpected = "\"<no exception expected>\""

        let missingFieldMessage, typeLoadMessage, typeName, exitCode =
            match cases.[name] with
            | Expected.Returns value -> unexpected, unexpected, "", value
            | Expected.MissingField message -> message, unexpected, "", MissingFieldExit
            | Expected.TypeLoad (message, typeName) -> unexpected, message, typeName, TypeLoadExit

        let driver = driverSource name missingFieldMessage typeLoadMessage typeName

        match cases.[name] with
        | Expected.Returns _ ->
            FabricatedGuest.run "FieldRefGuest" (fabricate ()) $"FieldRef%s{name}Driver" driver exitCode
        | Expected.MissingField _
        | Expected.TypeLoad _ ->

        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "FieldRefGuest" (fabricate ()) $"FieldRef%s{name}Driver" driver

        onHost |> shouldEqual (RealRuntimeResult.NormalExit exitCode)

        // The reason PawPrint refuses: the resolver finding no field, or the type it could not load.
        let reason =
            match cases.[name] with
            | Expected.TypeLoad _ -> typeName
            | _ -> "binds to no field"

        match onPawPrint with
        | FabricatedOutcome.Exited code ->
            failwith $"PawPrint ran the guest to exit code %d{code}, where the real runtime throws"
        | FabricatedOutcome.Failed e ->
            let messages = messages e

            if not (messages |> List.exists (fun message -> message.Contains reason)) then
                failwith $"PawPrint refused for a reason not naming '%s{reason}': %A{messages}"

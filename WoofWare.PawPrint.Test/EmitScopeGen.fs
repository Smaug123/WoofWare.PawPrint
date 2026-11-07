namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open WoofWare.PawPrint

/// Generated emit scopes, for property tests of `EmitScope`, its writer and what holds one: each a list
/// of definitions whose references are choices resolved against the rows the scope has so far, so that
/// every list makes a valid scope, and a prefix of the list makes a scope the whole list extends.
module EmitScopeGen =

    /// A body as ECMA-335 II.25.4 lays it out: `nop`s then `ret`.
    type BodyShape =
        | Tiny of nops : int
        /// `locals` picks a `StandAloneSig` to name as the local signature, if the scope has any.
        | Fat of nops : int * maxStack : uint16 * locals : int option
        /// No body at all, for a method that is not abstract: an IL method whose RVA is 0, as a
        /// dynamic module's method is until `SetMethodIL`.
        | NoBody

    /// One definition, its references given as choices that `apply` resolves against the rows the
    /// scope has so far, so that every generated list of operations makes a valid scope.
    type Op =
        | AssemblyRef of name : string * version : uint16 * withToken : bool
        | TypeRef of scope : int * name : string * ns : string
        | TypeDef of name : string * ns : string * flags : int * extends : int option
        | Field of owner : int * name : string * flags : int * fieldType : int
        | Method of
            owner : int *
            name : string *
            flags : int *
            parameters : int list *
            returnType : int *
            body : BodyShape
        | Param of method : int * sequence : int * name : string * flags : int
        | InterfaceImpl of cls : int * iface : int
        | MemberRef of parent : int * name : string * parameters : int list option
        /// A local signature; never empty.
        | StandAloneSig of locals : int list
        | MethodImpl of cls : int * body : int * declaration : int
        /// Overwrite the implementation flags of a method already in the scope, if it has any.
        | SetMethodImplFlags of method : int * flags : int
        /// Overwrite the parent of a type already in the scope other than `<Module>`, if it has any.
        | SetTypeDefExtends of typeDef : int * extends : int
        /// Give a method already in the scope its body, over any it has, if it is not abstract and `body`
        /// is one: `SetMethodIL` sets no body for an abstract method's empty code.
        | SetMethodBody of method : int * body : BodyShape

    type private Applied =
        {
            Scope : EmitScope
            /// The parameter count of each method's signature, by RID - 1.
            ParamCounts : int list
            /// Which `(method, sequence)` pairs already have a `Param` row.
            ParamSequences : Set<int * int>
            /// The `MemberRef`s with a method signature.
            MethodRefs : MemberRefRid list
        }

    // Unpaired surrogates too, built at run time because the F# compiler replaces one in a literal.
    let genName : Gen<string> =
        Gen.elements
            [
                "a"
                "Z"
                "0"
                "_"
                " "
                "<"
                "é"
                "日"
                "\U0001F600"
                string (char 0xD800)
                string (char 0xDFFF)
            ]
        |> Gen.listOf
        |> Gen.map String.Concat

    let genNamespace : Gen<string> = Gen.elements [ "" ; "A" ; "A.B" ; "日本" ]

    let typeFlagChoices : TypeAttributes list =
        [
            TypeAttributes.Public ||| TypeAttributes.Class
            TypeAttributes.NotPublic ||| TypeAttributes.Class
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract
            TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.BeforeFieldInit
        ]

    let fieldFlagChoices : FieldAttributes list =
        [
            FieldAttributes.Public
            FieldAttributes.Private ||| FieldAttributes.Static
            FieldAttributes.Assembly ||| FieldAttributes.InitOnly
        ]

    let methodFlagChoices : MethodAttributes list =
        [
            MethodAttributes.Public ||| MethodAttributes.Static
            MethodAttributes.Public ||| MethodAttributes.HideBySig
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.Final
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.SpecialName
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.Abstract
            ||| MethodAttributes.NewSlot
        ]

    let methodImplFlagChoices : MethodImplAttributes list =
        [
            MethodImplAttributes.IL
            MethodImplAttributes.NoInlining
            MethodImplAttributes.AggressiveInlining ||| MethodImplAttributes.NoOptimization
            MethodImplAttributes.Synchronized
        ]

    let paramFlagChoices : ParameterAttributes list =
        [
            ParameterAttributes.None
            ParameterAttributes.In
            ParameterAttributes.Out
            ParameterAttributes.Optional
        ]

    let genBody : Gen<BodyShape> =
        Gen.oneof
            [
                Gen.choose (0, 60) |> Gen.map BodyShape.Tiny
                Gen.constant BodyShape.NoBody
                gen {
                    let! nops = Gen.choose (0, 100)
                    let! maxStack = Gen.choose (0, 20) |> Gen.map uint16
                    let! locals = Gen.optionOf (Gen.choose (0, 1000))
                    return BodyShape.Fat (nops, maxStack, locals)
                }
            ]

    let choice : Gen<int> = Gen.choose (0, 1000)

    let genOp : Gen<Op> =
        Gen.frequency
            [
                1,
                gen {
                    let! name = genName
                    let! version = Gen.choose (0, 65535) |> Gen.map uint16
                    let! withToken = Gen.elements [ true ; false ]
                    return Op.AssemblyRef ($"Ref%s{name}", version, withToken)
                }
                3,
                gen {
                    let! scope = choice
                    let! name = genName
                    let! ns = genNamespace
                    return Op.TypeRef (scope, name, ns)
                }
                3,
                gen {
                    let! name = genName
                    let! ns = genNamespace
                    let! flags = choice
                    let! extends = Gen.optionOf choice
                    return Op.TypeDef (name, ns, flags, extends)
                }
                3,
                gen {
                    let! owner = choice
                    let! name = genName
                    let! flags = choice
                    let! fieldType = choice
                    return Op.Field (owner, name, flags, fieldType)
                }
                5,
                gen {
                    let! owner = choice
                    let! name = genName
                    let! flags = choice
                    let! parameters = Gen.listOfLength 3 choice |> Gen.bind Gen.subListOf
                    let! returnType = choice
                    let! body = genBody
                    return Op.Method (owner, name, flags, List.ofSeq parameters, returnType, body)
                }
                4,
                gen {
                    let! methodChoice = choice
                    let! sequence = Gen.choose (0, 3)
                    let! name = genName
                    let! flags = choice
                    return Op.Param (methodChoice, sequence, name, flags)
                }
                2, Gen.map2 (fun c i -> Op.InterfaceImpl (c, i)) choice choice
                2,
                gen {
                    let! parent = choice
                    let! name = genName
                    let! parameters = Gen.optionOf (Gen.listOfLength 2 choice |> Gen.bind Gen.subListOf)
                    return Op.MemberRef (parent, $"M%s{name}", parameters |> Option.map List.ofSeq)
                }
                // A local signature holds at least one local (ECMA-335 II.23.2.6).
                1,
                Gen.listOfLength 3 choice
                |> Gen.bind Gen.subListOf
                |> Gen.map2 (fun first rest -> Op.StandAloneSig (first :: List.ofSeq rest)) choice
                2, Gen.map3 (fun c b d -> Op.MethodImpl (c, b, d)) choice choice choice
                2, Gen.map2 (fun m f -> Op.SetMethodImplFlags (m, f)) choice choice
                2, Gen.map2 (fun t e -> Op.SetTypeDefExtends (t, e)) choice choice
                2, Gen.map2 (fun m b -> Op.SetMethodBody (m, b)) choice genBody
            ]

    let pick (choice : int) (items : 'a list) : 'a option =
        match items with
        | [] -> None
        | _ -> Some (List.item (choice % List.length items) items)

    let typeDefOrRefs (scope : EmitScope) : TypeDefOrRef list =
        [
            for i in 1 .. (EmitScope.typeDefs scope).Count do
                yield TypeDefOrRef.TypeDef (TypeDefRid i)
            for i in 1 .. (EmitScope.typeRefs scope).Count do
                yield TypeDefOrRef.TypeRef (TypeRefRid i)
        ]

    let compressedUnsigned (value : int) : byte list =
        if value < 0x80 then
            [ byte value ]
        elif value < 0x4000 then
            [ byte (0x80 ||| (value >>> 8)) ; byte value ]
        else
            [
                byte (0xC0 ||| (value >>> 24))
                byte (value >>> 16)
                byte (value >>> 8)
                byte value
            ]

    /// The element type a choice names, as ECMA-335 II.23.2.12 encodes it: a primitive, or a class
    /// already in the scope.
    let elementType (scope : EmitScope) (choice : int) : byte list =
        match choice % 6 with
        | 0 -> [ 0x08uy ] // int32
        | 1 -> [ 0x0Euy ] // string
        | 2 -> [ 0x1Cuy ] // object
        | 3 -> [ 0x02uy ] // bool
        | _ ->
            match pick choice (typeDefOrRefs scope) with
            | None -> [ 0x08uy ]
            | Some (TypeDefOrRef.TypeDef rid) -> 0x12uy :: compressedUnsigned ((rid.Rid <<< 2) ||| 0)
            | Some (TypeDefOrRef.TypeRef rid) -> 0x12uy :: compressedUnsigned ((rid.Rid <<< 2) ||| 1)

    let methodSignature (scope : EmitScope) (hasThis : bool) (parameters : int list) (returnType : int) =
        [
            yield (if hasThis then 0x20uy else 0x00uy)
            yield! compressedUnsigned parameters.Length
            yield!
                if returnType % 7 = 6 then
                    [ 0x01uy ]
                else
                    elementType scope returnType
            for p in parameters do
                yield! elementType scope p
        ]
        |> ImmutableArray.CreateRange

    let body (scope : EmitScope) (shape : BodyShape) : ImmutableArray<byte> option =
        match shape with
        | BodyShape.NoBody -> None
        | BodyShape.Tiny nops ->
            [
                yield byte (((nops + 1) <<< 2) ||| 0x2)
                yield! List.replicate nops 0x00uy
                yield 0x2Auy
            ]
            |> ImmutableArray.CreateRange
            |> Some
        | BodyShape.Fat (nops, maxStack, locals) ->
            let localsToken =
                match locals with
                | Some c when (EmitScope.standAloneSigs scope).Count > 0 ->
                    0x11000000 ||| (c % (EmitScope.standAloneSigs scope).Count + 1)
                | _ -> 0

            let codeSize = nops + 1

            [
                // Fat format, three dwords of header; `CorILMethod_InitLocals` when there are locals.
                yield! BitConverter.GetBytes (uint16 (0x3003 ||| (if localsToken = 0 then 0 else 0x10)))
                yield! BitConverter.GetBytes maxStack
                yield! BitConverter.GetBytes codeSize
                yield! BitConverter.GetBytes localsToken
                yield! List.replicate nops 0x00uy
                yield 0x2Auy
            ]
            |> ImmutableArray.CreateRange
            |> Some

    let private intern (value : string) (applied : Applied) : StringOffset * Applied =
        let offset, scope = EmitScope.internString value applied.Scope

        offset,
        { applied with
            Scope = scope
        }

    let private internBlob (value : ImmutableArray<byte>) (applied : Applied) : BlobOffset * Applied =
        let offset, scope = EmitScope.internBlob value applied.Scope

        offset,
        { applied with
            Scope = scope
        }

    let private withScope (scope : EmitScope) (applied : Applied) : Applied =
        { applied with
            Scope = scope
        }

    let private applyOp (applied : Applied) (op : Op) : Applied =
        let scope = applied.Scope
        let typeDefCount = (EmitScope.typeDefs scope).Count
        let methodCount = (EmitScope.methodDefs scope).Count

        match op with
        | Op.AssemblyRef (name, version, withToken) ->
            let name, applied = intern name applied

            let token, applied =
                internBlob
                    (if withToken then
                         ImmutableArray.Create<byte>
                             [| 0xb7uy ; 0x7auy ; 0x5cuy ; 0x56uy ; 0x19uy ; 0x34uy ; 0xe0uy ; 0x89uy |]
                     else
                         ImmutableArray.Empty)
                    applied

            let row : AssemblyRefRow =
                {
                    Version =
                        {
                            Major = version
                            Minor = 0us
                            Build = 1us
                            Revision = 2us
                        }
                    Flags = enum<AssemblyFlags> 0
                    PublicKeyOrToken = token
                    Name = name
                    Culture = StringOffset 0
                    HashValue = BlobOffset 0
                }

            withScope (EmitScope.addAssemblyRef row applied.Scope |> snd) applied
        | Op.TypeRef (scopeChoice, name, ns) ->
            let scopes =
                [
                    yield ResolutionScope.Module
                    for i in 1 .. (EmitScope.assemblyRefs scope).Count do
                        yield ResolutionScope.AssemblyRef (AssemblyRefRid i)
                    for i in 1 .. (EmitScope.typeRefs scope).Count do
                        yield ResolutionScope.TypeRef (TypeRefRid i)
                ]

            let name, applied = intern name applied
            let ns, applied = intern ns applied

            let row : TypeRefRow =
                {
                    ResolutionScope = (pick scopeChoice scopes).Value
                    Name = name
                    Namespace = ns
                }

            withScope (EmitScope.addTypeRef row applied.Scope |> snd) applied
        | Op.TypeDef (name, ns, flags, extends) ->
            // Unique, so that no two top-level types share a name, which an image may not have.
            let name, applied = intern $"%s{name}`T%d{typeDefCount + 1}" applied
            let ns, applied = intern ns applied

            let row : TypeDefRow =
                {
                    Flags = (pick flags typeFlagChoices).Value
                    Name = name
                    Namespace = ns
                    Extends = extends |> Option.bind (fun c -> pick c (typeDefOrRefs scope))
                }

            withScope (EmitScope.addTypeDef row applied.Scope |> snd) applied
        | Op.Field (owner, name, flags, fieldType) ->
            let name, applied = intern name applied

            let signature, applied =
                internBlob (ImmutableArray.CreateRange (0x06uy :: elementType scope fieldType)) applied

            let row : FieldRow =
                {
                    Owner = TypeDefRid (owner % typeDefCount + 1)
                    Flags = (pick flags fieldFlagChoices).Value
                    Name = name
                    Signature = signature
                }

            withScope (EmitScope.addField row applied.Scope |> snd) applied
        | Op.Method (owner, name, flags, parameters, returnType, shape) ->
            let flags = (pick flags methodFlagChoices).Value
            let name, applied = intern name applied

            let signature, applied =
                internBlob
                    (methodSignature scope (not (flags.HasFlag MethodAttributes.Static)) parameters returnType)
                    applied

            let row : MethodDefRow =
                {
                    Owner = TypeDefRid (owner % typeDefCount + 1)
                    Body =
                        if flags.HasFlag MethodAttributes.Abstract then
                            None
                        else
                            body scope shape
                    ImplFlags = MethodImplAttributes.IL
                    Flags = flags
                    Name = name
                    Signature = signature
                }

            { withScope (EmitScope.addMethodDef row applied.Scope |> snd) applied with
                ParamCounts = applied.ParamCounts @ [ parameters.Length ]
            }
        | Op.Param (methodChoice, sequence, name, flags) ->
            if methodCount = 0 then
                applied
            else
                let methodIndex = methodChoice % methodCount
                let sequence = sequence % (List.item methodIndex applied.ParamCounts + 1)

                if applied.ParamSequences.Contains (methodIndex, sequence) then
                    applied
                else
                    let name, applied = intern name applied

                    let row : ParamRow =
                        {
                            Owner = MethodDefRid (methodIndex + 1)
                            Flags = (pick flags paramFlagChoices).Value
                            Sequence = uint16 sequence
                            Name = name
                        }

                    { withScope (EmitScope.addParam row applied.Scope |> snd) applied with
                        ParamSequences = applied.ParamSequences.Add (methodIndex, sequence)
                    }
        | Op.InterfaceImpl (cls, iface) ->
            let row : InterfaceImplRow =
                {
                    Class = TypeDefRid (cls % typeDefCount + 1)
                    Interface = (pick iface (typeDefOrRefs scope)).Value
                }

            withScope (EmitScope.addInterfaceImpl row applied.Scope) applied
        | Op.MemberRef (parentChoice, name, parameters) ->
            let parents =
                [
                    for p in typeDefOrRefs scope do
                        match p with
                        | TypeDefOrRef.TypeDef rid -> yield MemberRefParent.TypeDef rid
                        | TypeDefOrRef.TypeRef rid -> yield MemberRefParent.TypeRef rid
                ]

            let name, applied = intern name applied

            let signature, applied =
                match parameters with
                | Some parameters -> internBlob (methodSignature scope true parameters 6) applied
                | None -> internBlob (ImmutableArray.Create<byte> [| 0x06uy ; 0x08uy |]) applied

            let row : MemberRefRow =
                {
                    Parent = (pick parentChoice parents).Value
                    Name = name
                    Signature = signature
                }

            let rid, scope = EmitScope.addMemberRef row applied.Scope

            { withScope scope applied with
                MethodRefs =
                    if parameters.IsSome then
                        applied.MethodRefs @ [ rid ]
                    else
                        applied.MethodRefs
            }
        | Op.StandAloneSig locals ->
            let signature, applied =
                [
                    yield 0x07uy
                    yield! compressedUnsigned locals.Length
                    for l in locals do
                        yield! elementType scope l
                ]
                |> ImmutableArray.CreateRange
                |> fun blob -> internBlob blob applied

            let row : StandAloneSigRow =
                {
                    Signature = signature
                }

            withScope (EmitScope.addStandAloneSig row applied.Scope |> snd) applied
        | Op.MethodImpl (cls, bodyChoice, declaration) ->
            let methods =
                [
                    for i in 1..methodCount do
                        yield MethodDefOrRef.MethodDef (MethodDefRid i)
                    for r in applied.MethodRefs do
                        yield MethodDefOrRef.MemberRef r
                ]

            match pick bodyChoice methods, pick declaration methods with
            | Some bodyTarget, Some declarationTarget ->
                let row : MethodImplRow =
                    {
                        Class = TypeDefRid (cls % typeDefCount + 1)
                        Body = bodyTarget
                        Declaration = declarationTarget
                    }

                withScope (EmitScope.addMethodImpl row applied.Scope) applied
            | _ -> applied
        | Op.SetMethodImplFlags (methodChoice, flags) ->
            if methodCount = 0 then
                applied
            else
                let method = MethodDefRid (methodChoice % methodCount + 1)
                let flags = (pick flags methodImplFlagChoices).Value
                withScope (EmitScope.setMethodImplFlags method flags applied.Scope) applied
        | Op.SetTypeDefExtends (typeChoice, extendsChoice) ->
            // `TypeBuilder_SetParentType` is never called for the global type.
            if typeDefCount < 2 then
                applied
            else
                let typeDef = TypeDefRid (typeChoice % (typeDefCount - 1) + 2)
                let extends = (pick extendsChoice (typeDefOrRefs scope)).Value
                withScope (EmitScope.setTypeDefExtends typeDef extends applied.Scope) applied
        | Op.SetMethodBody (methodChoice, shape) ->
            if methodCount = 0 then
                applied
            else
                let method = MethodDefRid (methodChoice % methodCount + 1)

                let row = (EmitScope.methodDefs scope).[method.Rid - 1]

                match body scope shape with
                | Some filled when not (row.Flags.HasFlag MethodAttributes.Abstract) ->
                    withScope (EmitScope.setMethodBody method filled applied.Scope) applied
                | Some _
                | None -> applied

    let emptyScope (mvid : Guid) : EmitScope =
        let scope = EmitScope.create "RefEmit_InMemoryManifestModule" mvid
        let name, scope = EmitScope.internString "Generated" scope

        EmitScope.defineAssembly
            {
                HashAlgorithm = AssemblyHashAlgorithm.Sha1
                Version =
                    {
                        Major = 1us
                        Minor = 2us
                        Build = 3us
                        Revision = 4us
                    }
                Flags = enum<AssemblyFlags> 0
                PublicKey = BlobOffset 0
                Name = name
                Culture = StringOffset 0
            }
            scope

    let apply (mvid : Guid) (ops : Op list) : EmitScope =
        let start : Applied =
            {
                Scope = emptyScope mvid
                ParamCounts = []
                ParamSequences = Set.empty
                MethodRefs = []
            }

        (List.fold applyOp start ops).Scope

    let genGuid : Gen<Guid> =
        Gen.arrayOfLength 16 (Gen.choose (0, 255) |> Gen.map byte) |> Gen.map Guid

    let genOps : Gen<Op list> =
        Gen.sized (fun size -> Gen.choose (0, 4 * size) |> Gen.bind (fun n -> Gen.listOfLength n genOp))
        |> Gen.map List.ofSeq

    let genScope : Gen<EmitScope> = Gen.map2 apply genGuid genOps

    // `MetadataReader` over the image's metadata, kept alive by the `PEReader` that owns it.
    let readImage (image : byte[]) : PEReader * MetadataReader =
        let pe = new PEReader (ImmutableArray.Create<byte> image)
        pe, pe.GetMetadataReader ()

    let token (handle : EntityHandle) : int =
        if handle.IsNil then 0 else MetadataTokens.GetToken handle

    /// One row of a table that does not need sorting, as the image holds it: heap offsets and tokens,
    /// without the derived list columns.
    let describeRows (reader : MetadataReader) : Map<TableIndex * int, obj> =
        let heap (h : StringHandle) = MetadataTokens.GetHeapOffset h
        let blob (h : BlobHandle) = MetadataTokens.GetHeapOffset h

        [
            for i in 1 .. reader.GetTableRowCount TableIndex.AssemblyRef do
                let r = reader.GetAssemblyReference (MetadataTokens.AssemblyReferenceHandle i)

                yield
                    (TableIndex.AssemblyRef, i),
                    box (r.Version, r.Flags, blob r.PublicKeyOrToken, heap r.Name, heap r.Culture)
            for i in 1 .. reader.GetTableRowCount TableIndex.TypeRef do
                let r = reader.GetTypeReference (MetadataTokens.TypeReferenceHandle i)
                yield (TableIndex.TypeRef, i), box (token r.ResolutionScope, heap r.Name, heap r.Namespace)
            for i in 1 .. reader.GetTableRowCount TableIndex.TypeDef do
                let r = reader.GetTypeDefinition (MetadataTokens.TypeDefinitionHandle i)
                yield (TableIndex.TypeDef, i), box (r.Attributes, heap r.Name, heap r.Namespace, token r.BaseType)
            for i in 1 .. reader.GetTableRowCount TableIndex.Field do
                let r = reader.GetFieldDefinition (MetadataTokens.FieldDefinitionHandle i)

                yield
                    (TableIndex.Field, i),
                    box (
                        r.Attributes,
                        heap r.Name,
                        blob r.Signature,
                        token (TypeDefinitionHandle.op_Implicit (r.GetDeclaringType ()))
                    )
            for i in 1 .. reader.GetTableRowCount TableIndex.MethodDef do
                let r = reader.GetMethodDefinition (MetadataTokens.MethodDefinitionHandle i)

                yield
                    (TableIndex.MethodDef, i),
                    box (
                        r.Attributes,
                        r.ImplAttributes,
                        heap r.Name,
                        blob r.Signature,
                        token (TypeDefinitionHandle.op_Implicit (r.GetDeclaringType ()))
                    )
            for i in 1 .. reader.GetTableRowCount TableIndex.Param do
                let r = reader.GetParameter (MetadataTokens.ParameterHandle i)
                yield (TableIndex.Param, i), box (r.Attributes, r.SequenceNumber, heap r.Name)
            for i in 1 .. reader.GetTableRowCount TableIndex.MemberRef do
                let r = reader.GetMemberReference (MetadataTokens.MemberReferenceHandle i)
                yield (TableIndex.MemberRef, i), box (token r.Parent, heap r.Name, blob r.Signature)
            for i in 1 .. reader.GetTableRowCount TableIndex.StandAloneSig do
                let r = reader.GetStandaloneSignature (MetadataTokens.StandaloneSignatureHandle i)
                yield (TableIndex.StandAloneSig, i), box (blob r.Signature)
        ]
        |> Map.ofList

    /// `describeRows` without the columns the emitter fills in after adding a row (a method's
    /// implementation flags, a type's parent), which an extension of a scope may change while the
    /// row's type is not created.
    let withoutFilledColumns (rows : Map<TableIndex * int, obj>) : Map<TableIndex * int, obj> =
        rows
        |> Map.map (fun (table, _) row ->
            if table = TableIndex.MethodDef then
                let attributes, _, name, signature, owner =
                    unbox<MethodAttributes * MethodImplAttributes * int * int * int> row

                box (attributes, name, signature, owner)
            elif table = TableIndex.TypeDef then
                let attributes, name, ns, _ = unbox<TypeAttributes * int * int * int> row
                box (attributes, name, ns)
            else
                row
        )

namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.Extensions.Logging.Abstractions
open NUnit.Framework
open WoofWare.PawPrint

/// `EmitScopeImage.write`, read back by SRM's `MetadataReader` (which ECMA-335 images PawPrint's loader
/// is built on), and by `Assembly.read`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeImage =

    /// A body as ECMA-335 II.25.4 lays it out: `nop`s then `ret`.
    type BodyShape =
        | Tiny of nops : int
        /// `locals` picks a `StandAloneSig` to name as the local signature, if the scope has any.
        | Fat of nops : int * maxStack : uint16 * locals : int option

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
    let private genName : Gen<string> =
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

    let private genNamespace : Gen<string> = Gen.elements [ "" ; "A" ; "A.B" ; "日本" ]

    let private typeFlagChoices : TypeAttributes list =
        [
            TypeAttributes.Public ||| TypeAttributes.Class
            TypeAttributes.NotPublic ||| TypeAttributes.Class
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract
            TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.BeforeFieldInit
        ]

    let private fieldFlagChoices : FieldAttributes list =
        [
            FieldAttributes.Public
            FieldAttributes.Private ||| FieldAttributes.Static
            FieldAttributes.Assembly ||| FieldAttributes.InitOnly
        ]

    let private methodFlagChoices : MethodAttributes list =
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

    let private paramFlagChoices : ParameterAttributes list =
        [
            ParameterAttributes.None
            ParameterAttributes.In
            ParameterAttributes.Out
            ParameterAttributes.Optional
        ]

    let private genBody : Gen<BodyShape> =
        Gen.oneof
            [
                Gen.choose (0, 60) |> Gen.map BodyShape.Tiny
                gen {
                    let! nops = Gen.choose (0, 100)
                    let! maxStack = Gen.choose (0, 20) |> Gen.map uint16
                    let! locals = Gen.optionOf (Gen.choose (0, 1000))
                    return BodyShape.Fat (nops, maxStack, locals)
                }
            ]

    let private choice : Gen<int> = Gen.choose (0, 1000)

    let private genOp : Gen<Op> =
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
            ]

    let private pick (choice : int) (items : 'a list) : 'a option =
        match items with
        | [] -> None
        | _ -> Some (List.item (choice % List.length items) items)

    let private typeDefOrRefs (scope : EmitScope) : TypeDefOrRef list =
        [
            for i in 1 .. (EmitScope.typeDefs scope).Count do
                yield TypeDefOrRef.TypeDef (TypeDefRid i)
            for i in 1 .. (EmitScope.typeRefs scope).Count do
                yield TypeDefOrRef.TypeRef (TypeRefRid i)
        ]

    let private compressedUnsigned (value : int) : byte list =
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
    let private elementType (scope : EmitScope) (choice : int) : byte list =
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

    let private methodSignature (scope : EmitScope) (hasThis : bool) (parameters : int list) (returnType : int) =
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

    let private body (scope : EmitScope) (shape : BodyShape) : ImmutableArray<byte> =
        match shape with
        | BodyShape.Tiny nops ->
            [
                yield byte (((nops + 1) <<< 2) ||| 0x2)
                yield! List.replicate nops 0x00uy
                yield 0x2Auy
            ]
            |> ImmutableArray.CreateRange
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
                            Some (body scope shape)
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

    let private emptyScope (mvid : Guid) : EmitScope =
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

    let private apply (mvid : Guid) (ops : Op list) : EmitScope =
        let start : Applied =
            {
                Scope = emptyScope mvid
                ParamCounts = []
                ParamSequences = Set.empty
                MethodRefs = []
            }

        (List.fold applyOp start ops).Scope

    let private genGuid : Gen<Guid> =
        Gen.arrayOfLength 16 (Gen.choose (0, 255) |> Gen.map byte) |> Gen.map Guid

    let private genOps : Gen<Op list> =
        Gen.sized (fun size -> Gen.choose (0, 4 * size) |> Gen.bind (fun n -> Gen.listOfLength n genOp))
        |> Gen.map List.ofSeq

    let private genScope : Gen<EmitScope> = Gen.map2 apply genGuid genOps

    // `MetadataReader` over the image's metadata, kept alive by the `PEReader` that owns it.
    let private readImage (image : byte[]) : PEReader * MetadataReader =
        let pe = new PEReader (ImmutableArray.Create<byte> image)
        pe, pe.GetMetadataReader ()

    let private token (handle : EntityHandle) : int =
        if handle.IsNil then 0 else MetadataTokens.GetToken handle

    let private typeDefOrRefToken (target : TypeDefOrRef) : int =
        match target with
        | TypeDefOrRef.TypeDef rid -> 0x02000000 ||| rid.Rid
        | TypeDefOrRef.TypeRef rid -> 0x01000000 ||| rid.Rid

    let private methodDefOrRefToken (target : MethodDefOrRef) : int =
        match target with
        | MethodDefOrRef.MethodDef rid -> 0x06000000 ||| rid.Rid
        | MethodDefOrRef.MemberRef rid -> 0x0A000000 ||| rid.Rid

    let private checkString
        (scope : EmitScope)
        (reader : MetadataReader)
        (expected : StringOffset)
        (actual : StringHandle)
        =
        MetadataTokens.GetHeapOffset actual |> shouldEqual expected.Offset
        reader.GetString actual |> shouldEqual (EmitScope.stringAt expected scope)

    let private checkBlob (scope : EmitScope) (reader : MetadataReader) (expected : BlobOffset) (actual : BlobHandle) =
        MetadataTokens.GetHeapOffset actual |> shouldEqual expected.Offset

        reader.GetBlobBytes actual
        |> List.ofArray
        |> shouldEqual (EmitScope.blobAt expected scope |> List.ofSeq)

    let private checkVersion (expected : MetadataVersion) (actual : Version) : unit =
        actual
        |> shouldEqual (Version (int expected.Major, int expected.Minor, int expected.Build, int expected.Revision))

    /// Whether the children's owners never decrease in RID order, so that each owner's children are one
    /// contiguous run and no pointer table is needed.
    let private grouped (owners : int seq) : bool =
        owners |> Seq.pairwise |> Seq.forall (fun (a, b) -> a <= b)

    let private ridsOwnedBy (owner : int) (owners : int seq) : int list =
        owners
        |> Seq.indexed
        |> Seq.filter (fun (_, o) -> o = owner)
        |> Seq.map (fun (i, _) -> i + 1)
        |> List.ofSeq

    /// Every row of the scope reads back from the image with the same columns at the RID the scope gave
    /// it, and every heap reference at the offset the scope gave it.
    let private checkRoundTrip (scope : EmitScope) (image : byte[]) : unit =
        let pe, reader = readImage image
        use _ = pe

        let moduleRow = EmitScope.moduleRow scope
        let moduleDefinition = reader.GetModuleDefinition ()
        moduleDefinition.Generation |> shouldEqual 0
        checkString scope reader moduleRow.Name moduleDefinition.Name

        MetadataTokens.GetHeapOffset moduleDefinition.Mvid
        |> shouldEqual moduleRow.Mvid.Index

        reader.GetGuid moduleDefinition.Mvid
        |> shouldEqual (EmitScope.guidAt moduleRow.Mvid scope)

        moduleDefinition.GenerationId.IsNil |> shouldEqual true
        moduleDefinition.BaseGenerationId.IsNil |> shouldEqual true

        match EmitScope.assembly scope with
        | None -> reader.IsAssembly |> shouldEqual false
        | Some expected ->
            let actual = reader.GetAssemblyDefinition ()
            actual.HashAlgorithm |> shouldEqual expected.HashAlgorithm
            checkVersion expected.Version actual.Version
            actual.Flags |> shouldEqual expected.Flags
            checkBlob scope reader expected.PublicKey actual.PublicKey
            checkString scope reader expected.Name actual.Name
            checkString scope reader expected.Culture actual.Culture

        let assemblyRefs = EmitScope.assemblyRefs scope
        reader.GetTableRowCount TableIndex.AssemblyRef |> shouldEqual assemblyRefs.Count

        for i in 1 .. assemblyRefs.Count do
            let expected = assemblyRefs.[i - 1]
            let actual = reader.GetAssemblyReference (MetadataTokens.AssemblyReferenceHandle i)
            checkVersion expected.Version actual.Version
            actual.Flags |> shouldEqual expected.Flags
            checkBlob scope reader expected.PublicKeyOrToken actual.PublicKeyOrToken
            checkString scope reader expected.Name actual.Name
            checkString scope reader expected.Culture actual.Culture
            checkBlob scope reader expected.HashValue actual.HashValue

        let typeRefs = EmitScope.typeRefs scope
        reader.GetTableRowCount TableIndex.TypeRef |> shouldEqual typeRefs.Count

        for i in 1 .. typeRefs.Count do
            let expected = typeRefs.[i - 1]
            let actual = reader.GetTypeReference (MetadataTokens.TypeReferenceHandle i)

            token actual.ResolutionScope
            |> shouldEqual (
                match expected.ResolutionScope with
                | ResolutionScope.Module -> 0x00000001
                | ResolutionScope.AssemblyRef rid -> 0x23000000 ||| rid.Rid
                | ResolutionScope.TypeRef rid -> 0x01000000 ||| rid.Rid
            )

            checkString scope reader expected.Name actual.Name
            checkString scope reader expected.Namespace actual.Namespace

        let fields = EmitScope.fields scope
        let methods = EmitScope.methodDefs scope
        let parameters = EmitScope.parameters scope
        let fieldOwners = fields |> Seq.map _.Owner.Rid
        let methodOwners = methods |> Seq.map _.Owner.Rid
        let paramOwners = parameters |> Seq.map _.Owner.Rid

        // A pointer table exactly when the children are not already grouped by parent.
        reader.GetTableRowCount TableIndex.FieldPtr
        |> shouldEqual (if grouped fieldOwners then 0 else fields.Count)

        reader.GetTableRowCount TableIndex.MethodPtr
        |> shouldEqual (if grouped methodOwners then 0 else methods.Count)

        reader.GetTableRowCount TableIndex.ParamPtr
        |> shouldEqual (if grouped paramOwners then 0 else parameters.Count)

        let typeDefs = EmitScope.typeDefs scope
        let interfaceImpls = EmitScope.interfaceImpls scope
        let methodImpls = EmitScope.methodImpls scope
        reader.GetTableRowCount TableIndex.TypeDef |> shouldEqual typeDefs.Count

        reader.GetTableRowCount TableIndex.InterfaceImpl
        |> shouldEqual interfaceImpls.Count

        reader.GetTableRowCount TableIndex.MethodImpl |> shouldEqual methodImpls.Count

        for i in 1 .. typeDefs.Count do
            let expected = typeDefs.[i - 1]
            let handle = MetadataTokens.TypeDefinitionHandle i
            let actual = reader.GetTypeDefinition handle
            actual.Attributes |> shouldEqual expected.Flags
            checkString scope reader expected.Name actual.Name
            checkString scope reader expected.Namespace actual.Namespace

            token actual.BaseType
            |> shouldEqual (expected.Extends |> Option.map typeDefOrRefToken |> Option.defaultValue 0)

            actual.GetFields ()
            |> Seq.map MetadataTokens.GetRowNumber
            |> List.ofSeq
            |> shouldEqual (ridsOwnedBy i fieldOwners)

            actual.GetMethods ()
            |> Seq.map MetadataTokens.GetRowNumber
            |> List.ofSeq
            |> shouldEqual (ridsOwnedBy i methodOwners)

            // A stable sort by class keeps each class's rows in the order they were added.
            actual.GetInterfaceImplementations ()
            |> Seq.map (fun h -> reader.GetInterfaceImplementation(h).Interface |> token)
            |> List.ofSeq
            |> shouldEqual (
                interfaceImpls
                |> Seq.filter (fun row -> row.Class.Rid = i)
                |> Seq.map (fun row -> typeDefOrRefToken row.Interface)
                |> List.ofSeq
            )

            actual.GetMethodImplementations ()
            |> Seq.map (fun h ->
                let row = reader.GetMethodImplementation h
                token row.MethodBody, token row.MethodDeclaration
            )
            |> List.ofSeq
            |> shouldEqual (
                methodImpls
                |> Seq.filter (fun row -> row.Class.Rid = i)
                |> Seq.map (fun row -> methodDefOrRefToken row.Body, methodDefOrRefToken row.Declaration)
                |> List.ofSeq
            )

        reader.GetTableRowCount TableIndex.Field |> shouldEqual fields.Count

        for i in 1 .. fields.Count do
            let expected = fields.[i - 1]
            let actual = reader.GetFieldDefinition (MetadataTokens.FieldDefinitionHandle i)
            actual.Attributes |> shouldEqual expected.Flags
            checkString scope reader expected.Name actual.Name
            checkBlob scope reader expected.Signature actual.Signature

            MetadataTokens.GetRowNumber (actual.GetDeclaringType ())
            |> shouldEqual expected.Owner.Rid

        reader.GetTableRowCount TableIndex.MethodDef |> shouldEqual methods.Count

        for i in 1 .. methods.Count do
            let expected = methods.[i - 1]
            let actual = reader.GetMethodDefinition (MetadataTokens.MethodDefinitionHandle i)
            actual.Attributes |> shouldEqual expected.Flags
            actual.ImplAttributes |> shouldEqual expected.ImplFlags
            checkString scope reader expected.Name actual.Name
            checkBlob scope reader expected.Signature actual.Signature

            MetadataTokens.GetRowNumber (actual.GetDeclaringType ())
            |> shouldEqual expected.Owner.Rid

            match expected.Body with
            | None -> actual.RelativeVirtualAddress |> shouldEqual 0
            | Some body ->
                actual.RelativeVirtualAddress |> shouldNotEqual 0

                pe.GetSectionData(actual.RelativeVirtualAddress).GetContent (0, body.Length)
                |> List.ofSeq
                |> shouldEqual (List.ofSeq body)

            actual.GetParameters ()
            |> Seq.map MetadataTokens.GetRowNumber
            |> List.ofSeq
            |> shouldEqual (ridsOwnedBy i paramOwners)

        reader.GetTableRowCount TableIndex.Param |> shouldEqual parameters.Count

        for i in 1 .. parameters.Count do
            let expected = parameters.[i - 1]
            let actual = reader.GetParameter (MetadataTokens.ParameterHandle i)
            actual.Attributes |> shouldEqual expected.Flags
            actual.SequenceNumber |> shouldEqual (int expected.Sequence)
            checkString scope reader expected.Name actual.Name

        let memberRefs = EmitScope.memberRefs scope
        reader.GetTableRowCount TableIndex.MemberRef |> shouldEqual memberRefs.Count

        for i in 1 .. memberRefs.Count do
            let expected = memberRefs.[i - 1]
            let actual = reader.GetMemberReference (MetadataTokens.MemberReferenceHandle i)

            token actual.Parent
            |> shouldEqual (
                match expected.Parent with
                | MemberRefParent.TypeDef rid -> 0x02000000 ||| rid.Rid
                | MemberRefParent.TypeRef rid -> 0x01000000 ||| rid.Rid
                | MemberRefParent.MethodDef rid -> 0x06000000 ||| rid.Rid
            )

            checkString scope reader expected.Name actual.Name
            checkBlob scope reader expected.Signature actual.Signature

        let sigs = EmitScope.standAloneSigs scope
        reader.GetTableRowCount TableIndex.StandAloneSig |> shouldEqual sigs.Count

        for i in 1 .. sigs.Count do
            let actual =
                reader.GetStandaloneSignature (MetadataTokens.StandaloneSignatureHandle i)

            checkBlob scope reader sigs.[i - 1].Signature actual.Signature

        // Nothing the scope cannot hold.
        for table in
            [
                TableIndex.Constant
                TableIndex.CustomAttribute
                TableIndex.TypeSpec
                TableIndex.ModuleRef
                TableIndex.NestedClass
                TableIndex.GenericParam
                TableIndex.MethodSpec
                TableIndex.Property
                TableIndex.Event
            ] do
            reader.GetTableRowCount table |> shouldEqual 0

        reader.GetUserString (MetadataTokens.UserStringHandle 0) |> shouldEqual ""

    [<Test>]
    let ``every row reads back at the RID the scope gave it`` () : unit =
        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen genScope) (fun scope -> checkRoundTrip scope (EmitScopeImage.write scope))
        )

    [<Test>]
    let ``the generator makes scopes with and without pointer tables`` () : unit =
        let scopes = Gen.sampleWithSeed (Rnd 20261105UL) 30 200 genScope

        let ungrouped (owners : EmitScope -> int seq) =
            scopes |> Seq.filter (fun s -> not (grouped (owners s))) |> Seq.length

        let methodOwners (s : EmitScope) =
            EmitScope.methodDefs s |> Seq.map _.Owner.Rid

        let fieldOwners (s : EmitScope) =
            EmitScope.fields s |> Seq.map _.Owner.Rid

        let paramOwners (s : EmitScope) =
            EmitScope.parameters s |> Seq.map _.Owner.Rid

        for owners in [ methodOwners ; fieldOwners ; paramOwners ] do
            ungrouped owners |> shouldBeGreaterThan 20
            (scopes.Length - ungrouped owners) |> shouldBeGreaterThan 20

        // And sorted tables that must be re-sorted.
        scopes
        |> Seq.filter (fun s -> not (grouped (EmitScope.interfaceImpls s |> Seq.map _.Class.Rid)))
        |> Seq.length
        |> shouldBeGreaterThan 20

        scopes
        |> Seq.filter (fun s -> not (grouped (EmitScope.methodImpls s |> Seq.map _.Class.Rid)))
        |> Seq.length
        |> shouldBeGreaterThan 20

    [<Test>]
    let ``Assembly.read lists each type's methods and fields as the scope defines them`` () : unit =
        let property (scope : EmitScope) : unit =
            use stream = new MemoryStream (EmitScopeImage.write scope)
            use assy = Assembly.read NullLoggerFactory.Instance None stream
            let typeDefs = EmitScope.typeDefs scope
            assy.TypeDefs.Count |> shouldEqual typeDefs.Count

            for i in 1 .. typeDefs.Count do
                let ty = assy.TypeDefs.[MetadataTokens.TypeDefinitionHandle i]
                ty.Name |> shouldEqual (EmitScope.stringAt typeDefs.[i - 1].Name scope)

                ty.Methods
                |> List.map (fun m -> m.TryMetadata.Value.Handle |> MetadataTokens.GetRowNumber)
                |> shouldEqual (ridsOwnedBy i (EmitScope.methodDefs scope |> Seq.map _.Owner.Rid))

                ty.Fields
                |> List.map (fun f ->
                    MetadataTokens.GetRowNumber (FieldDefinitionHandle.op_Implicit f.Handle : EntityHandle)
                )
                |> shouldEqual (ridsOwnedBy i (EmitScope.fields scope |> Seq.map _.Owner.Rid))

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 100, Prop.forAll (Arb.fromGen genScope) property)

    /// One row of a table that does not need sorting, as the image holds it: heap offsets and tokens,
    /// without the derived list columns.
    let private describeRows (reader : MetadataReader) : Map<TableIndex * int, obj> =
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

    [<Test>]
    let ``extending a scope leaves every row and heap entry it had where it was`` () : unit =
        let property (mvid : Guid) (ops : Op list) (split : int) : unit =
            let prefix = apply mvid (List.take (split % (ops.Length + 1)) ops)
            let extended = apply mvid ops
            let prefixPe, prefixReader = readImage (EmitScopeImage.write prefix)
            use _ = prefixPe
            let extendedPe, extendedReader = readImage (EmitScopeImage.write extended)
            use _ = extendedPe

            let before = describeRows prefixReader
            let after = describeRows extendedReader

            for KeyValue (key, row) in before do
                after.TryFind key |> shouldEqual (Some row)

            for value in EmitScope.strings prefix do
                let offset = EmitScope.internString value prefix |> fst

                extendedReader.GetString (MetadataTokens.StringHandle offset.Offset)
                |> shouldEqual value

            for value in EmitScope.blobs prefix do
                let offset = EmitScope.internBlob value prefix |> fst

                extendedReader.GetBlobBytes (MetadataTokens.BlobHandle offset.Offset)
                |> List.ofArray
                |> shouldEqual (List.ofSeq value)

            // A type's members only gain later RIDs, so what it had is a prefix of what it has.
            for i in 1 .. prefixReader.GetTableRowCount TableIndex.TypeDef do
                let members (reader : MetadataReader) =
                    let t = reader.GetTypeDefinition (MetadataTokens.TypeDefinitionHandle i)
                    List.ofSeq (t.GetMethods ()), List.ofSeq (t.GetFields ())

                let beforeMethods, beforeFields = members prefixReader
                let afterMethods, afterFields = members extendedReader
                List.truncate beforeMethods.Length afterMethods |> shouldEqual beforeMethods
                List.truncate beforeFields.Length afterFields |> shouldEqual beforeFields

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 genGuid genOps choice))
                (fun (mvid, ops, split) -> property mvid ops split)
        )

    /// What CoreCLR's emitter stores for a UTF-16 string: its UTF-8 encoding, each unpaired surrogate
    /// replaced by U+FFFD (measured on .NET 10: an assembly named "Bad" and U+D800 is named "Bad" and
    /// U+FFFD).
    let private asCoreClrStores (value : string) : string =
        Text.Encoding.UTF8.GetString (Text.Encoding.UTF8.GetBytes value)

    [<Test>]
    let ``a string is interned as the UTF-8 CoreCLR stores, unpaired surrogates replaced`` () : unit =
        let genChar =
            Gen.oneof [ Gen.choose (1, 0xFFFF) |> Gen.map char ; Gen.elements [ char 0xD800 ; char 0xDC00 ; 'a' ] ]

        let property (value : string) : unit =
            let offset, scope = EmitScope.internString value (EmitScope.create "M" Guid.Empty)
            EmitScope.stringAt offset scope |> shouldEqual (asCoreClrStores value)

            // And the string it is stored as is the same entry.
            EmitScope.internString (asCoreClrStores value) scope |> fst |> shouldEqual offset

            let pe, reader = readImage (EmitScopeImage.write scope)
            use _ = pe
            reader.GetString (MetadataTokens.StringHandle offset.Offset) |> shouldEqual (asCoreClrStores value)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (Gen.listOf genChar |> Gen.map (fun cs -> System.String (Array.ofList (List.ofSeq cs)))))
                property
        )

    [<Test>]
    let ``the image is a function of the scope`` () : unit =
        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 50,
            Prop.forAll
                (Arb.fromGen genScope)
                (fun scope -> EmitScopeImage.write scope |> shouldEqual (EmitScopeImage.write scope))
        )

    /// Bulk rows that push one table, or one heap, to either side of the size at which an index into it
    /// widens from two bytes to four (ECMA-335 II.24.2.6).
    type Bulk =
        | TypeRefs of int
        | MethodDefs of int
        | Params of int
        | Fields of int
        | LongString of int
        | LongBlob of int

    let indexWidthBoundaries : obj[] list =
        [
            // `MemberRefParent` has three tag bits, so it widens at 2^13 rows in any of its tables.
            TypeRefs 8191
            TypeRefs 8192
            // `TypeDefOrRef` and `ResolutionScope` have two.
            TypeRefs 16383
            TypeRefs 16384
            // `MethodDefOrRef` has one.
            MethodDefs 32767
            MethodDefs 32768
            // A plain index widens at 2^16 rows.
            Params 65535
            Params 65536
            Fields 65535
            Fields 65536
            // A heap index widens once the heap reaches 2^16 bytes.
            LongString 65530
            LongString 65540
            LongBlob 65530
            LongBlob 65540
        ]
        |> List.map (fun bulk -> [| box bulk |])

    [<TestCaseSource(nameof indexWidthBoundaries)>]
    let ``rows read back on both sides of an index width boundary`` (bulk : Bulk) : unit =
        // A few generated rows of every kind first, so that every coded index has something to name.
        let scope = Gen.sampleWithSeed (Rnd 1234UL) 20 1 genScope |> Array.exactlyOne
        let lastType = TypeDefRid (EmitScope.typeDefs scope).Count
        let emptyName = StringOffset 0

        let fieldSig, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x06uy ; 0x08uy |]) scope

        let methodSig, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x00uy ; 0x00uy ; 0x01uy |]) scope

        let method : MethodDefRow =
            {
                Owner = lastType
                Body = Some (ImmutableArray.Create<byte> [| 0x06uy ; 0x2Auy |])
                ImplFlags = MethodImplAttributes.IL
                Flags = MethodAttributes.Public ||| MethodAttributes.Static
                Name = emptyName
                Signature = methodSig
            }

        // Rows up to `target`, so that the table has exactly that many. Each belongs to the last owner,
        // so that no owner after it has an empty range, whose list column would have to name the row
        // one past the end.
        let padTo (target : int) (count : EmitScope -> int) (add : EmitScope -> EmitScope) (scope : EmitScope) =
            let existing = count scope
            existing |> shouldBeSmallerThan target
            Seq.fold (fun scope _ -> add scope) scope (Seq.replicate (target - existing) ())

        let scope =
            match bulk with
            | Bulk.TypeRefs target ->
                let scope =
                    padTo
                        (target - 1)
                        (fun s -> (EmitScope.typeRefs s).Count)
                        (EmitScope.addTypeRef
                            {
                                ResolutionScope = ResolutionScope.Module
                                Name = emptyName
                                Namespace = emptyName
                            }
                         >> snd)
                        scope

                let last, scope =
                    EmitScope.addTypeRef
                        {
                            ResolutionScope = ResolutionScope.TypeRef (TypeRefRid (target - 1))
                            Name = emptyName
                            Namespace = emptyName
                        }
                        scope

                last.Rid |> shouldEqual target

                // Named by every coded index that can name a TypeRef.
                let scope =
                    EmitScope.addInterfaceImpl
                        {
                            Class = lastType
                            Interface = TypeDefOrRef.TypeRef last
                        }
                        scope

                let _, scope =
                    EmitScope.addMemberRef
                        {
                            Parent = MemberRefParent.TypeRef last
                            Name = emptyName
                            Signature = fieldSig
                        }
                        scope

                EmitScope.addTypeDef
                    {
                        Flags = TypeAttributes.Public
                        Name = emptyName
                        Namespace = emptyName
                        Extends = Some (TypeDefOrRef.TypeRef last)
                    }
                    scope
                |> snd
            | Bulk.MethodDefs target ->
                let scope =
                    padTo target (fun s -> (EmitScope.methodDefs s).Count) (EmitScope.addMethodDef method >> snd) scope

                let last = MethodDefRid target

                let scope =
                    EmitScope.addMethodImpl
                        {
                            Class = lastType
                            Body = MethodDefOrRef.MethodDef last
                            Declaration = MethodDefOrRef.MethodDef last
                        }
                        scope

                EmitScope.addMemberRef
                    {
                        Parent = MemberRefParent.MethodDef last
                        Name = emptyName
                        Signature = methodSig
                    }
                    scope
                |> snd
            | Bulk.Params target ->
                let owner, scope = EmitScope.addMethodDef method scope

                padTo
                    target
                    (fun s -> (EmitScope.parameters s).Count)
                    (EmitScope.addParam
                        {
                            Owner = owner
                            Flags = ParameterAttributes.None
                            Sequence = 1us
                            Name = emptyName
                        }
                     >> snd)
                    scope
            | Bulk.Fields target ->
                padTo
                    target
                    (fun s -> (EmitScope.fields s).Count)
                    (EmitScope.addField
                        {
                            Owner = lastType
                            Flags = FieldAttributes.Public
                            Name = emptyName
                            Signature = fieldSig
                        }
                     >> snd)
                    scope
            | Bulk.LongString length ->
                let scope = EmitScope.internString (System.String ('s', length)) scope |> snd
                // A string interned after the long one, at an offset past it.
                let name, scope = EmitScope.internString "after" scope

                EmitScope.addTypeRef
                    {
                        ResolutionScope = ResolutionScope.Module
                        Name = name
                        Namespace = name
                    }
                    scope
                |> snd
            | Bulk.LongBlob length ->
                let scope =
                    EmitScope.internBlob (ImmutableArray.CreateRange (Array.create length 7uy)) scope
                    |> snd

                let signature, scope =
                    EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x07uy ; 0x00uy |]) scope

                EmitScope.addStandAloneSig
                    {
                        Signature = signature
                    }
                    scope
                |> snd

        checkRoundTrip scope (EmitScopeImage.write scope)

    [<Test>]
    let ``a list column that would name one past a full two-byte table is refused`` () : unit =
        // With 65535 fields, a `Field` index is two bytes, so a type after the last field's owner,
        // whose `FieldList` is the row one past the end, cannot be written: SRM reads 0 there as an
        // empty range, and the type before it would then end at row -1.
        let scope = EmitScope.create "M" Guid.Empty
        let name, scope = EmitScope.internString "T" scope

        let signature, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x06uy ; 0x08uy |]) scope

        let typeDef : TypeDefRow =
            {
                Flags = TypeAttributes.Public
                Name = name
                Namespace = StringOffset 0
                Extends = None
            }

        let owner, scope = EmitScope.addTypeDef typeDef scope

        let field : FieldRow =
            {
                Owner = owner
                Flags = FieldAttributes.Public
                Name = name
                Signature = signature
            }

        let scope =
            Seq.fold (fun scope _ -> EmitScope.addField field scope |> snd) scope (Seq.replicate 65535 ())

        // Fits: the last type's fields run to the end of the table.
        EmitScopeImage.write scope |> ignore<byte[]>

        let _, scope = EmitScope.addTypeDef typeDef scope

        let e =
            Assert.Throws<Exception> (fun () -> EmitScopeImage.write scope |> ignore<byte[]>)

        e.Message |> shouldContainText "65536"

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
open WoofWare.PawPrint.Test.EmitScopeGen

/// `EmitScopeImage.write`, read back by SRM's `MetadataReader` (which ECMA-335 images PawPrint's loader
/// is built on), and by `Assembly.read`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeImage =

    let private typeDefOrRefToken (target : TypeDefOrRef) : int =
        match target with
        | TypeDefOrRef.TypeDef rid -> 0x02000000 ||| rid.Rid
        | TypeDefOrRef.TypeRef rid -> 0x01000000 ||| rid.Rid
        | TypeDefOrRef.TypeSpec rid -> 0x1B000000 ||| rid.Rid

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

    /// Each method's parameter RIDs in the order CoreCLR lists them, keyed by method RID:
    /// `CMiniMdRW::FixParamSequence` (md/enc/metamodelrw.cpp) slides each new parameter ahead of every
    /// earlier one of the same method with a greater sequence number.
    let private paramsInCoreClrOrder (parameters : ParamRow seq) : Map<int, int list> =
        let listed = Collections.Generic.Dictionary<int, ResizeArray<int * uint16>> ()

        parameters
        |> Seq.iteri (fun i p ->
            let list =
                match listed.TryGetValue p.Owner.Rid with
                | true, list -> list
                | false, _ ->
                    let list = ResizeArray ()
                    listed.[p.Owner.Rid] <- list
                    list

            let mutable at = list.Count

            while at > 0 && p.Sequence < snd list.[at - 1] do
                at <- at - 1

            list.Insert (at, (i + 1, p.Sequence))
        )

        listed
        |> Seq.map (fun kv -> kv.Key, kv.Value |> Seq.map fst |> List.ofSeq)
        |> Map.ofSeq

    let private paramsOf (method : int) (order : Map<int, int list>) : int list =
        order.TryFind method |> Option.defaultValue []

    /// Whether every method's parameters, in CoreCLR's order, are already the `Param` table's RID order,
    /// so that no `ParamPtr` is needed.
    let private paramsInRidOrder (methodCount : int) (parameters : ParamRow seq) : bool =
        let order = paramsInCoreClrOrder parameters

        [
            for m in 1..methodCount do
                yield! paramsOf m order
        ] = [ 1 .. Seq.length parameters ]

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

        // A pointer table exactly when the children are not already grouped by parent.
        reader.GetTableRowCount TableIndex.FieldPtr
        |> shouldEqual (if grouped fieldOwners then 0 else fields.Count)

        reader.GetTableRowCount TableIndex.MethodPtr
        |> shouldEqual (if grouped methodOwners then 0 else methods.Count)

        let paramOrder = paramsInCoreClrOrder parameters

        reader.GetTableRowCount TableIndex.ParamPtr
        |> shouldEqual (
            if paramsInRidOrder methods.Count parameters then
                0
            else
                parameters.Count
        )

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
            |> shouldEqual (paramsOf i paramOrder)

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

        let typeSpecs = EmitScope.typeSpecs scope
        reader.GetTableRowCount TableIndex.TypeSpec |> shouldEqual typeSpecs.Count

        for i in 1 .. typeSpecs.Count do
            let actual = reader.GetTypeSpecification (MetadataTokens.TypeSpecificationHandle i)
            checkBlob scope reader typeSpecs.[i - 1].Signature actual.Signature

        // Nothing the scope cannot hold.
        for table in
            [
                TableIndex.Constant
                TableIndex.CustomAttribute
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
    let ``the sorted tables' RIDs name each row where the image put it`` () : unit =
        let property (scope : EmitScope) : unit =
            let pe, reader = readImage (EmitScopeImage.write scope)
            use _ = pe

            let interfaceRids = EmitScopeImage.interfaceImplRids scope
            let interfaceRows = EmitScope.interfaceImpls scope
            List.sort (List.ofSeq interfaceRids) |> shouldEqual [ 1 .. interfaceRows.Count ]

            for i in 0 .. interfaceRows.Count - 1 do
                let row =
                    reader.GetInterfaceImplementation (MetadataTokens.InterfaceImplementationHandle interfaceRids.[i])

                // The class is the row's only column SRM does not expose, so it is read as the type
                // whose list holds the row.
                let owner =
                    reader.TypeDefinitions
                    |> Seq.filter (fun t ->
                        (reader.GetTypeDefinition t).GetInterfaceImplementations ()
                        |> Seq.exists (fun h ->
                            MetadataTokens.GetRowNumber (InterfaceImplementationHandle.op_Implicit h : EntityHandle) = interfaceRids.[i]
                        )
                    )
                    |> Seq.map (fun t ->
                        MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit t : EntityHandle)
                    )
                    |> List.ofSeq

                owner |> shouldEqual [ interfaceRows.[i].Class.Rid ]

                token row.Interface
                |> shouldEqual (typeDefOrRefToken interfaceRows.[i].Interface)

            let implRids = EmitScopeImage.methodImplRids scope
            let implRows = EmitScope.methodImpls scope
            List.sort (List.ofSeq implRids) |> shouldEqual [ 1 .. implRows.Count ]

            for i in 0 .. implRows.Count - 1 do
                let row =
                    reader.GetMethodImplementation (MetadataTokens.MethodImplementationHandle implRids.[i])

                MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit row.Type : EntityHandle)
                |> shouldEqual implRows.[i].Class.Rid

                token row.MethodBody |> shouldEqual (methodDefOrRefToken implRows.[i].Body)

                token row.MethodDeclaration
                |> shouldEqual (methodDefOrRefToken implRows.[i].Declaration)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen genScope) property)

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

        // And parameters grouped by method that still need a pointer table, being out of sequence.
        scopes
        |> Seq.filter (fun s ->
            grouped (paramOwners s)
            && not (paramsInRidOrder (EmitScope.methodDefs s).Count (EmitScope.parameters s))
        )
        |> Seq.length
        |> shouldBeGreaterThan 5

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
        let mutable missing = 0
        let mutable abstractBodies = 0

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

                // A method with a body is IL; one without is abstract if marked so, and otherwise
                // an IL method with no body.
                for m in ty.Methods do
                    let row =
                        (EmitScope.methodDefs scope).[MetadataTokens.GetRowNumber (
                                                          MethodDefinitionHandle.op_Implicit m.TryMetadata.Value.Handle
                                                          : EntityHandle
                                                      )
                                                      - 1]

                    match row.Body, m.Body with
                    | Some _, MethodBody.Il _ -> ()
                    | None, MethodBody.Abstract when row.Flags.HasFlag MethodAttributes.Abstract ->
                        abstractBodies <- abstractBodies + 1
                    | None, MethodBody.MissingIl when not (row.Flags.HasFlag MethodAttributes.Abstract) ->
                        missing <- missing + 1
                    | expected, actual ->
                        failwith
                            $"%s{m.Name}: the row's body %A{expected} (flags %O{row.Flags}) read back as %A{actual}"

                ty.Fields
                |> List.map (fun f ->
                    MetadataTokens.GetRowNumber (FieldDefinitionHandle.op_Implicit f.Handle : EntityHandle)
                )
                |> shouldEqual (ridsOwnedBy i (EmitScope.fields scope |> Seq.map _.Owner.Rid))

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 100, Prop.forAll (Arb.fromGen genScope) property)

        missing |> shouldBeGreaterThan 50
        abstractBodies |> shouldBeGreaterThan 50

    /// A row with no body whose implementation is not managed IL: native code, unmanaged IL, and
    /// optimised IL, none of which the runtime has a body for.
    [<TestCase(0x0001)>]
    [<TestCase(0x0004)>]
    [<TestCase(0x0002)>]
    let ``Assembly.read refuses a method with no body whose code is not managed IL`` (implFlags : int) : unit =
        let scope = emptyScope Guid.Empty
        let name, scope = EmitScope.internString "Holder" scope

        let holder, scope =
            EmitScope.addTypeDef
                {
                    Flags = TypeAttributes.Public
                    Name = name
                    Namespace = StringOffset 0
                    Extends = None
                }
                scope

        let name, scope = EmitScope.internString "NoBody" scope

        let signature, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x00uy ; 0x00uy ; 0x01uy |]) scope

        let _, scope =
            EmitScope.addMethodDef
                {
                    Owner = holder
                    Body = None
                    ImplFlags = enum<MethodImplAttributes> implFlags
                    Flags = MethodAttributes.Public ||| MethodAttributes.Static
                    Name = name
                    Signature = signature
                }
                scope

        use stream = new MemoryStream (EmitScopeImage.write scope)

        let message =
            try
                use _ = Assembly.read NullLoggerFactory.Instance None stream
                failwith "expected Assembly.read to refuse the method"
            with e ->
                e.Message

        message |> shouldContainText "NoBody"
        message |> shouldContainText "not managed IL"

    [<Test>]
    let ``extending a scope leaves every row and heap entry it had where it was, but for columns filled in later``
        ()
        : unit
        =
        let property (mvid : Guid) (ops : Op list) (split : int) : unit =
            let prefix = apply mvid (List.take (split % (ops.Length + 1)) ops)
            let extended = apply mvid ops
            let prefixPe, prefixReader = readImage (EmitScopeImage.write prefix)
            use _ = prefixPe
            let extendedPe, extendedReader = readImage (EmitScopeImage.write extended)
            use _ = extendedPe

            // An extension may fill in a column of an earlier row, so those columns are left out.
            let before = withoutFilledColumns (describeRows prefixReader)
            let after = withoutFilledColumns (describeRows extendedReader)

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
            Gen.oneof
                [
                    Gen.choose (1, 0xFFFF) |> Gen.map char
                    Gen.elements [ char 0xD800 ; char 0xDC00 ; 'a' ]
                ]

        let property (value : string) : unit =
            let offset, scope = EmitScope.internString value (EmitScope.create "M" Guid.Empty)
            EmitScope.stringAt offset scope |> shouldEqual (asCoreClrStores value)

            // And the string it is stored as is the same entry.
            EmitScope.internString (asCoreClrStores value) scope
            |> fst
            |> shouldEqual offset

            let pe, reader = readImage (EmitScopeImage.write scope)
            use _ = pe

            reader.GetString (MetadataTokens.StringHandle offset.Offset)
            |> shouldEqual (asCoreClrStores value)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (
                    Gen.listOf genChar
                    |> Gen.map (fun cs -> System.String (Array.ofList (List.ofSeq cs)))
                ))
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

    /// ECMA-335 II.23.2.6 says a local signature holds at least one local, and SRM's decoder refuses one
    /// with none, but `TypeBuilder_SetMethodIL` gives every body built with no locals the signature
    /// `07 00`, and CoreCLR runs it. `Assembly.read` reads it as a signature of no locals.
    [<Test>]
    let ``a local signature of no locals reads back as an empty one`` () : unit =
        let scope = apply Guid.Empty [ Op.TypeDef ("T", "", 0, None) ]

        let sigRow, scope =
            EmitScopeReferences.defineStandAloneSig (ImmutableArray.Create<byte> [| 0x07uy ; 0x00uy |]) scope

        let name, scope = EmitScope.internString "M" scope

        let signature, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x00uy ; 0x00uy ; 0x08uy |]) scope

        let code = ImmutableArray.Create<byte> [| 0x16uy ; 0x2Auy |]

        let method, scope =
            EmitScope.addMethodDef
                {
                    Owner = TypeDefRid 2
                    Body = Some (EmitScopeDefinitions.methodBody true 1us code (Some sigRow) [])
                    ImplFlags = MethodImplAttributes.IL
                    Flags = MethodAttributes.Public ||| MethodAttributes.Static
                    Name = name
                    Signature = signature
                }
                scope

        use stream = new MemoryStream (EmitScopeImage.write scope)
        use assy = Assembly.read NullLoggerFactory.Instance None stream
        let ty = assy.TypeDefs.[MetadataTokens.TypeDefinitionHandle 2]

        match ty.Methods with
        | [ m ] ->
            MetadataTokens.GetRowNumber (MethodDefinitionHandle.op_Implicit m.TryMetadata.Value.Handle : EntityHandle)
            |> shouldEqual method.Rid

            match m.Body with
            | MethodBody.Il body ->
                body.LocalVars |> Option.map List.ofSeq |> shouldEqual (Some [])
                body.LocalsInit |> shouldEqual true
            | other -> failwith $"expected an IL body, got %A{other}"
        | methods -> failwith $"expected one method, got %d{methods.Length}"

    /// Strings `ModuleBuilder_GetStringConstant` asks for, chosen so that equal ones recur, that each
    /// length prefix's width is crossed, that one is a lone surrogate, and that one is the space the
    /// heap starts with.
    let private userStringValues : string list =
        [
            "a"
            ""
            " "
            "ab"
            "é"
            "\uD800"
            System.String ('x', 70)
            System.String ('y', 8200)
            "z"
        ]

    /// `RegMeta::DefineUserString` in a dynamic module: the string's UTF-16 bytes and a final byte of 1,
    /// found in the `#US` heap before it is added, after the `" "` that `RegMeta::GetSaveSize` gives an
    /// empty heap, whose final byte is 0. Real .NET's `ModuleBuilder.GetStringMetadataToken` reaches the
    /// same function, so it is the oracle.
    [<Test>]
    let ``a user string is found in the heap before it is added, as real .NET finds it`` () : unit =
        let property (choices : int list) : unit =
            let assembly =
                Emit.AssemblyBuilder.DefineDynamicAssembly (
                    AssemblyName "UserStringOracle",
                    Emit.AssemblyBuilderAccess.Run
                )

            let module' = assembly.DefineDynamicModule "UserStringOracle"
            let mutable scope = EmitScope.create "UserStringOracle" Guid.Empty

            for choice in choices do
                let value = userStringValues.[choice % userStringValues.Length]
                let offset, next = EmitScope.internUserString value scope

                0x70000000 ||| offset.Offset
                |> shouldEqual (module'.GetStringMetadataToken value)

                scope <- next

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 50,
            Prop.forAll (Arb.fromGen (Gen.listOf (Gen.choose (0, 100)) |> Gen.map List.ofSeq)) property
        )

    [<Test>]
    let ``every user string reads back from the image at its offset`` () : unit =
        let mutable scope = EmitScope.create "M" Guid.Empty

        let offsets =
            [
                for value in userStringValues do
                    let offset, next = EmitScope.internUserString value scope
                    scope <- next
                    yield value, offset
            ]

        let pe, reader = readImage (EmitScopeImage.write scope)
        use _ = pe

        // The space `RegMeta::GetSaveSize` gives an empty heap.
        reader.GetUserString (MetadataTokens.UserStringHandle 1) |> shouldEqual " "

        for value, offset in offsets do
            reader.GetUserString (MetadataTokens.UserStringHandle offset.Offset)
            |> shouldEqual value

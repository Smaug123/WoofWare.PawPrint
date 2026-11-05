namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test.EmitScopeGen

/// `EmitScopeExtension.violations`, judged by what the two scopes' images read back: the check must
/// pass exactly the replacements under which nothing the runtime has read from the earlier image
/// changes.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeExtension =

    let private created (scope : EmitScope) (choices : int list) : ImmutableHashSet<TypeDefRid> =
        let count = (EmitScope.typeDefs scope).Count

        choices
        |> Seq.map (fun c -> TypeDefRid (c % count + 1))
        |> ImmutableHashSet.CreateRange

    let private rowNumber (handle : EntityHandle) : int = MetadataTokens.GetRowNumber handle

    /// Everything a created type's `MethodTable` is built from, as handles into the image: its
    /// methods with their parameters, its fields, its interface implementations and its method impls.
    let private typeView (reader : MetadataReader) (typeDef : TypeDefRid) =
        let definition =
            reader.GetTypeDefinition (MetadataTokens.TypeDefinitionHandle typeDef.Rid)

        let methods =
            [
                for m in definition.GetMethods () do
                    let parameters =
                        [
                            for p in (reader.GetMethodDefinition m).GetParameters () do
                                yield rowNumber (ParameterHandle.op_Implicit p)
                        ]

                    yield rowNumber (MethodDefinitionHandle.op_Implicit m), parameters
            ]

        let fields =
            [
                for f in definition.GetFields () do
                    yield rowNumber (FieldDefinitionHandle.op_Implicit f)
            ]

        let interfaces =
            [
                for i in definition.GetInterfaceImplementations () do
                    let row = reader.GetInterfaceImplementation i
                    yield rowNumber (InterfaceImplementationHandle.op_Implicit i), token row.Interface
            ]

        let impls =
            [
                for i in definition.GetMethodImplementations () do
                    let row = reader.GetMethodImplementation i

                    yield
                        rowNumber (MethodImplementationHandle.op_Implicit i),
                        token row.MethodBody,
                        token row.MethodDeclaration
            ]

        methods, fields, interfaces, impls

    let private createdTypeNamed (violation : EmitScopeExtensionViolation) : TypeDefRid option =
        match violation with
        | EmitScopeExtensionViolation.AddedToCreatedType (_, _, createdType)
        | EmitScopeExtensionViolation.SortedRowMoved (_, _, createdType, _, _) -> Some createdType
        | EmitScopeExtensionViolation.RowChanged _
        | EmitScopeExtensionViolation.HeapEntryChanged _ -> None

    [<Test>]
    let ``a scope extends itself`` () : unit =
        let property (scope : EmitScope) (choices : int list) : unit =
            EmitScopeExtension.violations (created scope choices) scope scope
            |> shouldEqual []

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 100,
            Prop.forAll
                (Arb.fromGen (Gen.zip genScope (Gen.listOf choice |> Gen.map List.ofSeq)))
                (fun (scope, choices) -> property scope choices)
        )

    [<Test>]
    let ``appending to a scope is refused exactly for the created types it changes`` () : unit =
        let unchanged = ref 0
        let changed = ref 0
        let moved = ref 0
        let added = ref 0

        let property (mvid : Guid) (ops : Op list) (extra : Op list) (choices : int list) : unit =
            let before = apply mvid ops
            let after = apply mvid (ops @ extra)
            let created = created before choices
            let violations = EmitScopeExtension.violations created before after

            // `after` appends to `before`, so every row and heap entry of `before` is still there.
            violations |> List.filter (createdTypeNamed >> Option.isNone) |> shouldEqual []

            let beforePe, beforeReader = readImage (EmitScopeImage.write before)
            use _ = beforePe
            let afterPe, afterReader = readImage (EmitScopeImage.write after)
            use _ = afterPe

            let changedTypes =
                created
                |> Seq.filter (fun t -> typeView beforeReader t <> typeView afterReader t)
                |> Seq.map (fun t -> t.Rid)
                |> Set.ofSeq

            violations
            |> List.choose createdTypeNamed
            |> List.map (fun t -> t.Rid)
            |> Set.ofList
            |> shouldEqual changedTypes

            if changedTypes.IsEmpty then
                unchanged.Value <- unchanged.Value + 1
            else
                changed.Value <- changed.Value + 1

            for violation in violations do
                match violation with
                | EmitScopeExtensionViolation.SortedRowMoved _ -> moved.Value <- moved.Value + 1
                | EmitScopeExtensionViolation.AddedToCreatedType _ -> added.Value <- added.Value + 1
                | EmitScopeExtensionViolation.RowChanged _
                | EmitScopeExtensionViolation.HeapEntryChanged _ -> ()

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (
                    gen {
                        let! mvid = genGuid
                        let! ops = genOps
                        let! extra = genOps
                        // A few created types, so that many extensions change none of them.
                        let! choices = Gen.choose (0, 3) |> Gen.bind (fun n -> Gen.listOfLength n choice)
                        return mvid, ops, extra, List.ofSeq choices
                    }
                ))
                (fun (mvid, ops, extra, choices) -> property mvid ops extra choices)
        )

        // Both answers, and both reasons for refusing, must be common enough to be tested.
        unchanged.Value |> shouldBeGreaterThan 30
        changed.Value |> shouldBeGreaterThan 30
        moved.Value |> shouldBeGreaterThan 10
        added.Value |> shouldBeGreaterThan 30

    /// Every row, heap entry and identity column of `before`'s image, and whether `after`'s image reads
    /// each back the same; the sorted tables per class, each class's rows in image order.
    let private readsBackTheSame (before : EmitScope) (after : EmitScope) : bool =
        let beforePe, beforeReader = readImage (EmitScopeImage.write before)
        use _ = beforePe
        let afterPe, afterReader = readImage (EmitScopeImage.write after)
        use _ = afterPe

        let rows =
            let afterRows = describeRows afterReader

            describeRows beforeReader
            |> Map.forall (fun key row -> afterRows.TryFind key = Some row)

        let strings =
            EmitScope.strings before
            |> Seq.forall (fun value ->
                let offset = EmitScope.internString value before |> fst

                try
                    afterReader.GetString (MetadataTokens.StringHandle offset.Offset) = value
                with _ ->
                    false
            )

        let blobs =
            EmitScope.blobs before
            |> Seq.forall (fun value ->
                let offset = EmitScope.internBlob value before |> fst

                try
                    afterReader.GetBlobBytes (MetadataTokens.BlobHandle offset.Offset) |> Seq.toList = Seq.toList value
                with _ ->
                    false
            )

        let moduleRow (reader : MetadataReader) =
            let m = reader.GetModuleDefinition ()
            MetadataTokens.GetHeapOffset m.Name, reader.GetGuid m.Mvid

        let assemblyRow (reader : MetadataReader) =
            let a = reader.GetAssemblyDefinition ()

            a.Version,
            a.Flags,
            a.HashAlgorithm,
            MetadataTokens.GetHeapOffset a.PublicKey,
            MetadataTokens.GetHeapOffset a.Name,
            MetadataTokens.GetHeapOffset a.Culture

        let perClass (reader : MetadataReader) (t : int) =
            let definition = reader.GetTypeDefinition (MetadataTokens.TypeDefinitionHandle t)

            let interfaces =
                [
                    for i in definition.GetInterfaceImplementations () do
                        yield token (reader.GetInterfaceImplementation i).Interface
                ]

            let impls =
                [
                    for i in definition.GetMethodImplementations () do
                        let row = reader.GetMethodImplementation i
                        yield token row.MethodBody, token row.MethodDeclaration
                ]

            interfaces, impls

        let sorted =
            [ 1 .. (EmitScope.typeDefs before).Count ]
            |> List.forall (fun t ->
                let beforeInterfaces, beforeImpls = perClass beforeReader t
                let afterInterfaces, afterImpls = perClass afterReader t

                List.truncate beforeInterfaces.Length afterInterfaces = beforeInterfaces
                && List.truncate beforeImpls.Length afterImpls = beforeImpls
            )

        rows
        && strings
        && blobs
        && moduleRow beforeReader = moduleRow afterReader
        && assemblyRow beforeReader = assemblyRow afterReader
        && sorted

    [<Test>]
    let ``rewriting a scope's history is refused whenever its image would read back differently`` () : unit =
        let differed = ref 0

        let property (mvid : Guid) (ops : Op list) (index : int) (replacement : Op) (suffix : int) : unit =
            if not ops.IsEmpty then
                let i = index % ops.Length
                let before = apply mvid ops
                // The rewritten history, perhaps cut short.
                let rewritten =
                    List.updateAt i replacement ops |> List.truncate (i + 1 + suffix % ops.Length)

                let after = apply mvid rewritten

                let violations = EmitScopeExtension.violations ImmutableHashSet.Empty before after

                if not (readsBackTheSame before after) then
                    differed.Value <- differed.Value + 1
                    violations |> shouldNotEqual []

                // Nothing is created, so nothing can be added to or moved within a created type.
                violations |> List.choose createdTypeNamed |> shouldEqual []

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (
                    gen {
                        let! mvid = genGuid
                        let! ops = genOps
                        let! index = choice
                        let! replacement = genOp
                        let! suffix = choice
                        return mvid, ops, index, replacement, suffix
                    }
                ))
                (fun (mvid, ops, index, replacement, suffix) -> property mvid ops index replacement suffix)
        )

        differed.Value |> shouldBeGreaterThan 100

    [<Test>]
    let ``a changed row is reported at its RID`` () : unit =
        let scope = apply Guid.Empty []
        let first, scope = EmitScope.internString "First" scope
        let second, scope = EmitScope.internString "Second" scope

        let typeDef (name : StringOffset) : TypeDefRow =
            {
                Flags = Reflection.TypeAttributes.Public
                Name = name
                Namespace = StringOffset 0
                Extends = None
            }

        let before = EmitScope.addTypeDef (typeDef first) scope |> snd
        let after = EmitScope.addTypeDef (typeDef second) scope |> snd

        EmitScopeExtension.violations ImmutableHashSet.Empty before after
        |> shouldEqual [ EmitScopeExtensionViolation.RowChanged (TableIndex.TypeDef, 2) ]

        EmitScopeExtension.violations ImmutableHashSet.Empty before scope
        |> shouldEqual [ EmitScopeExtensionViolation.RowChanged (TableIndex.TypeDef, 2) ]

    [<Test>]
    let ``a changed method body is reported`` () : unit =
        let scope = apply Guid.Empty []
        let name, scope = EmitScope.internString "M" scope

        let signature, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0uy ; 0uy ; 1uy |]) scope

        let method (code : byte) : MethodDefRow =
            {
                Owner = TypeDefRid 1
                Body = Some (ImmutableArray.Create<byte> [| 0x06uy ; code |])
                ImplFlags = Reflection.MethodImplAttributes.IL
                Flags = Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static
                Name = name
                Signature = signature
            }

        let before = EmitScope.addMethodDef (method 0x2Auy) scope |> snd
        let same = EmitScope.addMethodDef (method 0x2Auy) scope |> snd
        let after = EmitScope.addMethodDef (method 0x00uy) scope |> snd

        EmitScopeExtension.violations ImmutableHashSet.Empty before same
        |> shouldEqual []

        EmitScopeExtension.violations ImmutableHashSet.Empty before after
        |> shouldEqual [ EmitScopeExtensionViolation.RowChanged (TableIndex.MethodDef, 1) ]

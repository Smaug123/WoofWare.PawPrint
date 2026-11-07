namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
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

    /// Everything a created type's `MethodTable` is built from, and what its methods run: its parent, its
    /// methods with their implementation flags, parameters and bodies, and its fields, as handles into
    /// the image, and its interface
    /// implementations and method impls in table order. Those two tables are sorted, so their RIDs
    /// move as rows of lower classes are added; the runtime keeps no handle into them, only what each
    /// row names.
    let private typeView (pe : PEReader) (reader : MetadataReader) (typeDef : TypeDefRid) =
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

                    // A body moves as bodies before it are filled in, so it is compared by content.
                    let body =
                        match (reader.GetMethodDefinition m).RelativeVirtualAddress with
                        | 0 -> None
                        | rva ->
                            let block = System.Reflection.Metadata.PEReaderExtensions.GetMethodBody (pe, rva)

                            Some (
                                List.ofArray (block.GetILBytes ()),
                                block.MaxStack,
                                block.LocalVariablesInitialized,
                                token (StandaloneSignatureHandle.op_Implicit block.LocalSignature)
                            )

                    yield
                        rowNumber (MethodDefinitionHandle.op_Implicit m),
                        (reader.GetMethodDefinition m).ImplAttributes,
                        parameters,
                        body
            ]

        let fields =
            [
                for f in definition.GetFields () do
                    yield rowNumber (FieldDefinitionHandle.op_Implicit f)
            ]

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

        token definition.BaseType, methods, fields, interfaces, impls

    let private createdTypeNamed (violation : EmitScopeExtensionViolation) : TypeDefRid option =
        match violation with
        | EmitScopeExtensionViolation.AddedToCreatedType (_, _, createdType)
        | EmitScopeExtensionViolation.FilledInCreatedType (_, _, createdType) -> Some createdType
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
        let added = ref 0
        let filled = ref 0

        let property (mvid : Guid) (ops : Op list) (extra : Op list) (choices : int list) : unit =
            let before = apply mvid ops
            let after = apply mvid (ops @ extra)
            let created = created before choices
            let violations = EmitScopeExtension.violations created before after

            // `after` appends to `before`, or fills in a column of one of its rows, so every row and
            // heap entry of `before` is still there.
            violations |> List.filter (createdTypeNamed >> Option.isNone) |> shouldEqual []

            let beforePe, beforeReader = readImage (EmitScopeImage.write before)
            use _ = beforePe
            let afterPe, afterReader = readImage (EmitScopeImage.write after)
            use _ = afterPe

            let changedTypes =
                created
                |> Seq.filter (fun t -> typeView beforePe beforeReader t <> typeView afterPe afterReader t)
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
                | EmitScopeExtensionViolation.AddedToCreatedType _ -> added.Value <- added.Value + 1
                | EmitScopeExtensionViolation.FilledInCreatedType _ -> filled.Value <- filled.Value + 1
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

        // Both answers, and every reason for refusing, must be common enough to be tested.
        unchanged.Value |> shouldBeGreaterThan 30
        changed.Value |> shouldBeGreaterThan 30
        added.Value |> shouldBeGreaterThan 30
        filled.Value |> shouldBeGreaterThan 10

    /// Rows appended for `<Module>`, the lowest TypeDef, sort before every other type's rows in
    /// `InterfaceImpl` and `MethodImpl`, so they move every created type's rows of those tables in the
    /// image while changing nothing about the type.
    [<Test>]
    let ``a created type's rows of a sorted table may move`` () : unit =
        let moved = ref 0

        let property (mvid : Guid) (ops : Op list) (choices : int list) : unit =
            let before = apply mvid ops
            let created = (created before choices).Remove (TypeDefRid 1)

            let after =
                (before, created)
                ||> Seq.fold (fun scope createdType ->
                    let scope =
                        EmitScope.addInterfaceImpl
                            {
                                Class = TypeDefRid 1
                                Interface = TypeDefOrRef.TypeDef createdType
                            }
                            scope

                    if (EmitScope.methodDefs scope).Count = 0 then
                        scope
                    else
                        EmitScope.addMethodImpl
                            {
                                Class = TypeDefRid 1
                                Body = MethodDefOrRef.MethodDef (MethodDefRid 1)
                                Declaration = MethodDefOrRef.MethodDef (MethodDefRid 1)
                            }
                            scope
                )

            EmitScopeExtension.violations created before after |> shouldEqual []

            let beforePe, beforeReader = readImage (EmitScopeImage.write before)
            use _ = beforePe
            let afterPe, afterReader = readImage (EmitScopeImage.write after)
            use _ = afterPe

            for t in created do
                typeView afterPe afterReader t |> shouldEqual (typeView beforePe beforeReader t)

            let movedIn
                (classes : TypeDefRid seq)
                (beforeRids : ImmutableArray<int>)
                (afterRids : ImmutableArray<int>)
                =
                Seq.zip3 classes beforeRids afterRids
                |> Seq.exists (fun (cls, b, a) -> created.Contains cls && b <> a)

            if
                movedIn
                    (EmitScope.interfaceImpls before |> Seq.map (fun row -> row.Class))
                    (EmitScopeImage.interfaceImplRids before)
                    (EmitScopeImage.interfaceImplRids after)
                || movedIn
                    (EmitScope.methodImpls before |> Seq.map (fun row -> row.Class))
                    (EmitScopeImage.methodImplRids before)
                    (EmitScopeImage.methodImplRids after)
            then
                moved.Value <- moved.Value + 1

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (
                    gen {
                        let! mvid = genGuid
                        let! ops = genOps
                        let! choices = Gen.choose (1, 3) |> Gen.bind (fun n -> Gen.listOfLength n choice)
                        return mvid, ops, List.ofSeq choices
                    }
                ))
                (fun (mvid, ops, choices) -> property mvid ops choices)
        )

        // Most samples must move a created type's rows, or the property barely tests the admission.
        moved.Value |> shouldBeGreaterThan 100

    /// What reading `after`'s image where `before`'s was read shows to have changed, each as a
    /// requirement on the violations reported: a category, for counting, and whether the violations
    /// meet it.
    let private differencesSeen
        (before : EmitScope)
        (after : EmitScope)
        : (string * (EmitScopeExtensionViolation list -> bool)) list
        =
        let beforePe, beforeReader = readImage (EmitScopeImage.write before)
        use _ = beforePe
        let afterPe, afterReader = readImage (EmitScopeImage.write after)
        use _ = afterPe

        let has (violation : EmitScopeExtensionViolation) (violations : EmitScopeExtensionViolation list) =
            List.contains violation violations

        // A heap entry reads back differently only if it, or an entry interned before it, changed.
        let heapEntryAtOrBefore (heap : HeapIndex) (entry : int) (violations : EmitScopeExtensionViolation list) =
            violations
            |> List.exists (fun v ->
                match v with
                | EmitScopeExtensionViolation.HeapEntryChanged (h, e) -> h = heap && e <= entry
                | _ -> false
            )

        let reads (f : unit -> 'a) : 'a option =
            try
                Some (f ())
            with _ ->
                None

        [
            // A row reads back differently only if the scope's row at that RID changed. Nothing is
            // created, so a column the emitter fills in may differ.
            let afterRows = withoutFilledColumns (describeRows afterReader)

            for KeyValue ((table, rid), row) in withoutFilledColumns (describeRows beforeReader) do
                if afterRows.TryFind (table, rid) <> Some row then
                    yield "row", has (EmitScopeExtensionViolation.RowChanged (table, rid))

            for entry, value in Seq.indexed (EmitScope.strings before) do
                let offset = EmitScope.internString value before |> fst

                if
                    reads (fun () -> afterReader.GetString (MetadataTokens.StringHandle offset.Offset))
                    <> Some value
                then
                    yield "string", heapEntryAtOrBefore HeapIndex.String (entry + 1)

            for entry, value in Seq.indexed (EmitScope.blobs before) do
                let offset = EmitScope.internBlob value before |> fst

                let read =
                    reads (fun () ->
                        afterReader.GetBlobBytes (MetadataTokens.BlobHandle offset.Offset)
                        |> List.ofArray
                    )

                if read <> Some (List.ofSeq value) then
                    yield "blob", heapEntryAtOrBefore HeapIndex.Blob (entry + 1)

            for entry, value in Seq.indexed (EmitScope.guids before) do
                if
                    reads (fun () -> afterReader.GetGuid (MetadataTokens.GuidHandle (entry + 1)))
                    <> Some value
                then
                    yield "guid", has (EmitScopeExtensionViolation.HeapEntryChanged (HeapIndex.Guid, entry + 1))

            let assemblyRow (reader : MetadataReader) =
                let a = reader.GetAssemblyDefinition ()

                a.Version,
                a.Flags,
                a.HashAlgorithm,
                MetadataTokens.GetHeapOffset a.PublicKey,
                MetadataTokens.GetHeapOffset a.Name,
                MetadataTokens.GetHeapOffset a.Culture

            if assemblyRow beforeReader <> assemblyRow afterReader then
                yield "assembly", has (EmitScopeExtensionViolation.RowChanged (TableIndex.Assembly, 1))

            // A class's rows in a sorted table, in image order, gain rows only at the end, unless the
            // scope's rows of that table changed.
            let rowOfTable (table : TableIndex) (violations : EmitScopeExtensionViolation list) =
                violations
                |> List.exists (fun v ->
                    match v with
                    | EmitScopeExtensionViolation.RowChanged (t, _) -> t = table
                    | _ -> false
                )

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

            for t in 1 .. min (EmitScope.typeDefs before).Count (EmitScope.typeDefs after).Count do
                let beforeInterfaces, beforeImpls = perClass beforeReader t
                let afterInterfaces, afterImpls = perClass afterReader t

                if List.truncate beforeInterfaces.Length afterInterfaces <> beforeInterfaces then
                    yield "sorted", rowOfTable TableIndex.InterfaceImpl

                if List.truncate beforeImpls.Length afterImpls <> beforeImpls then
                    yield "sorted", rowOfTable TableIndex.MethodImpl
        ]

    [<Test>]
    let ``rewriting a scope's history is refused for everything its image would read back differently`` () : unit =
        let seen = Collections.Generic.Dictionary<string, int> ()

        let property
            (mvid : Guid)
            (otherMvid : Guid option)
            (ops : Op list)
            (index : int)
            (replacement : Op)
            (suffix : int)
            : unit
            =
            if not ops.IsEmpty then
                let i = index % ops.Length
                let before = apply mvid ops
                // The rewritten history, perhaps cut short, and perhaps of another module.
                let rewritten =
                    List.updateAt i replacement ops |> List.truncate (i + 1 + suffix % ops.Length)

                let after = apply (Option.defaultValue mvid otherMvid) rewritten

                let violations = EmitScopeExtension.violations ImmutableHashSet.Empty before after

                for category, met in differencesSeen before after do
                    seen.[category] <-
                        (match seen.TryGetValue category with
                         | true, n -> n
                         | false, _ -> 0)
                        + 1

                    if not (met violations) then
                        failwith $"a %s{category} difference is not among the violations %A{violations}"

                // Nothing is created, so nothing can be added to or filled in a created type.
                violations |> List.choose createdTypeNamed |> shouldEqual []

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (
                    gen {
                        let! mvid = genGuid
                        let! otherMvid = Gen.frequency [ 3, Gen.constant None ; 1, Gen.map Some genGuid ]
                        let! ops = genOps
                        let! index = choice
                        let! replacement = genOp
                        let! suffix = choice
                        return mvid, otherMvid, ops, index, replacement, suffix
                    }
                ))
                (fun (mvid, otherMvid, ops, index, replacement, suffix) ->
                    property mvid otherMvid ops index replacement suffix
                )
        )

        for category in [ "row" ; "string" ; "blob" ; "guid" ; "sorted" ] do
            let count =
                match seen.TryGetValue category with
                | true, n -> n
                | false, _ -> 0

            if count < 20 then
                failwith $"only %d{count} %s{category} differences were generated; the property barely tests them"

    /// The one way two modules' scopes differ in the `Module` row alone: the empty name is at offset 0,
    /// and `<Module>`, which every scope interns next, is then at the offset the other name has.
    [<Test>]
    let ``a scope of another module is refused`` () : unit =
        let before = EmitScope.create "" Guid.Empty
        let after = EmitScope.create "<Module>" Guid.Empty

        EmitScope.strings after
        |> List.ofSeq
        |> shouldEqual (List.ofSeq (EmitScope.strings before))

        EmitScopeExtension.violations ImmutableHashSet.Empty before after
        |> shouldEqual [ EmitScopeExtensionViolation.RowChanged (TableIndex.Module, 1) ]

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
    let ``a changed method body is reported only for a created type`` () : unit =
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

        // `SetMethodIL` gives a method its body again when `CreateType` is retried, so a different body
        // is a fill, which only a created type refuses.
        EmitScopeExtension.violations ImmutableHashSet.Empty before after
        |> shouldEqual []

        EmitScopeExtension.violations (ImmutableHashSet.Create (TypeDefRid 1)) before after
        |> shouldEqual
            [
                EmitScopeExtensionViolation.FilledInCreatedType (TableIndex.MethodDef, 1, TypeDefRid 1)
            ]

    [<Test>]
    let ``only a method's implementation flags may be filled in, and only while its type is not created`` () : unit =
        let scope = apply Guid.Empty []
        let name, scope = EmitScope.internString "M" scope

        let signature, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0uy ; 0uy ; 1uy |]) scope

        let method (flags : Reflection.MethodAttributes) : MethodDefRow =
            {
                Owner = TypeDefRid 1
                Body = None
                ImplFlags = Reflection.MethodImplAttributes.IL
                Flags = flags
                Name = name
                Signature = signature
            }

        let publicStatic =
            Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static

        let privateStatic =
            Reflection.MethodAttributes.Private ||| Reflection.MethodAttributes.Static

        let before = EmitScope.addMethodDef (method publicStatic) scope |> snd

        let fill (scope : EmitScope) =
            EmitScope.setMethodImplFlags (MethodDefRid 1) Reflection.MethodImplAttributes.NoInlining scope

        let filled = fill before
        let reflagged = EmitScope.addMethodDef (method privateStatic) scope |> snd
        let both = fill reflagged
        let created = ImmutableHashSet.Create (TypeDefRid 1)

        EmitScopeExtension.violations ImmutableHashSet.Empty before filled
        |> shouldEqual []

        EmitScopeExtension.violations created before filled
        |> shouldEqual
            [
                EmitScopeExtensionViolation.FilledInCreatedType (TableIndex.MethodDef, 1, TypeDefRid 1)
            ]

        // Any other column is fixed once the row is added, filled in alongside or not.
        for after in [ reflagged ; both ] do
            for createdTypes in [ ImmutableHashSet.Empty ; created ] do
                EmitScopeExtension.violations createdTypes before after
                |> shouldEqual [ EmitScopeExtensionViolation.RowChanged (TableIndex.MethodDef, 1) ]

    [<Test>]
    let ``only a type's parent may be filled in, and only while it is not created`` () : unit =
        let scope = apply Guid.Empty []
        let first, scope = EmitScope.internString "First" scope
        let second, scope = EmitScope.internString "Second" scope

        let typeDef (name : StringOffset) (flags : Reflection.TypeAttributes) : TypeDefRow =
            {
                Flags = flags
                Name = name
                Namespace = StringOffset 0
                Extends = None
            }

        let firstType, scope =
            EmitScope.addTypeDef (typeDef first Reflection.TypeAttributes.Public) scope

        let before, secondType =
            let secondType, scope =
                EmitScope.addTypeDef (typeDef second Reflection.TypeAttributes.Public) scope

            scope, secondType

        let filled =
            EmitScope.setTypeDefExtends secondType (TypeDefOrRef.TypeDef firstType) before

        let reflagged =
            EmitScope.addTypeDef (typeDef second Reflection.TypeAttributes.Sealed) scope
            |> snd

        let both =
            EmitScope.setTypeDefExtends secondType (TypeDefOrRef.TypeDef firstType) reflagged

        let created = ImmutableHashSet.Create secondType

        EmitScopeExtension.violations ImmutableHashSet.Empty before filled
        |> shouldEqual []

        // Another type's being created does not fix this one's parent.
        EmitScopeExtension.violations (ImmutableHashSet.Create firstType) before filled
        |> shouldEqual []

        EmitScopeExtension.violations created before filled
        |> shouldEqual
            [
                EmitScopeExtensionViolation.FilledInCreatedType (TableIndex.TypeDef, secondType.Rid, secondType)
            ]

        // Any other column is fixed once the row is added, filled in alongside or not.
        for after in [ reflagged ; both ] do
            for createdTypes in [ ImmutableHashSet.Empty ; created ] do
                EmitScopeExtension.violations createdTypes before after
                |> shouldEqual [ EmitScopeExtensionViolation.RowChanged (TableIndex.TypeDef, secondType.Rid) ]

    [<Test>]
    let ``a method's body may be filled in, and only while its type is not created`` () : unit =
        let scope = apply Guid.Empty []
        let name, scope = EmitScope.internString "M" scope

        let signature, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0uy ; 0uy ; 1uy |]) scope

        let before =
            EmitScope.addMethodDef
                {
                    Owner = TypeDefRid 1
                    Body = None
                    ImplFlags = Reflection.MethodImplAttributes.IL
                    Flags = Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static
                    Name = name
                    Signature = signature
                }
                scope
            |> snd

        let body (code : byte) =
            ImmutableArray.Create<byte> [| 0x06uy ; code |]

        let filled = EmitScope.setMethodBody (MethodDefRid 1) (body 0x2Auy) before
        let created = ImmutableHashSet.Create (TypeDefRid 1)

        EmitScopeExtension.violations ImmutableHashSet.Empty before filled
        |> shouldEqual []

        EmitScopeExtension.violations created before filled
        |> shouldEqual
            [
                EmitScopeExtensionViolation.FilledInCreatedType (TableIndex.MethodDef, 1, TypeDefRid 1)
            ]

        // A `CreateType` retried after failing partway gives a method its body again.
        let refilled = EmitScope.setMethodBody (MethodDefRid 1) (body 0x00uy) filled

        EmitScopeExtension.violations ImmutableHashSet.Empty filled refilled
        |> shouldEqual []

        EmitScopeExtension.violations created filled refilled
        |> shouldEqual
            [
                EmitScopeExtensionViolation.FilledInCreatedType (TableIndex.MethodDef, 1, TypeDefRid 1)
            ]

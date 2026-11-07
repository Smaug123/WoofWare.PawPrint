namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The methods CoreCLR's emitter defines in a dynamic module for `TypeBuilder_DefineMethod`, and the
/// implementation flags `TypeBuilder_SetMethodImpl` fills in afterwards.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeMethods =

    /// Names the emitter marks special and names it does not: it compares the UTF-16 name, ordinally,
    /// with `.ctor` and `.cctor`, and its first eight characters with `_VtblGap`. Two differ only in a
    /// lone surrogate, which the emitter stores as the same UTF-8.
    let private methodNames : string list =
        [
            "M"
            ".ctor"
            ".cctor"
            ".CTOR"
            ".ctor\uD800"
            "_VtblGap"
            "_VtblGap1_2"
            "_VtblGap\uD800"
            "_VtblGa"
            "_vtblGap"
            "M\uD800"
            "M\uDBFF"
        ]

    /// Signatures, some of them alike, so that two methods of one type share a name and signature.
    let private signatures : ImmutableArray<byte> list =
        [
            ImmutableArray.Create<byte> [| 0x20uy ; 0x00uy ; 0x01uy |]
            ImmutableArray.Create<byte> [| 0x00uy ; 0x00uy ; 0x01uy |]
            ImmutableArray.Create<byte> [| 0x20uy ; 0x01uy ; 0x08uy ; 0x0Euy |]
        ]

    /// `mdReservedMask`, which `DefineMethod` clears from the flags it is given.
    let private reserved : int = 0xd000

    /// The string a `#Strings` entry holds for `value`: its UTF-8, with each lone surrogate replaced.
    let private stored (value : string) : string =
        Encoding.UTF8.GetString (Encoding.UTF8.GetBytes value)

    /// A scope with `<Module>` and two more types, for methods to belong to.
    let private withTypes () : EmitScope =
        let define (fullName : string) (scope : EmitScope) : EmitScope =
            match EmitScopeDefinitions.defineTopLevelTypeDef fullName TypeAttributes.Public None [] scope with
            | TypeDefDefinition.Defined (_, scope) -> scope
            | TypeDefDefinition.Duplicate _ -> failwith $"%s{fullName} is already in the scope"

        EmitScope.create "Dyn" Guid.Empty |> define "A" |> define "N.B"

    /// A method as the model holds it.
    type private ModelMethod =
        {
            Owner : TypeDefRid
            Name : string
            Signature : ImmutableArray<byte>
            Flags : MethodAttributes
            ImplFlags : MethodImplAttributes
        }

    type private Op =
        | Define of owner : int * name : int * signature : int * flags : int
        | SetImpl of method : int * flags : int

    let private genFlags : Gen<int> =
        Gen.zip (Gen.choose (0, 0xFFFF)) (Gen.choose (0, 0xFFFF))
        |> Gen.map (fun (high, low) -> (high <<< 16) ||| low)

    let private genOp : Gen<Op> =
        Gen.frequency
            [
                3,
                gen {
                    let! owner = Gen.choose (0, 1000)
                    let! name = Gen.choose (0, methodNames.Length - 1)
                    let! signature = Gen.choose (0, signatures.Length - 1)
                    let! flags = genFlags
                    return Op.Define (owner, name, signature, flags)
                }
                1, Gen.map2 (fun m f -> Op.SetImpl (m, f)) (Gen.choose (0, 1000)) genFlags
            ]

    [<Test>]
    let ``every method is a new row with the flags the emitter keeps, and its implementation flags are the last set``
        ()
        : unit
        =
        let mutable special = 0

        let property (ops : Op list) : unit =
            let start = withTypes ()
            let typeCount = (EmitScope.typeDefs start).Count
            let mutable model : ModelMethod list = []
            let mutable scope = start

            for op in ops do
                match op with
                | Op.Define (owner, nameIndex, signatureIndex, flags) ->
                    let owner = TypeDefRid (owner % typeCount + 1)
                    let name = methodNames.[nameIndex]
                    let signature = signatures.[signatureIndex]

                    let isSpecial =
                        name = ".ctor"
                        || name = ".cctor"
                        || name.StartsWith ("_VtblGap", StringComparison.Ordinal)

                    if isSpecial then
                        special <- special + 1

                    let expectedFlags =
                        (flags &&& ~~~reserved &&& 0xFFFF)
                        ||| (if isSpecial then
                                 int (MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName)
                             else
                                 0)

                    let rid, next =
                        EmitScopeDefinitions.defineMethod owner name signature (enum<MethodAttributes> flags) scope

                    model <-
                        model
                        @ [
                            {
                                Owner = owner
                                Name = name
                                Signature = signature
                                Flags = enum<MethodAttributes> expectedFlags
                                ImplFlags = MethodImplAttributes.IL
                            }
                        ]

                    rid |> shouldEqual (MethodDefRid model.Length)
                    scope <- next
                | Op.SetImpl (methodChoice, flags) ->
                    if not model.IsEmpty then
                        let index = methodChoice % model.Length

                        scope <-
                            EmitScopeDefinitions.setMethodImplFlags
                                (MethodDefRid (index + 1))
                                (enum<MethodImplAttributes> flags)
                                scope

                        model <-
                            model
                            |> List.updateAt
                                index
                                { model.[index] with
                                    ImplFlags = enum<MethodImplAttributes> (flags &&& 0xFFFF)
                                }

            EmitScope.methodDefs scope
            |> Seq.map (fun row ->
                row.Body |> shouldEqual None

                {
                    Owner = row.Owner
                    Name = EmitScope.stringAt row.Name scope
                    Signature = EmitScope.blobAt row.Signature scope
                    Flags = row.Flags
                    ImplFlags = row.ImplFlags
                }
            )
            |> List.ofSeq
            |> List.map (fun m -> m.Owner, m.Name, List.ofSeq m.Signature, m.Flags, m.ImplFlags)
            |> shouldEqual (
                model
                |> List.map (fun m -> m.Owner, stored m.Name, List.ofSeq m.Signature, m.Flags, m.ImplFlags)
            )

            // Nothing but the MethodDef table and the heaps changes.
            EmitScope.typeDefs scope |> shouldEqual (EmitScope.typeDefs start)
            EmitScope.typeRefs scope |> shouldEqual (EmitScope.typeRefs start)
            EmitScope.parameters scope |> Seq.isEmpty |> shouldEqual true

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.listOf genOp |> Gen.map List.ofSeq)) property
        )

        special |> shouldBeGreaterThan 100

    let private thrown (f : unit -> 'a) : string =
        match
            (try
                f () |> ignore
                None
             with :? ArgumentException as e ->
                 Some e.Message)
        with
        | None -> failwith "expected an ArgumentException"
        | Some message -> message

    [<Test>]
    let ``a method row's flags must fit their two-byte columns`` () : unit =
        let scope = withTypes ()
        let name, scope = EmitScope.internString "M" scope
        let signature, scope = EmitScope.internBlob signatures.[0] scope

        let row : MethodDefRow =
            {
                Owner = TypeDefRid 2
                Body = None
                ImplFlags = MethodImplAttributes.IL
                Flags = MethodAttributes.Public
                Name = name
                Signature = signature
            }

        thrown (fun () ->
            EmitScope.addMethodDef
                { row with
                    Flags = enum<MethodAttributes> 0x10006
                }
                scope
        )
        |> shouldContainText "0x10006"

        thrown (fun () ->
            EmitScope.addMethodDef
                { row with
                    ImplFlags = enum<MethodImplAttributes> 0x10000
                }
                scope
        )
        |> shouldContainText "0x10000"

        let method, scope = EmitScope.addMethodDef row scope

        thrown (fun () -> EmitScope.setMethodImplFlags method (enum<MethodImplAttributes> 0x10008) scope)
        |> shouldContainText "0x10008"

        thrown (fun () -> EmitScope.setMethodImplFlags (MethodDefRid 2) MethodImplAttributes.NoInlining scope)
        |> shouldContainText "MethodDef RID 2"

        let filled =
            EmitScope.setMethodImplFlags method MethodImplAttributes.NoInlining scope

        (EmitScope.methodDefs filled).[0]
        |> shouldEqual
            { row with
                ImplFlags = MethodImplAttributes.NoInlining
            }

        // The scope it was given is unchanged.
        (EmitScope.methodDefs scope).[0] |> shouldEqual row

    /// `TypeBuilder_SetMethodIL` gives a method its body when its type is created, and gives it again,
    /// overwriting the first, when `CreateType` is retried after failing partway.
    [<Test>]
    let ``a method's body is set, and set again over the first`` () : unit =
        let scope = withTypes ()
        let name, scope = EmitScope.internString "M" scope
        let signature, scope = EmitScope.internBlob signatures.[0] scope

        let row : MethodDefRow =
            {
                Owner = TypeDefRid 2
                Body = None
                ImplFlags = MethodImplAttributes.IL
                Flags = MethodAttributes.Public ||| MethodAttributes.Static
                Name = name
                Signature = signature
            }

        let method, scope = EmitScope.addMethodDef row scope
        let body = ImmutableArray.Create<byte> [| 0x06uy ; 0x2Auy |]
        let filled = EmitScope.setMethodBody method body scope

        (EmitScope.methodDefs filled).[0]
        |> shouldEqual
            { row with
                Body = Some body
            }

        (EmitScope.methodDefs scope).[0] |> shouldEqual row

        let other = ImmutableArray.Create<byte> [| 0x0Auy ; 0x16uy ; 0x2Auy |]

        (EmitScope.methodDefs (EmitScope.setMethodBody method other filled)).[0]
        |> shouldEqual
            { row with
                Body = Some other
            }

        thrown (fun () -> EmitScope.setMethodBody (MethodDefRid 2) body scope)
        |> shouldContainText "MethodDef RID 2"

    /// The body `TypeBuilder_SetMethodIL` writes, read back from the module's image by SRM's reader:
    /// the code, max stack, local signature, init-locals flag and exception clauses it was given. The
    /// header is tiny exactly when `COR_ILMETHOD::Size` picks one (no flags, no local signature, max
    /// stack at most 8, fewer than 64 bytes of code and no clauses), which the reader reports as a max
    /// stack of 8 with no locals. The clauses' section is small exactly when `ExceptionHandlingSize`
    /// picks one: at most 20 clauses, each with offsets that fit 16 bits and lengths that fit 8.
    [<Test>]
    let ``a method body reads back as SetMethodIL wrote it, in CoreCLR's formats`` () : unit =
        let tiny = ref 0
        let fat = ref 0
        let smallSections = ref 0
        let fatSections = ref 0

        // With `fits`, a clause a small section can hold; otherwise either side of each of its bounds,
        // often.
        let genClause (fits : bool) : Gen<ExceptionClause> =
            let offsets =
                if fits then
                    Gen.elements [ 0 ; 1 ; 0xFF ; 0xFFFF ]
                else
                    Gen.frequency [ 3, Gen.choose (0, 300) ; 1, Gen.elements [ 0xFFFF ; 0x10000 ] ]

            let lengths =
                if fits then
                    Gen.elements [ 0 ; 1 ; 0xFF ]
                else
                    Gen.frequency [ 3, Gen.choose (0, 300) ; 1, Gen.elements [ 0xFF ; 0x100 ] ]

            gen {
                let! offset = offsets
                let! length = lengths
                let! handlerOffset = offsets
                let! handlerLength = lengths
                let! token = Gen.choose (1, 5)

                let! handler =
                    Gen.elements
                        [
                            ExceptionClauseHandler.Catch (0x01000000 ||| token)
                            ExceptionClauseHandler.Filter (token * 3)
                            ExceptionClauseHandler.Finally
                            ExceptionClauseHandler.Fault 0
                        ]

                return
                    {
                        Handler = handler
                        TryOffset = offset
                        TryLength = length
                        HandlerOffset = handlerOffset
                        HandlerLength = handlerLength
                    }
            }

        let property
            (initLocals : bool)
            (maxStack : int)
            (codeLength : int)
            (localSig : int option)
            (clauses : ExceptionClause list)
            : unit
            =
            let maxStack = uint16 maxStack
            // `ret`s, so that the body is valid IL whatever its length.
            let code = ImmutableArray.CreateRange (Seq.replicate codeLength 0x2Auy)
            let localSignature = localSig |> Option.map (fun s -> StandAloneSigRid (s % 3 + 1))

            let body =
                EmitScopeDefinitions.methodBody initLocals maxStack code localSignature clauses

            let isTiny =
                not initLocals
                && localSignature.IsNone
                && maxStack <= 8us
                && codeLength < 64
                && clauses.IsEmpty

            let isSmallSection =
                clauses.Length <= 20
                && clauses
                   |> List.forall (fun c ->
                       c.TryOffset <= 0xFFFF
                       && c.TryLength <= 0xFF
                       && c.HandlerOffset <= 0xFFFF
                       && c.HandlerLength <= 0xFF
                   )

            if isTiny then
                tiny.Value <- tiny.Value + 1
            else
                fat.Value <- fat.Value + 1

            if not clauses.IsEmpty then
                if isSmallSection then
                    smallSections.Value <- smallSections.Value + 1
                else
                    fatSections.Value <- fatSections.Value + 1

            let scope =
                EmitScopeGen.apply
                    Guid.Empty
                    [
                        EmitScopeGen.Op.TypeDef ("T", "", 0, None)
                        EmitScopeGen.Op.StandAloneSig [ 0 ]
                        EmitScopeGen.Op.StandAloneSig [ 1 ]
                        EmitScopeGen.Op.StandAloneSig [ 2 ]
                    ]

            let name, scope = EmitScope.internString "M" scope

            let signature, scope =
                EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x00uy ; 0x00uy ; 0x01uy |]) scope

            let method, scope =
                EmitScope.addMethodDef
                    {
                        Owner = TypeDefRid 2
                        Body = Some body
                        ImplFlags = MethodImplAttributes.IL
                        Flags = MethodAttributes.Public ||| MethodAttributes.Static
                        Name = name
                        Signature = signature
                    }
                    scope

            let pe, reader = EmitScopeGen.readImage (EmitScopeImage.write scope)
            use _ = pe

            let rva =
                (reader.GetMethodDefinition (Metadata.Ecma335.MetadataTokens.MethodDefinitionHandle method.Rid))
                    .RelativeVirtualAddress

            let block = Metadata.PEReaderExtensions.GetMethodBody (pe, rva)

            block.GetILBytes () |> List.ofArray |> shouldEqual (List.ofSeq code)
            block.Size |> shouldEqual body.Length

            let alignedCode = (codeLength + 3) &&& ~~~3

            let sectionLength =
                match clauses with
                | [] -> 0
                | _ when isSmallSection -> 4 + 12 * clauses.Length
                | _ -> 4 + 24 * clauses.Length

            if isTiny then
                body.Length |> shouldEqual (1 + codeLength)
                block.MaxStack |> shouldEqual 8
                block.LocalVariablesInitialized |> shouldEqual false
                block.LocalSignature.IsNil |> shouldEqual true
            else
                body.Length
                |> shouldEqual (
                    if clauses.IsEmpty then
                        12 + codeLength
                    else
                        12 + alignedCode + sectionLength
                )

                block.MaxStack |> shouldEqual (int maxStack)
                block.LocalVariablesInitialized |> shouldEqual initLocals

                match localSignature with
                | None -> block.LocalSignature.IsNil |> shouldEqual true
                | Some rid ->
                    Metadata.Ecma335.MetadataTokens.GetRowNumber (
                        Metadata.StandaloneSignatureHandle.op_Implicit block.LocalSignature
                    )
                    |> shouldEqual rid.Rid

            if not clauses.IsEmpty then
                // `CorILMethod_Sect_EHTable`, with `CorILMethod_Sect_FatFormat` for a fat section.
                body.[12 + alignedCode]
                |> shouldEqual (if isSmallSection then 0x01uy else 0x41uy)

            block.ExceptionRegions
            |> Seq.map (fun r ->
                let classOrFilter =
                    match r.Kind with
                    | Metadata.ExceptionRegionKind.Filter -> r.FilterOffset
                    | Metadata.ExceptionRegionKind.Catch -> Metadata.Ecma335.MetadataTokens.GetToken r.CatchType
                    | _ -> 0

                int r.Kind, r.TryOffset, r.TryLength, r.HandlerOffset, r.HandlerLength, classOrFilter
            )
            |> List.ofSeq
            |> shouldEqual (
                clauses
                |> List.map (fun c ->
                    let kind, classOrFilter =
                        match c.Handler with
                        | ExceptionClauseHandler.Catch token -> 0, token
                        | ExceptionClauseHandler.Filter offset -> 1, offset
                        | ExceptionClauseHandler.Finally -> 2, 0
                        | ExceptionClauseHandler.Fault _ -> 4, 0

                    kind, c.TryOffset, c.TryLength, c.HandlerOffset, c.HandlerLength, classOrFilter
                )
            )

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll
                (Arb.fromGen (
                    gen {
                        let! initLocals = Gen.elements [ true ; false ]
                        // Each bound of the tiny header, and either side of it, often.
                        let! maxStack =
                            Gen.frequency
                                [ 3, Gen.choose (0, 10) ; 2, Gen.elements [ 8 ; 9 ] ; 1, Gen.choose (0, 65535) ]

                        let! codeLength =
                            Gen.frequency
                                [
                                    3, Gen.choose (0, 70)
                                    2, Gen.elements [ 0 ; 63 ; 64 ]
                                    1, Gen.choose (0, 300)
                                ]

                        let! localSig = Gen.frequency [ 1, Gen.constant None ; 1, Gen.map Some (Gen.choose (0, 2)) ]

                        // No clauses for half the bodies, so that the header's bounds are tested; and
                        // either side of the 20 clauses a small section holds.
                        let! clauseCount =
                            Gen.frequency [ 4, Gen.constant 0 ; 3, Gen.choose (1, 4) ; 1, Gen.elements [ 20 ; 21 ] ]

                        let! fits = Gen.elements [ true ; false ]
                        let! clauses = Gen.listOfLength clauseCount (genClause fits)
                        return initLocals, maxStack, codeLength, localSig, List.ofSeq clauses
                    }
                ))
                (fun (initLocals, maxStack, codeLength, localSig, clauses) ->
                    property initLocals maxStack codeLength localSig clauses
                )
        )

        // About a twentieth of the samples are tiny, and of those with clauses, some of each format.
        tiny.Value |> shouldBeGreaterThan 20
        fat.Value |> shouldBeGreaterThan 500
        smallSections.Value |> shouldBeGreaterThan 50
        fatSections.Value |> shouldBeGreaterThan 50

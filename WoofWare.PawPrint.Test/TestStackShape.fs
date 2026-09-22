namespace WoofWare.PawPrint.Test

#nowarn "9" // `fixed`, to hand a byte array to a BlobReader

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the stack-shape analysis: the evaluation stack's shape at the entry of every
/// instruction, joined over every path that reaches it, and the offsets at which a float32 must
/// be widened because CoreCLR's importer types the slot as double over every incoming path.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStackShape =

    /// A body from a list of instructions laid out consecutively from offset 0. Branch operands
    /// in the list are *absolute target offsets*, translated here into the relative deltas the
    /// opcodes carry, so a test can name its targets by the offset of the instruction they land on.
    let private layOut
        (regions : ExceptionRegion list)
        (ops : IlOp list)
        : MethodInstructions<TypeDefn> * Map<int, int>
        =
        let offsets =
            ops
            |> List.scan (fun offset op -> offset + IlOp.NumberOfBytes op) 0
            |> List.take ops.Length

        let relative (offset : int) (op : IlOp) (target : int) : int =
            target - (offset + IlOp.NumberOfBytes op)

        let laidOut =
            List.zip ops offsets
            |> List.map (fun (op, offset) ->
                let op =
                    match op with
                    | IlOp.UnaryConst (UnaryConstIlOp.Br target) ->
                        IlOp.UnaryConst (UnaryConstIlOp.Br (relative offset op target))
                    | IlOp.UnaryConst (UnaryConstIlOp.Brtrue target) ->
                        IlOp.UnaryConst (UnaryConstIlOp.Brtrue (relative offset op target))
                    | IlOp.UnaryConst (UnaryConstIlOp.Brfalse target) ->
                        IlOp.UnaryConst (UnaryConstIlOp.Brfalse (relative offset op target))
                    | IlOp.UnaryConst (UnaryConstIlOp.Leave target) ->
                        IlOp.UnaryConst (UnaryConstIlOp.Leave (relative offset op target))
                    | IlOp.Switch targets ->
                        IlOp.Switch (targets |> Seq.map (relative offset op) |> ImmutableArray.CreateRange)
                    | other -> other

                op, offset
            )

        let instructions =
            {
                Instructions = laidOut
                Locations = laidOut |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
                LocalsInit = true
                LocalVars = None
                ExceptionRegions = ImmutableArray.CreateRange regions
            }

        // Index in the list -> offset, for tests that want to name an instruction by position.
        instructions, offsets |> List.indexed |> Map.ofList

    let private single : SlotShape = SlotShape.Float FloatWidth.Single
    let private double : SlotShape = SlotShape.Float FloatWidth.Double
    let private other : SlotShape = SlotShape.Other

    let private inputs (arguments : SlotShape list) (locals : SlotShape list) (returnsValue : bool) : StackShapeInputs =
        {
            Arguments = ImmutableArray.CreateRange arguments
            Locals = ImmutableArray.CreateRange locals
            ReturnsValue = returnsValue
            Tokens = Map.empty
        }

    /// Read CoreLib, whose metadata every token-reader test judges types against.
    let private corelib () : DumpedAssembly =
        Assembly.readFile (LoggerFactory.makeTest () |> snd) typeof<obj>.Assembly.Location

    /// Analyse, and fail if any reachable instruction could not be typed.
    let private analyseOrFail (inputs : StackShapeInputs) (body : MethodInstructions<TypeDefn>) : StackShape =
        let shape = StackShape.analyse inputs body

        if not shape.Invalid.IsEmpty then
            failwith $"expected every instruction to be typed, but got %O{shape.Invalid}"

        shape

    let private ldc : IlOp = IlOp.Nullary NullaryIlOp.LdcI4_1
    let private ldcI4 : IlOp = ldc
    let private ldcR4 : IlOp = IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 1.0f)
    let private ldcR8 : IlOp = IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 1.0)
    let private add : IlOp = IlOp.Nullary NullaryIlOp.Add
    let private dup : IlOp = IlOp.Nullary NullaryIlOp.Dup
    let private nop : IlOp = IlOp.Nullary NullaryIlOp.Nop
    let private ret : IlOp = IlOp.Nullary NullaryIlOp.Ret
    let private pop : IlOp = IlOp.Nullary NullaryIlOp.Pop
    let private ldarg0 : IlOp = IlOp.Nullary NullaryIlOp.LdArg0
    let private ldloc0 : IlOp = IlOp.Nullary NullaryIlOp.Ldloc_0

    let private br (target : int) : IlOp =
        IlOp.UnaryConst (UnaryConstIlOp.Br target)

    let private brtrue (target : int) : IlOp =
        IlOp.UnaryConst (UnaryConstIlOp.Brtrue target)

    let private callToken : MetadataOperand =
        MetadataOperand.FromMetadata (SourcedMetadataToken.ofInt (System.Reflection.AssemblyName "Test") 0x06000001)

    /// Lay out `ops`, then rewrite the branches at the listed indices to the offsets of the
    /// instructions at the listed target indices.
    let private layOutWithBranches
        (regions : ExceptionRegion list)
        (ops : IlOp list)
        (branches : (int * int) list)
        : MethodInstructions<TypeDefn> * Map<int, int>
        =
        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match List.tryFind (fun (from, _) -> from = i) branches with
                | None -> op
                | Some (_, target) ->
                    match op with
                    | IlOp.UnaryConst (UnaryConstIlOp.Br _) -> br at.[target]
                    | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _) -> brtrue at.[target]
                    | IlOp.UnaryConst (UnaryConstIlOp.Leave _) -> IlOp.UnaryConst (UnaryConstIlOp.Leave at.[target])
                    | other -> failwith $"instruction %d{i} is %O{other}, not a branch"
            )

        layOut regions ops

    [<Test>]
    let ``straight-line code is entered at the depth its predecessor leaves`` () : unit =
        let body, at = layOut [] [ ldc ; ldc ; add ; dup ; pop ; pop ; ret ]
        let shape = analyseOrFail (inputs [] [] false) body

        [ 0..6 ]
        |> List.map (fun i -> shape.Entry.[at.[i]].Length)
        |> shouldEqual [ 0 ; 1 ; 2 ; 1 ; 2 ; 1 ; 0 ]

        shape.Reachable |> shouldEqual (Set.ofSeq at.Values)

    [<Test>]
    let ``two arms of one depth meet at a typed join`` () : unit =
        // `ldarg0; brtrue L; ldc; br J; L: ldc; ldc; add; J: pop; ret`
        let ops = [ ldarg0 ; brtrue 0 ; ldc ; br 0 ; ldc ; ldc ; add ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 7 ]
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[4]].Length |> shouldEqual 0
        shape.Entry.[at.[6]].Length |> shouldEqual 2
        shape.Entry.[at.[7]].Length |> shouldEqual 1
        shape.Entry.[at.[8]].Length |> shouldEqual 0

    [<Test>]
    let ``a loop's back edge delivers the depth its head already has`` () : unit =
        // `ldc; HEAD: dup; brtrue HEAD; pop; ret`
        let ops = [ ldc ; dup ; brtrue 0 ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 2, 1 ]
        let shape = analyseOrFail (inputs [] [] false) body

        shape.Entry.[at.[1]].Length |> shouldEqual 1
        shape.Entry.[at.[2]].Length |> shouldEqual 2
        shape.Entry.[at.[3]].Length |> shouldEqual 1

    [<Test>]
    let ``two paths of different depth at a join is a conflict, and what follows only from it is untyped`` () : unit =
        // `ldarg0; brtrue L; br J; L: ldc; br J; J: nop; ret` returning a value: `ret` would
        // underflow on the empty arm, but the arms disagree at J, so J is a conflict and
        // everything past it is neither typed nor invalid.
        let ops = [ ldarg0 ; brtrue 0 ; br 0 ; ldc ; br 0 ; nop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 3 ; 2, 5 ; 4, 5 ]
        let shape = StackShape.analyse (inputs [ other ] [] true) body

        match Map.tryFind at.[5] shape.Invalid with
        | Some (StackShapeError.DepthMismatch (_, 0, 1) as error) -> error.IsConflict |> shouldEqual true
        | other -> failwith $"expected DepthMismatch at the join, got %O{other}"

        shape.Invalid.ContainsKey at.[6] |> shouldEqual false
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Entry.ContainsKey at.[6] |> shouldEqual false
        shape.Reachable.Contains at.[6] |> shouldEqual true
        shape.Entry.[at.[3]].Length |> shouldEqual 0
        shape.Entry.[at.[4]].Length |> shouldEqual 1

    [<Test>]
    let ``an arrival at a join that already failed on another arm makes it a conflict`` () : unit =
        // `ldarg0; brtrue GOOD; br JOIN; GOOD: ldc; br JOIN; JOIN: ret` returning a value: JOIN is
        // reached empty first, where `ret` underflows, and then one deep. Two arms that disagree
        // are a conflict whichever arrives first, since the empty one may be an arm the importer
        // never imports.
        let ops = [ ldarg0 ; brtrue 0 ; br 0 ; ldc ; br 0 ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 3 ; 2, 5 ; 4, 5 ]
        let shape = StackShape.analyse (inputs [ other ] [] true) body

        shape.Invalid.[at.[5]].IsConflict |> shouldEqual true
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Reachable.Contains at.[5] |> shouldEqual true

    [<Test>]
    let ``a conflict at one join leaves every join it feeds unknown rather than classified by the other arms``
        ()
        : unit
        =
        // `ldarg0; switch (A, B); ldc; br J; A: nop; br J; B: nop; br R; J: nop; R: ret` returning a
        // value. J is a conflict (one deep from the fall-through, empty from A). R is reached
        // from B empty and from J at a depth nobody knows: it is not an underflow, since the path
        // through the fall-through arm reaches it one deep, so R is untyped rather than invalid.
        let ops =
            [
                ldarg0 // 0
                IlOp.Switch (ImmutableArray.Create (0, 0)) // 1 -> A, B
                ldc // 2
                br 0 // 3 -> J
                nop // 4 A
                br 0 // 5 -> J
                nop // 6 B
                br 0 // 7 -> R
                nop // 8 J
                ret // 9 R
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> IlOp.Switch (ImmutableArray.Create (at.[4], at.[6]))
                | _ -> op
            )

        let body, at = layOutWithBranches [] ops [ 3, 8 ; 5, 8 ; 7, 9 ]
        let shape = StackShape.analyse (inputs [ other ] [] true) body

        shape.Invalid.[at.[8]].IsConflict |> shouldEqual true
        shape.Invalid.ContainsKey at.[9] |> shouldEqual false
        shape.Entry.ContainsKey at.[9] |> shouldEqual false
        shape.Reachable.Contains at.[9] |> shouldEqual true
        shape.Entry.[at.[6]].Length |> shouldEqual 0

    [<Test>]
    let ``popping an empty stack is invalid IL`` () : unit =
        let body, _ = layOut [] [ pop ; ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs [] [] false) body).Invalid with
        | Some (StackShapeError.StackUnderflow (0, _, 0) as error) -> error.IsConflict |> shouldEqual false
        | other -> failwith $"expected StackUnderflow at offset 0, got %O{other}"

    [<Test>]
    let ``an untypable arm leaves the rest of the body typed`` () : unit =
        // `ldarg0; brtrue BAD; ldc; ret; BAD: pop; ret`: BAD underflows, and that is recorded
        // against BAD alone; the other arm is typed, and what follows BAD is untyped.
        let ops = [ ldarg0 ; brtrue 0 ; ldc ; ret ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 4 ]
        let shape = StackShape.analyse (inputs [ other ] [] true) body

        shape.Entry.[at.[2]].Length |> shouldEqual 0
        shape.Entry.[at.[3]].Length |> shouldEqual 1

        match Map.tryFind at.[4] shape.Invalid with
        | Some (StackShapeError.StackUnderflow _) -> ()
        | other -> failwith $"expected StackUnderflow at BAD, got %O{other}"

        shape.Entry.ContainsKey at.[4] |> shouldEqual false
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Invalid.ContainsKey at.[5] |> shouldEqual false
        shape.Reachable.Contains at.[5] |> shouldEqual true

    [<Test>]
    let ``a branch outside the body is invalid at the branch`` () : unit =
        let body, at = layOut [] [ ldarg0 ; brtrue 1000 ; ret ]
        let shape = StackShape.analyse (inputs [ other ] [] false) body

        match Map.tryFind at.[1] shape.Invalid with
        | Some (StackShapeError.BranchOutsideBody (_, target)) -> target |> shouldEqual 1000
        | other -> failwith $"expected BranchOutsideBody at the branch, got %O{other}"

        // The branch delivered nothing, so its fall-through is untyped.
        shape.Entry.ContainsKey at.[2] |> shouldEqual false

    [<Test>]
    let ``an argument or local beyond the signature is invalid at the load`` () : unit =
        let body, _ = layOut [] [ IlOp.Nullary NullaryIlOp.LdArg1 ; ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs [ other ] [] false) body).Invalid with
        | Some (StackShapeError.ArgumentOutOfRange (0, 1)) -> ()
        | other -> failwith $"expected ArgumentOutOfRange, got %O{other}"

        analyseOrFail (inputs [ other ; other ] [] false) body |> ignore

        let body, _ = layOut [] [ IlOp.UnaryConst (UnaryConstIlOp.Stloc_s 2uy) ; ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs [] [ other ; other ] false) body).Invalid with
        | Some (StackShapeError.LocalOutOfRange (0, 2)) -> ()
        | other -> failwith $"expected LocalOutOfRange, got %O{other}"

    [<Test>]
    let ``a handler starts with the exception, a finally starts empty, and leave empties the stack`` () : unit =
        // try { ldc; leave END } filter { pop; ldc; endfilter } handler { pop; leave END }
        // finally { endfinally } END: ret
        let ops =
            [
                ldc // 0 try start
                IlOp.UnaryConst (UnaryConstIlOp.Leave 0) // 1
                pop // 2 filter
                ldc // 3
                IlOp.Nullary NullaryIlOp.Endfilter // 4
                pop // 5 handler
                IlOp.UnaryConst (UnaryConstIlOp.Leave 0) // 6
                IlOp.Nullary NullaryIlOp.Endfinally // 7 finally handler
                ret // 8 END
            ]

        let _, at = layOut [] ops

        let regions =
            [
                ExceptionRegion.Filter (
                    at.[2],
                    {
                        TryOffset = at.[0]
                        TryLength = at.[2] - at.[0]
                        HandlerOffset = at.[5]
                        HandlerLength = at.[7] - at.[5]
                    }
                )
                ExceptionRegion.Finally
                    {
                        TryOffset = at.[0]
                        TryLength = at.[7] - at.[0]
                        HandlerOffset = at.[7]
                        HandlerLength = at.[8] - at.[7]
                    }
            ]

        let body, at = layOutWithBranches regions ops [ 1, 8 ; 6, 8 ]
        let shape = analyseOrFail (inputs [] [] false) body

        [ 0..8 ]
        |> List.map (fun i -> shape.Entry.[at.[i]].Length)
        |> shouldEqual [ 0 ; 1 ; 1 ; 0 ; 1 ; 1 ; 0 ; 0 ; 0 ]

    [<Test>]
    let ``switch delivers the stack to every target and the fall-through`` () : unit =
        let ops =
            [
                ldc // 0
                ldarg0 // 1
                IlOp.Switch (ImmutableArray.Create (0, 0)) // 2
                ldc // 3 fall-through
                pop // 4
                pop // 5 target
                ret // 6
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 2 -> IlOp.Switch (ImmutableArray.Create (at.[5], at.[5]))
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[3]].Length |> shouldEqual 1
        shape.Entry.[at.[5]].Length |> shouldEqual 1
        shape.Entry.[at.[6]].Length |> shouldEqual 0

    [<Test>]
    let ``a call pops its arguments and pushes its return value`` () : unit =
        let ops =
            [
                ldc
                ldc
                IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, callToken)
                ldc
                add
                pop
                ret
            ]

        let body, at = layOut [] ops

        let withCall (shape : TokenShape) : StackShapeInputs =
            { inputs [] [] false with
                Tokens = Map.ofList [ at.[2], shape ]
            }

        let shape = analyseOrFail (withCall (TokenShape.Callee (2, Some other))) body
        shape.Entry.[at.[3]].Length |> shouldEqual 1
        shape.Entry.[at.[5]].Length |> shouldEqual 1

        let shape = StackShape.analyse (withCall (TokenShape.Callee (2, None))) body

        match Map.tryFind at.[4] shape.Invalid with
        | Some (StackShapeError.StackUnderflow (_, _, 1)) -> ()
        | other -> failwith $"expected the add after a void call to underflow, got %O{other}"

        match Map.tryFind at.[2] (StackShape.analyse (inputs [] [] false) body).Invalid with
        | Some (StackShapeError.MissingTokenShape (offset, _) as error) ->
            offset |> shouldEqual at.[2]
            error.IsConflict |> shouldEqual false
        | other -> failwith $"expected MissingTokenShape for the call, got %O{other}"

    [<Test>]
    let ``a call whose token could not be read leaves what follows it unknown, not unreached`` () : unit =
        // `ldarg0; brtrue CALL; br JOIN; CALL: call C; JOIN: ret` returning a value, with C's
        // token unread: the call is recorded as missing its shape, and JOIN, which C may well
        // reach one deep, is untyped rather than an underflow classified from the other arm.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> CALL
                br 0 // 2 -> JOIN
                IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, callToken) // 3 CALL
                ret // 4 JOIN
            ]

        let body, at = layOutWithBranches [] ops [ 1, 3 ; 2, 4 ]
        let shape = StackShape.analyse (inputs [ other ] [] true) body

        match Map.tryFind at.[3] shape.Invalid with
        | Some (StackShapeError.MissingTokenShape _) -> ()
        | other -> failwith $"expected MissingTokenShape at the call, got %O{other}"

        shape.Invalid.ContainsKey at.[4] |> shouldEqual false
        shape.Entry.ContainsKey at.[4] |> shouldEqual false
        shape.Reachable.Contains at.[4] |> shouldEqual true

    [<Test>]
    let ``a call token that does not name a method has no shape`` () : unit =
        // Invalid IL the instruction refuses if it executes; nothing is claimed ahead of that.
        let corelib = corelib ()

        let field =
            MetadataToken.FieldDefinition (System.Reflection.Metadata.Ecma335.MetadataTokens.FieldDefinitionHandle 1)

        StackShapeTokens.ofMetadataToken corelib GenericBinding.AtDefinition UnaryMetadataTokenIlOp.Call field
        |> shouldEqual None

        StackShapeTokens.ofMetadataToken corelib GenericBinding.AtDefinition UnaryMetadataTokenIlOp.Newobj field
        |> shouldEqual None

        // Rows the tables do not have, and a signature row that is not there to decode.
        let absent (token : MetadataToken) : unit =
            StackShapeTokens.ofMetadataToken corelib GenericBinding.AtDefinition UnaryMetadataTokenIlOp.Call token
            |> shouldEqual None

        absent (
            MetadataToken.MethodDef (System.Reflection.Metadata.Ecma335.MetadataTokens.MethodDefinitionHandle 0xFFFFFF)
        )

        absent (
            MetadataToken.MemberReference (
                System.Reflection.Metadata.Ecma335.MetadataTokens.MemberReferenceHandle 0xFFFFFF
            )
        )

        absent (
            MetadataToken.MethodSpecification (
                System.Reflection.Metadata.Ecma335.MetadataTokens.MethodSpecificationHandle 0xFFFFFF
            )
        )

        absent (
            MetadataToken.StandaloneSignature (
                System.Reflection.Metadata.Ecma335.MetadataTokens.StandaloneSignatureHandle 0xFFFFFF
            )
        )

        // A member reference to a field, where one exists to try.
        let test =
            Assembly.readFile (LoggerFactory.makeTest () |> snd) typeof<RunResult>.Assembly.Location

        for KeyValue (handle, reference) in test.Members do
            match reference.Signature with
            | MemberSignature.Field _ ->
                StackShapeTokens.ofMetadataToken
                    test
                    GenericBinding.AtDefinition
                    UnaryMetadataTokenIlOp.Call
                    (MetadataToken.MemberReference handle)
                |> shouldEqual None
            | MemberSignature.Method _ -> ()

    [<Test>]
    let ``a calli signature's arity read from the blob agrees with the full decoder`` () : unit =
        // CoreLib's standalone signatures are its `calli` targets and its locals signatures;
        // for every one the full decoder accepts, the arity reader must agree, and a locals
        // signature is no call at all.
        let corelib = corelib ()

        let reader =
            System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader corelib.PeReader

        let rows =
            System.Reflection.Metadata.Ecma335.MetadataReaderExtensions.GetTableRowCount (
                reader,
                System.Reflection.Metadata.Ecma335.TableIndex.StandAloneSig
            )

        rows > 0 |> shouldEqual true
        let mutable methods = 0

        for row in 1..rows do
            let handle =
                System.Reflection.Metadata.Ecma335.MetadataTokens.StandaloneSignatureHandle row

            let signature = reader.GetStandaloneSignature handle
            let header = reader.GetBlobReader(signature.Signature).ReadSignatureHeader ()

            let actual =
                StackShapeTokens.ofMetadataToken
                    corelib
                    GenericBinding.AtDefinition
                    UnaryMetadataTokenIlOp.Calli
                    (MetadataToken.StandaloneSignature handle)

            if header.Kind = System.Reflection.Metadata.SignatureKind.Method then
                methods <- methods + 1

                let expected =
                    signature.DecodeMethodSignature (TypeDefn.typeProvider corelib.Name, ())
                    |> TypeMethodSignature.make
                    |> StackShapeTokens.calleeShape corelib GenericBinding.AtDefinition GenericSubstitution.None

                actual |> shouldEqual (Some expected)
            else
                actual |> shouldEqual None

        methods > 0 |> shouldEqual true

    [<Test>]
    let ``calli pops the function pointer after the arguments, and newobj pushes the object`` () : unit =
        let ops =
            [
                ldc
                ldc
                IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, callToken)
                IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, callToken)
                pop
                ret
            ]

        let body, at = layOut [] ops

        let withTokens =
            { inputs [] [] false with
                Tokens = Map.ofList [ at.[2], TokenShape.Callee (1, None) ; at.[3], TokenShape.Callee (0, None) ]
            }

        let shape = analyseOrFail withTokens body
        shape.Entry.[at.[3]].Length |> shouldEqual 0
        shape.Entry.[at.[4]].Length |> shouldEqual 1

    [<Test>]
    let ``ret pops the return value exactly when the method returns one`` () : unit =
        let body, _ = layOut [] [ ldc ; ret ]
        analyseOrFail (inputs [] [] true) body |> ignore

        // A value left on the stack at `ret` is not an underflow, and the analysis does not
        // police it; only the pop can fail, and it does not here.
        (StackShape.analyse (inputs [] [] false) body).Invalid.IsEmpty
        |> shouldEqual true

        let body, _ = layOut [] [ ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs [] [] true) body).Invalid with
        | Some (StackShapeError.StackUnderflow (0, _, 0)) -> ()
        | other -> failwith $"expected StackUnderflow at ret, got %O{other}"

    [<Test>]
    let ``a void return under a custom modifier is still a void return`` () : unit =
        // An init-only setter is `void modreq(IsExternalInit)`, which the decoder reports as a
        // modified type; a `ret` from it pops nothing, and a call to it pushes nothing.
        let isExternalInit =
            TypeDefn.FromReference (
                {
                    Handle =
                        ComparableTypeReferenceHandle.Make (
                            System.Reflection.Metadata.Ecma335.MetadataTokens.TypeReferenceHandle 1
                        )
                    Name = "IsExternalInit"
                    Namespace = "System.Runtime.CompilerServices"
                    ResolutionScope =
                        TypeRefResolutionScope.Assembly (
                            System.Reflection.Metadata.Ecma335.MetadataTokens.AssemblyReferenceHandle 1
                        )
                },
                System.Reflection.Metadata.SignatureTypeKind.Class
            )

        let modifiedVoid =
            TypeDefn.Modified
                {
                    Unmodified = TypeDefn.Void
                    Modifier = isExternalInit
                    IsRequired = true
                }

        let corelib = corelib ()

        let returnShape (returnType : MethodReturnType<TypeDefn>) : SlotShape option =
            StackShapeTokens.returnShape corelib GenericBinding.AtDefinition GenericSubstitution.None returnType

        returnShape (MethodReturnType.Returns modifiedVoid) |> shouldEqual None

        returnShape (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Single))
        |> shouldEqual (Some single)

        returnShape MethodReturnType.Void |> shouldEqual None

    [<Test>]
    let ``an explicit this is a parameter, not an extra slot`` () : unit =
        // ECMA-335 II.15.3: with EXPLICITTHIS the receiver is the first parameter type, so a
        // `calli` through such a signature pops parameters plus the function pointer only.
        let signature (attributes : System.Reflection.Metadata.SignatureAttributes) : TypeMethodSignature<TypeDefn> =
            {
                Header =
                    ComparableSignatureHeader.Make (
                        System.Reflection.Metadata.SignatureHeader (
                            System.Reflection.Metadata.SignatureKind.Method,
                            System.Reflection.Metadata.SignatureCallingConvention.Default,
                            attributes
                        )
                    )
                ParameterTypes =
                    [
                        TypeDefn.PrimitiveType PrimitiveType.Object
                        TypeDefn.PrimitiveType PrimitiveType.Int32
                    ]
                GenericParameterCount = 0
                RequiredParameterCount = 2
                ReturnType = MethodReturnType.Void
            }

        StackShapeTokens.calleeShape
            (corelib ())
            GenericBinding.AtDefinition
            GenericSubstitution.None
            (signature (
                System.Reflection.Metadata.SignatureAttributes.Instance
                ||| System.Reflection.Metadata.SignatureAttributes.ExplicitThis
            ))
        |> shouldEqual (TokenShape.Callee (2, None))

        StackShapeTokens.calleeShape
            (corelib ())
            GenericBinding.AtDefinition
            GenericSubstitution.None
            (signature System.Reflection.Metadata.SignatureAttributes.Instance)
        |> shouldEqual (TokenShape.Callee (3, None))

        StackShapeTokens.calleeShape
            (corelib ())
            GenericBinding.AtDefinition
            GenericSubstitution.None
            (signature System.Reflection.Metadata.SignatureAttributes.None)
        |> shouldEqual (TokenShape.Callee (2, None))

    [<Test>]
    let ``reachability follows branches and handler entries but not dead code`` () : unit =
        // `br END; <dead call>; END: ret`, plus a catch handler nothing branches to.
        let ops =
            [
                br 0 // 0 -> END
                IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, callToken) // 1 dead
                ret // 2 END
                pop // 3 handler
                ret // 4
            ]

        let _, at = layOut [] ops

        let regions =
            [
                ExceptionRegion.Catch (
                    ExceptionCatchType.FromMetadata (MetadataToken.ofInt 0x01000001),
                    {
                        TryOffset = at.[0]
                        TryLength = at.[3] - at.[0]
                        HandlerOffset = at.[3]
                        HandlerLength = at.[4] + 1 - at.[3]
                    }
                )
            ]

        let body, at = layOutWithBranches regions ops [ 0, 2 ]

        StackShape.reachable body
        |> shouldEqual (Set.ofList [ at.[0] ; at.[2] ; at.[3] ; at.[4] ])

        // The dead call has no token shape, and no one asks for it.
        let shape = analyseOrFail (inputs [] [] false) body
        shape.Entry.ContainsKey at.[1] |> shouldEqual false
        shape.Entry.[at.[3]].Length |> shouldEqual 1

    // ---------- Widths: what a float32 becomes at a join ----------

    [<Test>]
    let ``straight-line float32 arithmetic stays single and needs no promotion`` () : unit =
        let body, at = layOut [] [ ldcR4 ; ldcR4 ; add ; ldcR4 ; add ; pop ; ret ]
        let shape = analyseOrFail (inputs [] [] false) body

        shape.Entry.[at.[2]] |> shouldEqual [ single ; single ]
        shape.Entry.[at.[3]] |> shouldEqual [ single ]
        shape.Entry.[at.[4]] |> shouldEqual [ single ; single ]
        shape.Entry.[at.[5]] |> shouldEqual [ single ]
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a float32 arm meeting a double arm makes the join double and promotes the float32 arm`` () : unit =
        // The finding's shape: select 16777216f or 16777216d, then `ldc.r4 1; add; ldc.r4 1; add`.
        // CoreCLR types the joined slot double, so the float32 arm is cast on arrival and the
        // additions happen in double.
        let ops =
            [
                ldloc0 // 0
                brtrue 0 // 1 -> single arm
                ldcR8 // 2
                br 0 // 3 -> join
                ldcR4 // 4  single arm
                ldcR4 // 5  join
                add // 6
                ldcR4 // 7
                add // 8
                pop // 9
                ret // 10
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> brtrue at.[4]
                | 3 -> br at.[5]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ other ] [ other ] false) body

        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Entry.[at.[6]] |> shouldEqual [ single ; double ]
        shape.Entry.[at.[7]] |> shouldEqual [ double ]
        shape.Entry.[at.[9]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[5], [ 0 ] ])

    [<Test>]
    let ``the arm order does not matter`` () : unit =
        let ops =
            [
                ldloc0 // 0
                brtrue 0 // 1 -> double arm
                ldcR4 // 2
                br 0 // 3 -> join
                ldcR8 // 4
                pop // 5 join
                ret // 6
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> brtrue at.[4]
                | 3 -> br at.[5]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ other ] [ other ] false) body

        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[5], [ 0 ] ])

    [<Test>]
    let ``two float32 arms need no promotion`` () : unit =
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdArg0
                brtrue 0
                ldcR4
                br 0
                ldcR4
                pop
                ret
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> brtrue at.[4]
                | 3 -> br at.[5]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[5]] |> shouldEqual [ single ]
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a loop whose back edge delivers a double widens the loop head`` () : unit =
        // `ldc.r4; L: ldc.r8; mul; br L`: the head first sees a float32, then a double from the
        // back edge, so the slot is double and the first arrival is promoted.
        let ops = [ ldcR4 ; ldcR8 ; IlOp.Nullary NullaryIlOp.Mul ; br 0 ]
        let _, at = layOut [] ops
        let ops = ops |> List.mapi (fun i op -> if i = 3 then br at.[1] else op)
        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [] [] false) body

        shape.Entry.[at.[1]] |> shouldEqual [ double ]
        shape.Entry.[at.[2]] |> shouldEqual [ double ; double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[1], [ 0 ] ])

    [<Test>]
    let ``a float meeting a non-float at a join is a conflict, and what follows only from it is unknown`` () : unit =
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdArg0
                brtrue 0
                ldcR4
                br 0
                ldcI4
                pop
                ret
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> brtrue at.[4]
                | 3 -> br at.[5]
                | _ -> op
            )

        let body, at = layOut [] ops

        let shape = StackShape.analyse (inputs [ other ] [] false) body

        match Map.tryFind at.[5] shape.Invalid with
        | Some (StackShapeError.FloatMergedWithOther (offset, 0)) -> offset |> shouldEqual at.[5]
        | other -> failwith $"expected FloatMergedWithOther at the join, got %O{other}"

        // The join and what only it leads to are untyped; the arms before it are typed. Both
        // stay reachable, so an interpreter can tell them from dead code.
        shape.Invalid.[at.[5]].IsConflict |> shouldEqual true
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Entry.ContainsKey at.[6] |> shouldEqual false
        shape.Entry.ContainsKey at.[4] |> shouldEqual true
        shape.Reachable.Contains at.[5] |> shouldEqual true
        shape.Reachable.Contains at.[6] |> shouldEqual true

    [<Test>]
    let ``conv.r.un is single only when conv.r4 follows it`` () : unit =
        let body1, at1 =
            layOut
                []
                [
                    ldcI4
                    IlOp.Nullary NullaryIlOp.Conv_r_un
                    IlOp.Nullary NullaryIlOp.Conv_R4
                    pop
                    ret
                ]

        let shape1 = analyseOrFail (inputs [] [] false) body1
        shape1.Entry.[at1.[2]] |> shouldEqual [ single ]
        shape1.Entry.[at1.[3]] |> shouldEqual [ single ]

        let body2, at2 =
            layOut [] [ ldcI4 ; IlOp.Nullary NullaryIlOp.Conv_r_un ; pop ; ret ]

        let shape2 = analyseOrFail (inputs [] [] false) body2
        shape2.Entry.[at2.[2]] |> shouldEqual [ double ]

    [<Test>]
    let ``a generic parameter takes the shape of the argument the call site binds it to`` () : unit =
        // `flag ? Id<float>(1f) : 2f`: the MethodSpec binds `!!0` to float32, so the call pushes a
        // single and the join with the literal is valid. Unbound, or bound to the caller's own
        // parameter, it is `Other`.
        let corelib = corelib ()

        let bound =
            { GenericSubstitution.None with
                MethodArguments = Some (ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Single))
            }

        StackShapeTokens.shapeOfTypeDefnWith
            corelib
            GenericBinding.AtDefinition
            bound
            (TypeDefn.GenericMethodParameter 0)
        |> shouldEqual single

        StackShapeTokens.shapeOfTypeDefnWith
            corelib
            GenericBinding.AtDefinition
            GenericSubstitution.None
            (TypeDefn.GenericMethodParameter 0)
        |> shouldEqual other

        let boundToCallers =
            { GenericSubstitution.None with
                TypeArguments = Some (ImmutableArray.Create (TypeDefn.GenericMethodParameter 0))
            }

        StackShapeTokens.shapeOfTypeDefnWith
            corelib
            GenericBinding.AtDefinition
            boundToCallers
            (TypeDefn.GenericTypeParameter 0)
        |> shouldEqual other

        let boundType =
            { GenericSubstitution.None with
                TypeArguments = Some (ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Double))
            }

        StackShapeTokens.shapeOfTypeDefnWith
            corelib
            GenericBinding.AtDefinition
            boundType
            (TypeDefn.GenericTypeParameter 0)
        |> shouldEqual double

        // A method's own parameter is not the type's: the wrong table does not bind it.
        StackShapeTokens.shapeOfTypeDefnWith
            corelib
            GenericBinding.AtDefinition
            boundType
            (TypeDefn.GenericMethodParameter 0)
        |> shouldEqual other

    [<Test>]
    let ``a parameter no token binds takes the shape of the running instantiation`` () : unit =
        // In `M<float>`, `ldarg` of a `T` argument pushes a float32; a token's `!!0` bound to the
        // caller's `!0` is read through the same instantiation.
        let corelib = corelib ()

        let running =
            {
                TypeParameter = fun i -> if i = 0 then single else other
                MethodParameter = fun i -> if i = 0 then double else other
            }

        StackShapeTokens.shapeOfTypeDefn corelib running (TypeDefn.GenericTypeParameter 0)
        |> shouldEqual single

        StackShapeTokens.shapeOfTypeDefn corelib running (TypeDefn.GenericMethodParameter 0)
        |> shouldEqual double

        StackShapeTokens.shapeOfTypeDefn corelib running (TypeDefn.GenericTypeParameter 1)
        |> shouldEqual other

        let boundToCallers =
            { GenericSubstitution.None with
                MethodArguments = Some (ImmutableArray.Create (TypeDefn.GenericTypeParameter 0))
            }

        StackShapeTokens.shapeOfTypeDefnWith corelib running boundToCallers (TypeDefn.GenericMethodParameter 0)
        |> shouldEqual single

    [<Test>]
    let ``every block a predecessor shares with a widened block is widened too`` () : unit =
        // A delivers a float32 to both B (fall-through) and C (branch); D delivers a double to C.
        // CoreCLR gives A's successors one spill temp, so B is typed double as well, and the
        // float32 arriving there is promoted.
        let ops =
            [
                ldloc0 // 0
                brtrue 0 // 1 -> D
                ldcR4 // 2  A
                ldloc0 // 3
                brtrue 0 // 4 -> C
                pop // 5  B
                ret // 6
                ldcR8 // 7  D
                br 0 // 8 -> C
                pop // 9  C
                ret // 10
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> brtrue at.[7]
                | 4 -> brtrue at.[9]
                | 8 -> br at.[9]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ other ] [ other ] false) body

        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Entry.[at.[9]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[5], [ 0 ] ; at.[9], [ 0 ] ])

    [<Test>]
    let ``a late branch joining two settled cliques requeues the one it widens`` () : unit =
        // M2 is first reached alone with a float32 and fully processed; M1 is reached with a
        // double. A later branch has M1 and M2 as its two successors, which makes them one
        // clique typed double, so M2 and everything after it must be redone.
        let ops =
            [
                ldloc0 // 0
                brtrue 0 // 1 -> L1
                ldcR4 // 2
                br 0 // 3 -> M2
                ldloc0 // 4  L1
                brtrue 0 // 5 -> L2
                ldcR8 // 6
                br 0 // 7 -> M1
                ldcR4 // 8  L2
                ldloc0 // 9
                brtrue 0 // 10 -> M1, fall through to M2
                IlOp.Nullary NullaryIlOp.Neg // 11 M2
                pop // 12
                ret // 13
                pop // 14 M1
                ret // 15
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> brtrue at.[4]
                | 3 -> br at.[11]
                | 5 -> brtrue at.[8]
                | 7 -> br at.[14]
                | 10 -> brtrue at.[14]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ other ] [ other ] false) body

        shape.Entry.[at.[11]] |> shouldEqual [ double ]
        shape.Entry.[at.[12]] |> shouldEqual [ double ]
        shape.Entry.[at.[14]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[11], [ 0 ] ; at.[14], [ 0 ] ])

    [<Test>]
    let ``a reference named System.Single is the primitive only when scoped to a framework assembly`` () : unit =
        // A facade forwards the primitive; a guest's own `System.Single` is a struct to the JIT.
        // The test assembly references both kinds of assembly.
        let test =
            Assembly.readFile (LoggerFactory.makeTest () |> snd) typeof<RunResult>.Assembly.Location

        let scope (assemblyName : string) : System.Reflection.Metadata.AssemblyReferenceHandle =
            test.AssemblyReferences
            |> Seq.pick (fun (KeyValue (handle, reference)) ->
                if reference.Name.Name = assemblyName then
                    Some handle
                else
                    None
            )

        let reference (name : string) (scope : System.Reflection.Metadata.AssemblyReferenceHandle) : TypeDefn =
            TypeDefn.FromReference (
                {
                    Handle =
                        ComparableTypeReferenceHandle.Make (
                            System.Reflection.Metadata.Ecma335.MetadataTokens.TypeReferenceHandle 1
                        )
                    Name = name
                    Namespace = "System"
                    ResolutionScope = TypeRefResolutionScope.Assembly scope
                },
                System.Reflection.Metadata.SignatureTypeKind.ValueType
            )

        let shapeOf (ty : TypeDefn) : SlotShape =
            StackShapeTokens.shapeOfTypeDefn test GenericBinding.AtDefinition ty

        shapeOf (reference "Single" (scope "System.Runtime")) |> shouldEqual single
        shapeOf (reference "Double" (scope "System.Runtime")) |> shouldEqual double
        shapeOf (reference "Single" (scope "FsCheck")) |> shouldEqual other
        shapeOf (reference "Vector" (scope "System.Runtime")) |> shouldEqual other

        // A reference into a scope the table does not have is no primitive either.
        shapeOf (reference "Single" (System.Reflection.Metadata.Ecma335.MetadataTokens.AssemblyReferenceHandle 0xFFFF))
        |> shouldEqual other

    [<Test>]
    let ``a conflict at one successor leaves the sibling sharing its spill temps unknown`` () : unit =
        // `ldarg0; brtrue J; ldc.r4; ldarg0; brtrue K; K': pop; ret; K: pop; ret; J: ldc.r4;
        // ldc.r4; br K`. K and K' are the two successors of one branch, so their slots share
        // spill temps; J then reaches K two deep where the branch reached both one deep. K is a
        // conflict, and K', which shares K's temp, is unknown too rather than typed from the
        // branch alone.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> J
                ldcR4 // 2
                ldarg0 // 3
                brtrue 0 // 4 -> K
                pop // 5 K'
                ret // 6
                pop // 7 K
                ret // 8
                ldcR4 // 9 J
                ldcR4 // 10
                br 0 // 11 -> K
            ]

        let body, at = layOutWithBranches [] ops [ 1, 9 ; 4, 7 ; 11, 7 ]
        let shape = StackShape.analyse (inputs [ other ] [] false) body

        shape.Invalid.[at.[7]].IsConflict |> shouldEqual true
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Invalid.ContainsKey at.[5] |> shouldEqual false
        shape.Reachable.Contains at.[5] |> shouldEqual true

    [<Test>]
    let ``a late branch joining two settled cliques that cannot meet leaves both unknown`` () : unit =
        // The shape of the test above, with M1 reached by an integer instead of a double: the
        // late branch makes M1 and M2 one clique, which cannot meet, so both sides and all that
        // follows them are unknown, not just the side the branch delivered to second.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> L1
                ldcR4 // 2
                br 0 // 3 -> M2
                ldarg0 // 4  L1
                brtrue 0 // 5 -> L2
                ldcI4 // 6
                br 0 // 7 -> M1
                ldcR4 // 8  L2
                ldarg0 // 9
                brtrue 0 // 10 -> M1, fall through to M2
                IlOp.Nullary NullaryIlOp.Neg // 11 M2
                pop // 12
                ret // 13
                pop // 14 M1
                ret // 15
            ]

        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 11 ; 5, 8 ; 7, 14 ; 10, 14 ]
        let shape = StackShape.analyse (inputs [ other ] [] false) body

        for i in [ 11 ; 12 ; 13 ; 14 ; 15 ] do
            shape.Entry.ContainsKey at.[i] |> shouldEqual false

        shape.Invalid
        |> Map.filter (fun _ e -> e.IsConflict)
        |> Map.isEmpty
        |> shouldEqual false

        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a calli return encoded as a type token is judged like any other type`` () : unit =
        // `valuetype System.Single` (ELEMENT_TYPE_VALUETYPE then a TypeDefOrRef) is a valid way
        // to return the primitive; the arity reader judges the token by name and scope.
        let corelib = corelib ()

        let singleHandle =
            corelib.TypeDefs
            |> Seq.pick (fun (KeyValue (handle, typeInfo)) ->
                if typeInfo.Namespace = "System" && typeInfo.Name = "Single" then
                    Some handle
                else
                    None
            )

        let blob (returnEncoding : System.Reflection.Metadata.BlobBuilder -> unit) : byte[] =
            let builder = System.Reflection.Metadata.BlobBuilder ()
            builder.WriteByte 0uy // default calling convention, static
            builder.WriteCompressedInteger 1 // one parameter
            returnEncoding builder
            builder.WriteByte 0x08uy // ELEMENT_TYPE_I4 parameter
            builder.ToArray ()

        let shapeOf (bytes : byte[]) : TokenShape option =
            use pinned = fixed bytes
            let reader = System.Reflection.Metadata.BlobReader (pinned, bytes.Length)
            StackShapeTokens.calliShapeOfBlob corelib GenericBinding.AtDefinition reader

        let typeDef (builder : System.Reflection.Metadata.BlobBuilder) : unit =
            builder.WriteByte 0x11uy // ELEMENT_TYPE_VALUETYPE

            System.Reflection.Metadata.Ecma335.CodedIndex.TypeDefOrRef (
                System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit singleHandle
                : System.Reflection.Metadata.EntityHandle
            )
            |> builder.WriteCompressedInteger

        shapeOf (blob typeDef)
        |> shouldEqual (Some (TokenShape.Callee (1, Some single)))

        shapeOf (blob (fun b -> b.WriteByte 0x0Duy)) // ELEMENT_TYPE_R8
        |> shouldEqual (Some (TokenShape.Callee (1, Some double)))

        shapeOf (blob (fun b -> b.WriteByte 0x01uy)) // ELEMENT_TYPE_VOID
        |> shouldEqual (Some (TokenShape.Callee (1, None)))

        // A blob that ends before its return type is no call.
        shapeOf [| 0uy ; 1uy |] |> shouldEqual None

    [<Test>]
    let ``a sibling is unknown whichever order the conflict and the branch arrive in`` () : unit =
        // The sibling test with a `nop` before the branch, so that J reaches K two deep before
        // the branch delivers one value to K and K'. K is already unknown when the branch
        // arrives; K' shares its temp all the same.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> J
                ldcR4 // 2
                ldarg0 // 3
                nop // 4
                brtrue 0 // 5 -> K
                pop // 6 K'
                ret // 7
                pop // 8 K
                ret // 9
                ldcR4 // 10 J
                ldcR4 // 11
                br 0 // 12 -> K
            ]

        let body, at = layOutWithBranches [] ops [ 1, 10 ; 5, 8 ; 12, 8 ]
        let shape = StackShape.analyse (inputs [ other ] [] false) body

        shape.Invalid.[at.[8]].IsConflict |> shouldEqual true
        shape.Entry.ContainsKey at.[6] |> shouldEqual false
        shape.Invalid.ContainsKey at.[6] |> shouldEqual false

    [<Test>]
    let ``a handler entry and the method entry share no spill temp`` () : unit =
        // try { ldarg0; brtrue L; leave END; L: ldc.r4; leave END } catch { pop; leave END }
        // END: ret. The catch handler's exception slot is its own temp: it is untyped by no
        // conflict, and the method entry is typed as ever.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> L
                IlOp.UnaryConst (UnaryConstIlOp.Leave 0) // 2 -> END
                ldcR4 // 3 L
                IlOp.UnaryConst (UnaryConstIlOp.Leave 0) // 4 -> END
                pop // 5 handler
                IlOp.UnaryConst (UnaryConstIlOp.Leave 0) // 6 -> END
                ret // 7 END
            ]

        let _, at = layOut [] ops

        let regions =
            [
                ExceptionRegion.Catch (
                    ExceptionCatchType.FromMetadata (MetadataToken.ofInt 0x01000001),
                    {
                        TryOffset = at.[0]
                        TryLength = at.[5] - at.[0]
                        HandlerOffset = at.[5]
                        HandlerLength = at.[7] - at.[5]
                    }
                )
            ]

        let body, at = layOutWithBranches regions ops [ 1, 3 ; 2, 7 ; 4, 7 ; 6, 7 ]
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[0]] |> shouldEqual []
        shape.Entry.[at.[5]] |> shouldEqual [ other ]
        shape.Entry.[at.[7]] |> shouldEqual []

    [<TestCase(false)>]
    [<TestCase(true)>]
    let ``the two successors of a dead conditional share a temp, so a live single meets a live double``
        (literalCondition : bool)
        : unit
        =
        // `ldloc0; brtrue C_ARM; ldc.r4; br B; C_ARM: ldc.r8; br C; DEAD: ldloc0; brtrue C; B: pop;
        // ret; C: pop; ret`: nothing reaches DEAD, but the JIT walks its two successors B and C
        // into one spill clique, so B's float32 is a double, and cast on arrival. A branch on a
        // literal that nothing reaches is never imported, so never folded either.
        let ops =
            [
                ldloc0 // 0
                brtrue 0 // 1 -> C_ARM
                ldcR4 // 2
                br 0 // 3 -> B
                ldcR8 // 4 C_ARM
                br 0 // 5 -> C
                (if literalCondition then ldc else ldloc0) // 6 DEAD
                brtrue 0 // 7 -> C, falls through to B
                pop // 8 B
                ret // 9
                pop // 10 C
                ret // 11
            ]

        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 8 ; 5, 10 ; 7, 10 ]
        let shape = analyseOrFail (inputs [ other ] [ other ] false) body

        shape.Entry.[at.[8]] |> shouldEqual [ double ]
        shape.Entry.[at.[10]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[6] |> shouldEqual false
        shape.Promotions |> shouldEqual (Map.ofList [ at.[8], [ 0 ] ])

    /// The refusals in `shape.Invalid`, each at its offset with the branch it names, failing on
    /// any other error. `StackShapeError` carries an `IlOp`, which has no equality.
    let private refusalsOf (shape : StackShape) : Map<int, int> =
        shape.Invalid
        |> Map.map (fun offset error ->
            match error with
            | StackShapeError.WidthDependsOnFoldedBranch (at, branch) when at = offset -> branch
            | other -> failwith $"expected only refusals, got %O{other} at offset %d{offset}"
        )

    /// `COND; brtrue A; ldc.r4; br J; A: ldc.r8; J: pop; ret`, with `COND` the listed instructions.
    let private literalJoin (condition : IlOp list) : MethodInstructions<TypeDefn> * Map<int, int> * int * int =
        let ops =
            condition
            @ [
                brtrue 0 // B -> A
                ldcR4
                br 0 // -> J
                ldcR8 // A
                pop // J
                ret
            ]

        let b = condition.Length
        let body, at = layOutWithBranches [] ops [ b, b + 3 ; b + 2, b + 4 ]
        body, at, at.[b], at.[b + 4]

    [<TestCase(0, true)>]
    [<TestCase(1, true)>]
    [<TestCase(2, true)>]
    [<TestCase(3, false)>]
    [<TestCase(4, false)>]
    let ``a float32 meeting a double downstream of a branch on literals is refused, and one on a runtime value is promoted``
        (conditionIndex : int, literal : bool)
        : unit
        =
        let condition =
            [
                [ ldc ]
                [ ldc ; ldc ; add ]
                [ IlOp.Nullary NullaryIlOp.LdNull ]
                [ ldloc0 ]
                [ ldloc0 ; ldc ; add ]
            ].[conditionIndex]

        let body, at, branch, join = literalJoin condition
        let shape = StackShape.analyse (inputs [] [ other ] false) body

        if literal then
            refusalsOf shape |> shouldEqual (Map.ofList [ join, branch ])
            shape.Invalid.[join].IsConflict |> shouldEqual false
            shape.Entry.ContainsKey join |> shouldEqual false
            // What follows only from the join is unknown, neither typed nor refused.
            shape.Entry.ContainsKey at.[condition.Length + 5] |> shouldEqual false
            shape.Invalid.ContainsKey at.[condition.Length + 5] |> shouldEqual false
            shape.Promotions |> shouldEqual Map.empty
        else
            shape.Invalid.IsEmpty |> shouldEqual true
            shape.Entry.[join] |> shouldEqual [ double ]
            shape.Promotions |> shouldEqual (Map.ofList [ join, [ 0 ] ])

        // The arms themselves are typed either way.
        shape.Entry.[at.[condition.Length + 1]] |> shouldEqual []
        shape.Entry.[at.[condition.Length + 3]] |> shouldEqual []

    [<TestCase(0, true)>]
    [<TestCase(1, true)>]
    [<TestCase(2, true)>]
    [<TestCase(3, true)>]
    [<TestCase(4, true)>]
    [<TestCase(5, false)>]
    [<TestCase(6, false)>]
    [<TestCase(7, true)>]
    [<TestCase(8, true)>]
    [<TestCase(9, true)>]
    [<TestCase(10, false)>]
    [<TestCase(11, false)>]
    let ``a float32 meeting a double downstream of a branch the importer may fold is refused, and one on a runtime value is promoted``
        (conditionIndex : int, foldable : bool)
        : unit
        =
        let call = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, callToken)
        let callvirt = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, callToken)
        let ldsfld = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsfld, callToken)
        let ldtoken = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, callToken)

        let returning (arguments : int) : TokenShape option =
            Some (TokenShape.Callee (arguments, Some other))

        let condition : (IlOp * TokenShape option) list =
            [
                // An intrinsic such as `Vector128.IsHardwareAccelerated`.
                [ call, returning 0 ]
                // A static readonly field, which the importer reads once its class is initialised.
                [ ldsfld, Some (TokenShape.Field other) ]
                // `typeof(A) == typeof(B)`.
                [
                    ldtoken, None
                    call, returning 1
                    ldtoken, None
                    call, returning 1
                    call, returning 2
                ]
                // `typeof(A).IsValueType`.
                [ ldtoken, None ; call, returning 1 ; callvirt, returning 1 ]
                // An intrinsic's result combined with a literal.
                [ call, returning 0 ; ldc, None ; add, None ]
                // A call on a local, which the importer never holds as a constant.
                [ ldloc0, None ; call, returning 1 ]
                // A static field combined with a local.
                [ ldsfld, Some (TokenShape.Field other) ; ldloc0, None ; add, None ]
                // An argument, which an inlinee receives as the constant its caller passes.
                [ ldarg0, None ]
                // A call on an argument.
                [ ldarg0, None ; call, returning 1 ]
                // An argument, though a different argument is written.
                [
                    ldc, None
                    IlOp.UnaryConst (UnaryConstIlOp.Starg_s 1uy), None
                    ldarg0, None
                ]
                // An argument the body writes, which the JIT then never substitutes.
                [
                    ldc, None
                    IlOp.UnaryConst (UnaryConstIlOp.Starg_s 0uy), None
                    ldarg0, None
                ]
                // An argument whose address the body takes, which the JIT then never substitutes.
                [
                    IlOp.UnaryConst (UnaryConstIlOp.Ldarga_s 0uy), None
                    pop, None
                    ldarg0, None
                ]
            ].[conditionIndex]

        let body, at, branch, join = literalJoin (List.map fst condition)

        let inputs =
            { inputs [ other ; other ] [ other ] false with
                Tokens =
                    condition
                    |> List.indexed
                    |> List.choose (fun (i, (_, token)) -> token |> Option.map (fun t -> at.[i], t))
                    |> Map.ofList
            }

        let shape = StackShape.analyse inputs body

        if foldable then
            refusalsOf shape |> shouldEqual (Map.ofList [ join, branch ])
            shape.Promotions |> shouldEqual Map.empty
        else
            shape.Invalid.IsEmpty |> shouldEqual true
            shape.Promotions |> shouldEqual (Map.ofList [ join, [ 0 ] ])

    [<Test>]
    let ``a join of one width downstream of a branch on literals is typed`` () : unit =
        // `ldc.i4.1; brtrue A; ldc.r4; br J; A: ldc.r4; J: pop; ret`: whichever arm the JIT
        // imports, the join is single.
        let ops = [ ldc ; brtrue 0 ; ldcR4 ; br 0 ; ldcR4 ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 5 ]
        let shape = analyseOrFail (inputs [] [] false) body

        shape.Entry.[at.[5]] |> shouldEqual [ single ]
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``every promotion sharing a spill temp with a successor of a branch on literals is refused`` () : unit =
        // B = `ldc.r8; ldc.i4.1; brtrue T1` falls through to T2, and P = `ldc.r4; ldloc0; brtrue T1`
        // falls through to S: T1, T2 and S share a temp, which is double only if B's arms are
        // imported. S is no successor of B, but its float32 arrival is refused all the same; T2
        // receives only a double, so has no width to decide, but shares the undecided temp.
        let ops (condition : IlOp) =
            [
                ldloc0 // 0
                brtrue 0 // 1 -> PP
                ldcR8 // 2
                condition // 3
                brtrue 0 // 4 B -> T1
                pop // 5 T2
                ret // 6
                ldcR4 // 7 PP
                ldloc0 // 8
                brtrue 0 // 9 P -> T1
                pop // 10 S
                ret // 11
                pop // 12 T1
                ret // 13
            ]

        let branches = [ 1, 7 ; 4, 12 ; 9, 12 ]

        let body, at = layOutWithBranches [] (ops ldc) branches
        let shape = StackShape.analyse (inputs [ other ] [ other ] false) body

        refusalsOf shape
        |> shouldEqual (Map.ofList [ at.[10], at.[4] ; at.[12], at.[4] ])

        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

        // On a runtime condition every arm is imported: the temp is double, and both float32
        // arrivals are cast.
        let body, at = layOutWithBranches [] (ops ldloc0) branches
        let shape = analyseOrFail (inputs [ other ] [ other ] false) body
        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[10], [ 0 ] ; at.[12], [ 0 ] ])

    [<TestCase(false)>]
    [<TestCase(true)>]
    let ``a literal arriving at a block's first instruction is a spill temp, not a literal`` (boundary : bool) : unit =
        // `ldc.r4; ldc.i4.1; X; X; brtrue J; pop; ldc.r8; J: pop; ret`, where `X; X` is
        // `ldloc0; brtrue NEXT` (a conditional branch, after which a block starts) or `nop; nop`.
        let separator : IlOp list =
            if boundary then [ ldloc0 ; brtrue 0 ] else [ nop ; nop ]

        let ops = [ ldcR4 ; ldc ] @ separator @ [ brtrue 0 ; pop ; ldcR8 ; pop ; ret ]

        let branches = (if boundary then [ 3, 4 ] else []) @ [ 4, 7 ]

        let body, at = layOutWithBranches [] ops branches
        let shape = StackShape.analyse (inputs [ other ] [ other ] false) body

        // The branch's fall-through shares J's temp, so its float32 is cast too.
        if boundary then
            shape.Invalid.IsEmpty |> shouldEqual true
            shape.Promotions |> shouldEqual (Map.ofList [ at.[5], [ 0 ] ; at.[7], [ 0 ] ])
        else
            refusalsOf shape |> shouldEqual (Map.ofList [ at.[5], at.[4] ; at.[7], at.[4] ])

    [<Test>]
    let ``a br to the next instruction starts no block, so a literal survives it`` () : unit =
        // `ldc.i4.1; br NEXT; brtrue A; ...`: the JIT merges the two blocks before importing.
        let ops = [ ldc ; br 0 ; brtrue 0 ; ldcR4 ; br 0 ; ldcR8 ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 2 ; 2, 5 ; 4, 6 ]
        let shape = StackShape.analyse (inputs [] [] false) body

        refusalsOf shape |> shouldEqual (Map.ofList [ at.[6], at.[2] ])

    [<Test>]
    let ``a spill clique spans successors of dead conditionals transitively`` () : unit =
        // Dead conditionals with successor pairs (A, C) and (C, B): A and B share a temp through
        // C, which nothing reaches.
        let ops =
            [
                ldloc0 // 0
                brtrue 0 // 1 -> BARM
                ldcR4 // 2
                br 0 // 3 -> A
                ldcR8 // 4 BARM
                br 0 // 5 -> B
                ldloc0 // 6 DEAD1
                brtrue 0 // 7 -> C, falls through to A
                pop // 8 A
                ret // 9
                ldloc0 // 10 DEAD2
                brtrue 0 // 11 -> B, falls through to C
                pop // 12 C
                ret // 13
                pop // 14 B
                ret // 15
            ]

        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 8 ; 5, 14 ; 7, 12 ; 11, 14 ]
        let shape = analyseOrFail (inputs [ other ] [ other ] false) body

        shape.Entry.[at.[8]] |> shouldEqual [ double ]
        shape.Entry.[at.[14]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[12] |> shouldEqual false
        shape.Promotions |> shouldEqual (Map.ofList [ at.[8], [ 0 ] ])

    [<Test>]
    let ``a conflict at an empty join makes its whole component unknown`` () : unit =
        // J is reached empty (`br J`) and, from a later arm, one deep: a conflict at depth zero.
        // K is J's sibling under P and shares its temps, so K is unknown too, and so is M after it.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> P
                br 0 // 2 -> J (empty)
                ldarg0 // 3 P
                brtrue 0 // 4 -> J, falls through to K
                ldcR4 // 5 K
                br 0 // 6 -> M
                ldarg0 // 7 J
                brtrue 0 // 8 -> DEEP
                pop // 9
                ret // 10
                ldcR8 // 11 DEEP
                ldcR8 // 12
                pop // 13
                br 0 // 14 -> J, one deep
                pop // 15 M
                ret // 16
            ]

        let body, at =
            layOutWithBranches [] ops [ 1, 3 ; 2, 7 ; 4, 7 ; 6, 15 ; 8, 11 ; 14, 7 ]

        let shape = StackShape.analyse (inputs [ other ] [] false) body

        shape.Invalid.[at.[7]].IsConflict |> shouldEqual true
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Entry.ContainsKey at.[15] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

    // ---------- Property: the dataflow agrees with simulating each path ----------

    /// Arms that each push some values and then meet at a common join, followed by a
    /// straight-line tail. Every path through this shape is one arm then the tail, so the
    /// expected depth at each tail instruction is computable by hand: run the tail once per arm.
    type private ArmsAndTail =
        {
            /// How many values each arm pushes before branching to the join.
            Arms : int list
            Tail : IlOp list
        }

    let private genTailOp : Gen<IlOp> =
        Gen.elements [ ldc ; pop ; dup ; add ; nop ; ldarg0 ]

    let private genArmsAndTail : Gen<ArmsAndTail> =
        gen {
            let! armCount = Gen.choose (1, 4)
            let! arms = Gen.listOfLength armCount (Gen.choose (0, 3))
            let! tailLength = Gen.choose (0, 8)
            let! tail = Gen.listOfLength tailLength genTailOp

            return
                {
                    Arms = arms
                    Tail = tail
                }
        }

    /// The depth on entry to each tail instruction along one path and then on entry to the
    /// `ret` after it, cut short at the first tail instruction that underflows, whose index is
    /// also returned. Computed from ECMA-335's pops and pushes rather than by the analysis under
    /// test.
    let private simulateTail (entry : int) (tail : IlOp list) : int list * int option =
        let step (depth : int) (op : IlOp) : int option =
            match op with
            | IlOp.Nullary NullaryIlOp.LdcI4_1
            | IlOp.Nullary NullaryIlOp.LdArg0 -> Some (depth + 1)
            | IlOp.Nullary NullaryIlOp.Nop -> Some depth
            | IlOp.Nullary NullaryIlOp.Pop -> if depth >= 1 then Some (depth - 1) else None
            | IlOp.Nullary NullaryIlOp.Dup -> if depth >= 1 then Some (depth + 1) else None
            | IlOp.Nullary NullaryIlOp.Add -> if depth >= 2 then Some (depth - 1) else None
            | other -> failwith $"unexpected tail op %O{other}"

        let rec go (depth : int) (index : int) (ops : IlOp list) (acc : int list) : int list * int option =
            match ops with
            | [] -> List.rev (depth :: acc), None
            | op :: rest ->
                match step depth op with
                | None -> List.rev (depth :: acc), Some index
                | Some next -> go next (index + 1) rest (depth :: acc)

        go entry 0 tail []

    [<Test>]
    let ``the depth at every tail instruction is the one every arm delivers, or the join is a conflict`` () : unit =
        let property (case : ArmsAndTail) : unit =
            // `ldarg0; brtrue A1; ldarg0; brtrue A2; ...; arm0; br J; A1: arm1; br J; ...; J: tail; ret`
            let armCount = case.Arms.Length

            let prefix = [ 1 .. armCount - 1 ] |> List.collect (fun _ -> [ ldarg0 ; brtrue 0 ])

            let armBodies =
                case.Arms |> List.map (fun pushes -> List.replicate pushes ldc @ [ br 0 ])

            let ops = prefix @ List.concat armBodies @ case.Tail @ [ ret ]

            let armStart (i : int) : int =
                prefix.Length
                + (case.Arms |> List.take i |> List.sumBy (fun pushes -> pushes + 1))

            let joinIndex = armStart armCount

            let branches =
                ([ 1 .. armCount - 1 ] |> List.map (fun i -> 2 * i - 1, armStart i))
                @ ([ 0 .. armCount - 1 ]
                   |> List.map (fun i -> armStart i + case.Arms.[i], joinIndex))

            let body, at = layOutWithBranches [] ops branches
            let shape = StackShape.analyse (inputs [ other ] [] false) body

            shape.Reachable |> shouldEqual (Set.ofSeq at.Values)

            // Each arm's own instructions are typed whatever the join does.
            for i in 0 .. armCount - 1 do
                for k in 0 .. case.Arms.[i] do
                    shape.Entry.[at.[armStart i + k]].Length |> shouldEqual k

            // Tail instruction `k`, with `k = Tail.Length` being the `ret`.
            let tailAt (k : int) : int = at.[joinIndex + k]

            match List.distinct case.Arms with
            | [ entry ] ->
                let depths, underflow = simulateTail entry case.Tail

                depths
                |> List.iteri (fun k depth ->
                    if underflow = Some k then
                        match Map.tryFind (tailAt k) shape.Invalid with
                        | Some (StackShapeError.StackUnderflow (_, _, d)) -> d |> shouldEqual depth
                        | other -> failwith $"expected an underflow at tail instruction %d{k}, got %O{other}"
                    else
                        shape.Entry.[tailAt k].Length |> shouldEqual depth
                )

                match underflow with
                | None -> shape.Invalid.IsEmpty |> shouldEqual true
                | Some k ->
                    // Nothing past the underflow is typed or invalid.
                    for j in k + 1 .. case.Tail.Length do
                        shape.Entry.ContainsKey (tailAt j) |> shouldEqual false
                        shape.Invalid.ContainsKey (tailAt j) |> shouldEqual false
            | _ ->
                shape.Invalid.[at.[joinIndex]].IsConflict |> shouldEqual true

                for k in 1 .. case.Tail.Length do
                    shape.Entry.ContainsKey (tailAt k) |> shouldEqual false
                    shape.Invalid.ContainsKey (tailAt k) |> shouldEqual false

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen genArmsAndTail) property)

    // ---------- Property: the shapes at a join and after it agree with enumerating the paths ----------

    /// Arms that each push one float literal and then meet at a common join, followed by a
    /// straight-line tail of float operations. Every path through this shape is one arm then
    /// the tail, so the expected shape at each tail instruction is computable by hand: run the
    /// tail once per arm and join the results slot by slot.
    type private WidthArmsAndTail =
        {
            Arms : FloatWidth list
            Tail : IlOp list
        }

    let private genWidthTailOp : Gen<IlOp> =
        Gen.elements
            [
                ldcR4
                ldcR8
                add
                IlOp.Nullary NullaryIlOp.Mul
                IlOp.Nullary NullaryIlOp.Conv_R4
                IlOp.Nullary NullaryIlOp.Conv_R8
                IlOp.Nullary NullaryIlOp.Neg
                IlOp.Nullary NullaryIlOp.Dup
            ]

    /// A tail that never underflows, checked by simulating depths.
    let private genWidthArmsAndTail : Gen<WidthArmsAndTail> =
        gen {
            let! armCount = Gen.choose (1, 4)
            let! arms = Gen.listOfLength armCount (Gen.elements [ FloatWidth.Single ; FloatWidth.Double ])
            let! tailLength = Gen.choose (0, 8)
            let! tail = Gen.listOfLength tailLength genWidthTailOp

            let survives =
                (Some 1, tail)
                ||> List.fold (fun depth op ->
                    match depth with
                    | None -> None
                    | Some depth ->
                        match op with
                        | IlOp.UnaryConst _ -> Some (depth + 1)
                        | IlOp.Nullary NullaryIlOp.Dup -> if depth >= 1 then Some (depth + 1) else None
                        | IlOp.Nullary NullaryIlOp.Add
                        | IlOp.Nullary NullaryIlOp.Mul -> if depth >= 2 then Some (depth - 1) else None
                        | _ -> if depth >= 1 then Some depth else None
                )

            return!
                match survives with
                | Some _ ->
                    Gen.constant
                        {
                            Arms = arms
                            Tail = tail
                        }
                | None ->
                    Gen.constant
                        {
                            Arms = arms
                            Tail = []
                        }
        }

    /// One path's stack, top first, at the entry of each tail instruction, computed by the
    /// rules stated in ECMA-335 and the importer rather than by the analysis under test.
    let private pathStacks (arm : FloatWidth) (tail : IlOp list) : SlotShape list list =
        let step (stack : SlotShape list) (op : IlOp) : SlotShape list =
            match op, stack with
            | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 _), _ -> single :: stack
            | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 _), _ -> double :: stack
            | IlOp.Nullary NullaryIlOp.Dup, top :: rest -> top :: top :: rest
            | IlOp.Nullary NullaryIlOp.Conv_R4, _ :: rest -> single :: rest
            | IlOp.Nullary NullaryIlOp.Conv_R8, _ :: rest -> double :: rest
            | IlOp.Nullary NullaryIlOp.Neg, top :: rest -> top :: rest
            | IlOp.Nullary NullaryIlOp.Add, a :: b :: rest
            | IlOp.Nullary NullaryIlOp.Mul, a :: b :: rest ->
                let result =
                    match a, b with
                    | SlotShape.Float FloatWidth.Single, SlotShape.Float FloatWidth.Single -> single
                    | _ -> double

                result :: rest
            | _ -> failwith $"generator produced an underflowing tail: %O{op} on %O{stack}"

        tail |> List.scan step [ SlotShape.Float arm ] |> List.take tail.Length

    let private joinShapes (a : SlotShape) (b : SlotShape) : SlotShape =
        match a, b with
        | SlotShape.Float FloatWidth.Single, SlotShape.Float FloatWidth.Single -> single
        | SlotShape.Float _, SlotShape.Float _ -> double
        | _ -> failwith "the generator only makes floats"

    [<Test>]
    let ``the join of every arm's path is what the analysis reports, and only the join is promoted`` () : unit =
        let property (case : WidthArmsAndTail) : unit =
            // arm i: `ldloc0; ldc.i4 i; beq -> arm i` would need integer comparisons; a chain
            // of `brtrue` on ldloc0 suffices since only the *shapes* matter, and a local is
            // never a constant to the importer, so no branch on it can be folded.
            let armCount = case.Arms.Length

            let prefix =
                [
                    for _ in 1 .. armCount - 1 do
                        yield ldloc0
                        yield brtrue 0
                ]

            let armOps =
                case.Arms
                |> List.mapi (fun i width ->
                    let literal =
                        match width with
                        | FloatWidth.Single -> ldcR4
                        | FloatWidth.Double -> ldcR8

                    if i = armCount - 1 then [ literal ] else [ literal ; br 0 ]
                )

            let tailOps = case.Tail @ [ pop ; ret ]
            let allOps = prefix @ List.concat armOps @ tailOps
            let _, at = layOut [] allOps

            // Offsets of each arm's first instruction and of the join.
            let armStart (i : int) : int =
                let before = prefix.Length + (armOps |> List.take i |> List.sumBy List.length)
                at.[before]

            let joinIndex = prefix.Length + (armOps |> List.sumBy List.length)
            let join = at.[joinIndex]

            let allOps =
                let mutable branchSeen = 0

                allOps
                |> List.mapi (fun i op ->
                    match op with
                    | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _) when i < prefix.Length ->
                        branchSeen <- branchSeen + 1
                        // The k-th brtrue (1-based) jumps to arm k; the fall-through after the last
                        // is arm 0... keep it simple: brtrue k -> arm k, fall-through -> arm 0.
                        brtrue (armStart branchSeen)
                    | IlOp.UnaryConst (UnaryConstIlOp.Br _) -> br join
                    | other -> other
                )

            let body, at = layOut [] allOps
            let shape = analyseOrFail (inputs [ other ] [ other ] false) body

            let expectedAtTail =
                case.Arms
                |> List.map (fun arm -> pathStacks arm case.Tail)
                |> List.reduce (fun a b -> List.map2 (List.map2 joinShapes) a b)

            for i in 0 .. case.Tail.Length - 1 do
                shape.Entry.[at.[joinIndex + i]] |> shouldEqual expectedAtTail.[i]

            let mixed =
                case.Arms |> List.exists ((=) FloatWidth.Single)
                && case.Arms |> List.exists ((=) FloatWidth.Double)

            // Only the join itself is a promotion point: after it every path carries the same
            // shapes, so nothing downstream ever sees a float32 where a double is expected.
            let expectedPromotions = if mixed then Map.ofList [ join, [ 0 ] ] else Map.empty

            shape.Promotions |> shouldEqual expectedPromotions

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen genWidthArmsAndTail) property)

    // ---------- Property: every typed answer is the one the folded program gets ----------

    /// One statement of a generated body, each leaving one float on the stack. A branch names
    /// its target by statement index.
    [<RequireQualifiedAccess>]
    type private Statement =
        /// `conv.r4`.
        | ToSingle
        /// `conv.r8`.
        | ToDouble
        /// `ldc.i4.0; brtrue` or `ldc.i4.1; brtrue`, which the JIT may fold.
        | LiteralBranch of taken : bool * target : int
        /// `call; brtrue` on a callee with no arguments, such as an `IsSupported` intrinsic, which
        /// the JIT may fold; `taken` is the constant it folds to.
        | CallBranch of taken : bool * target : int
        /// `ldarg.0; brtrue`, which the JIT folds when it inlines the body into a caller passing a
        /// constant; `taken` is the constant.
        | ArgumentBranch of taken : bool * target : int
        /// `ldloc.0; brtrue`, which no JIT folds.
        | RuntimeBranch of target : int
        | Jump of target : int
        | Return

    let private genStatements : Gen<Statement list> =
        gen {
            let! count = Gen.choose (2, 12)

            // Mostly forward branches; some backward, for loops.
            let genTarget (from : int) : Gen<int> =
                Gen.frequency [ 4, Gen.choose (min (from + 1) count, count) ; 1, Gen.choose (0, from) ]

            let genStatement (index : int) : Gen<Statement> =
                Gen.frequency
                    [
                        3, Gen.constant Statement.ToSingle
                        3, Gen.constant Statement.ToDouble
                        3,
                        Gen.map2
                            (fun taken target -> Statement.LiteralBranch (taken, target))
                            (Gen.elements [ true ; false ])
                            (genTarget index)
                        2,
                        Gen.map2
                            (fun taken target -> Statement.CallBranch (taken, target))
                            (Gen.elements [ true ; false ])
                            (genTarget index)
                        2,
                        Gen.map2
                            (fun taken target -> Statement.ArgumentBranch (taken, target))
                            (Gen.elements [ true ; false ])
                            (genTarget index)
                        3, Gen.map Statement.RuntimeBranch (genTarget index)
                        2, Gen.map Statement.Jump (genTarget index)
                        1, Gen.constant Statement.Return
                    ]

            let! statements = [ 1 .. count - 1 ] |> List.map genStatement |> Gen.sequenceToList
            return Statement.ToSingle :: statements @ [ Statement.Return ]
        }

    /// Lay out the statements, rewriting to `nop`s and a `br` (the target, or the next
    /// instruction) each foldable branch in `folded`: the program the JIT imports once it has
    /// folded them. Every statement occupies the same bytes either way. Also returns each
    /// statement's first offset, and the token shapes of the calls laid out.
    let private layOutStatements
        (folded : Set<int>)
        (statements : Statement list)
        : MethodInstructions<TypeDefn> * Map<int, int> * Map<int, TokenShape>
        =
        let opsOf (index : int) (statement : Statement) : IlOp list =
            match statement with
            // The body's first statement has nothing to convert, so pushes instead.
            | Statement.ToSingle when index = 0 -> [ ldcR4 ]
            | Statement.ToSingle -> [ IlOp.Nullary NullaryIlOp.Conv_R4 ]
            | Statement.ToDouble -> [ IlOp.Nullary NullaryIlOp.Conv_R8 ]
            | Statement.LiteralBranch (taken, _) when folded.Contains index ->
                ignore taken
                [ nop ; br 0 ]
            | Statement.LiteralBranch (taken, _) ->
                [ (if taken then ldc else IlOp.Nullary NullaryIlOp.LdcI4_0) ; brtrue 0 ]
            | Statement.CallBranch _ when folded.Contains index -> List.replicate 5 nop @ [ br 0 ]
            | Statement.CallBranch _ -> [ IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, callToken) ; brtrue 0 ]
            | Statement.ArgumentBranch _ when folded.Contains index -> [ nop ; br 0 ]
            | Statement.ArgumentBranch _ -> [ ldarg0 ; brtrue 0 ]
            | Statement.RuntimeBranch _ -> [ ldloc0 ; brtrue 0 ]
            | Statement.Jump _ -> [ br 0 ]
            | Statement.Return -> [ ret ]

        let perStatement = statements |> List.mapi opsOf
        let firstOp = perStatement |> List.scan (fun acc ops -> acc + List.length ops) 0
        let ops = List.concat perStatement

        // Every branching statement ends in its branch.
        let branchOp (i : int) : int = firstOp.[i + 1] - 1

        let branches =
            statements
            |> List.indexed
            |> List.choose (fun (i, statement) ->
                match statement with
                | Statement.LiteralBranch (taken, target)
                | Statement.CallBranch (taken, target)
                | Statement.ArgumentBranch (taken, target) when folded.Contains i ->
                    Some (branchOp i, (if taken then firstOp.[target] else firstOp.[i + 1]))
                | Statement.LiteralBranch (_, target)
                | Statement.CallBranch (_, target)
                | Statement.ArgumentBranch (_, target)
                | Statement.RuntimeBranch target
                | Statement.Jump target -> Some (branchOp i, firstOp.[target])
                | _ -> None
            )

        let body, at = layOutWithBranches [] ops branches

        let tokens =
            statements
            |> List.indexed
            |> List.choose (fun (i, statement) ->
                match statement with
                | Statement.CallBranch _ when not (folded.Contains i) ->
                    Some (at.[firstOp.[i]], TokenShape.Callee (0, Some other))
                | _ -> None
            )
            |> Map.ofList

        body, (statements |> List.mapi (fun i _ -> i, at.[firstOp.[i]]) |> Map.ofList), tokens

    /// The foldable branches the JIT imports: those reached from the entry when every foldable
    /// branch it imports goes only the way its constant says.
    let private importedFoldableBranches (statements : Statement list) : Set<int> =
        let statements = Array.ofList statements

        let rec walk (seen : Set<int>) (pending : int list) : Set<int> =
            match pending with
            | [] -> seen
            | i :: rest when seen.Contains i || i >= statements.Length -> walk seen rest
            | i :: rest ->
                let next =
                    match statements.[i] with
                    | Statement.LiteralBranch (true, target)
                    | Statement.CallBranch (true, target)
                    | Statement.ArgumentBranch (true, target) -> [ target ]
                    | Statement.LiteralBranch (false, _)
                    | Statement.CallBranch (false, _)
                    | Statement.ArgumentBranch (false, _) -> [ i + 1 ]
                    | Statement.RuntimeBranch target -> [ target ; i + 1 ]
                    | Statement.Jump target -> [ target ]
                    | Statement.Return -> []
                    | Statement.ToSingle
                    | Statement.ToDouble -> [ i + 1 ]

                walk (Set.add i seen) (next @ rest)

        walk Set.empty [ 0 ]
        |> Set.filter (fun i ->
            match statements.[i] with
            | Statement.LiteralBranch _
            | Statement.CallBranch _
            | Statement.ArgumentBranch _ -> true
            | _ -> false
        )

    [<Test>]
    let ``every join the analysis types has the shape and promotion the folded program gets`` () : unit =
        let mutable refusals = 0
        let mutable callRefusals = 0
        let mutable argumentRefusals = 0
        let mutable comparedPromotions = 0

        let property (statements : Statement list) : unit =
            // Offsets are compared at statement starts only: within a rewritten foldable branch the
            // folded program has not pushed the condition.
            let body, starts, tokens = layOutStatements Set.empty statements

            let shape =
                StackShape.analyse
                    { inputs [ other ] [ other ] false with
                        Tokens = tokens
                    }
                    body

            let foldedBody, _, foldedTokens =
                layOutStatements (importedFoldableBranches statements) statements

            let folded =
                StackShape.analyse
                    { inputs [ other ] [ other ] false with
                        Tokens = foldedTokens
                    }
                    foldedBody

            // Debuggable code imports every arm, which is the analysis's own graph: the refusals
            // aside, it answers as it would with no literal anywhere.
            let refused =
                shape.Invalid
                |> Map.filter (fun _ error ->
                    match error with
                    | StackShapeError.WidthDependsOnFoldedBranch _ -> true
                    | _ -> false
                )

            refusals <- refusals + refused.Count

            // A call branch's `brtrue` follows its 5-byte `call`.
            let callBranches =
                statements
                |> List.indexed
                |> List.choose (fun (i, statement) ->
                    match statement with
                    | Statement.CallBranch _ -> Some (starts.[i] + 5)
                    | _ -> None
                )
                |> Set.ofList

            // An argument branch's `brtrue` follows its 1-byte `ldarg.0`.
            let argumentBranches =
                statements
                |> List.indexed
                |> List.choose (fun (i, statement) ->
                    match statement with
                    | Statement.ArgumentBranch _ -> Some (starts.[i] + 1)
                    | _ -> None
                )
                |> Set.ofList

            for KeyValue (_, error) in refused do
                match error with
                | StackShapeError.WidthDependsOnFoldedBranch (_, branch) when callBranches.Contains branch ->
                    callRefusals <- callRefusals + 1
                | StackShapeError.WidthDependsOnFoldedBranch (_, branch) when argumentBranches.Contains branch ->
                    argumentRefusals <- argumentRefusals + 1
                | _ -> ()

            for offset in starts.Values do
                match Map.tryFind offset shape.Entry, Map.tryFind offset folded.Entry with
                | None, _
                | _, None -> ()
                | Some entry, Some foldedEntry ->
                    entry |> shouldEqual foldedEntry

                    let promotion = Map.tryFind offset shape.Promotions

                    if promotion.IsSome then
                        comparedPromotions <- comparedPromotions + 1

                    promotion |> shouldEqual (Map.tryFind offset folded.Promotions)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen genStatements) property)

        // The generator reaches both kinds of join the property is about.
        refusals |> shouldBeGreaterThan 0
        callRefusals |> shouldBeGreaterThan 0
        argumentRefusals |> shouldBeGreaterThan 0
        comparedPromotions |> shouldBeGreaterThan 0

    // ---------- The interpreter's refusal of a float32 entering an untyped block ----------

    [<Test>]
    let ``a float32 entering a block the analysis could not type is refused, and nothing else is`` () : unit =
        // `ldarg0; brtrue A; ldc.i4.1; br J; A: ldc.r4; J: pop; ret`: J joins an int32 with a
        // float32, a conflict, so J and what follows it are untyped. J starts a block, so a value
        // on the stack there arrives in a spill temp whose width CoreCLR decides over paths the
        // analysis could not type; `ret` starts none.
        let ops = [ ldarg0 ; brtrue 0 ; ldc ; br 0 ; ldcR4 ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 5 ]
        let shape = StackShape.analyse (inputs [ other ] [] false) body

        shape.Invalid.[at.[5]].IsConflict |> shouldEqual true

        let singleValue = EvalStackValue.Float (EvalStackFloat.Single 1.0f)
        let doubleValue = EvalStackValue.Float (EvalStackFloat.Double 1.0)
        let intValue = EvalStackValue.Int32 (Int32Source.Verbatim 1)

        let refusedSlot (offset : int) (stack : EvalStackValue list) : int option =
            StackShapeOfMethod.untypedSpilledSingle shape offset stack

        refusedSlot at.[5] [ singleValue ] |> shouldEqual (Some 0)
        refusedSlot at.[5] [ intValue ; singleValue ] |> shouldEqual (Some 1)
        refusedSlot at.[5] [ doubleValue ] |> shouldEqual None
        refusedSlot at.[5] [ intValue ] |> shouldEqual None
        // Untyped, but no block starts there: nothing is spilled.
        shape.Entry.ContainsKey at.[6] |> shouldEqual false
        refusedSlot at.[6] [ singleValue ] |> shouldEqual None
        // Typed block starts are the analysis's to decide.
        refusedSlot at.[4] [ singleValue ] |> shouldEqual None

    [<Test>]
    let ``the block starts are the entry, every branch target, and every conditional's fall-through`` () : unit =
        let ops = [ ldarg0 ; brtrue 0 ; ldc ; br 0 ; ldcR4 ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 5 ]
        let shape = StackShape.analyse (inputs [ other ] [] false) body

        shape.BlockStarts
        |> shouldEqual (Set.ofList [ at.[0] ; at.[2] ; at.[4] ; at.[5] ])

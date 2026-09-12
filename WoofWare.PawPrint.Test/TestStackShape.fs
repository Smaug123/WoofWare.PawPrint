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
            Mode = CompilationMode.Tier0
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
                IlOp.Nullary NullaryIlOp.LdArg0 // 0
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
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Entry.[at.[6]] |> shouldEqual [ single ; double ]
        shape.Entry.[at.[7]] |> shouldEqual [ double ]
        shape.Entry.[at.[9]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[5], [ 0 ] ])

    [<Test>]
    let ``the arm order does not matter`` () : unit =
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdArg0 // 0
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
        let shape = analyseOrFail (inputs [ other ] [] false) body

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
                IlOp.Nullary NullaryIlOp.LdArg0 // 0
                brtrue 0 // 1 -> D
                ldcR4 // 2  A
                IlOp.Nullary NullaryIlOp.LdArg0 // 3
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
        let shape = analyseOrFail (inputs [ other ] [] false) body

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
                IlOp.Nullary NullaryIlOp.LdArg0 // 0
                brtrue 0 // 1 -> L1
                ldcR4 // 2
                br 0 // 3 -> M2
                IlOp.Nullary NullaryIlOp.LdArg0 // 4  L1
                brtrue 0 // 5 -> L2
                ldcR8 // 6
                br 0 // 7 -> M1
                ldcR4 // 8  L2
                IlOp.Nullary NullaryIlOp.LdArg0 // 9
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
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[11]] |> shouldEqual [ double ]
        shape.Entry.[at.[12]] |> shouldEqual [ double ]
        shape.Entry.[at.[14]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[11], [ 0 ] ; at.[14], [ 0 ] ])

    [<Test>]
    let ``a branch on a constant of its block delivers only the arm the importer imports`` () : unit =
        // `ldc.i4.1; brtrue L; br J; L: nop; nop; ldc.i4.1; br J; J: nop; ret`: the fall-through
        // arm is dead to an optimising importer, so J is reached only with one value and the
        // method returns 1. Nothing here may be invalid.
        let nop = IlOp.Nullary NullaryIlOp.Nop

        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdcI4_1 // 0
                brtrue 0 // 1 -> L
                br 0 // 2 -> J
                nop // 3 L
                nop // 4
                IlOp.Nullary NullaryIlOp.LdcI4_1 // 5
                br 0 // 6 -> J
                nop // 7 J
                ret // 8
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 1 -> brtrue at.[3]
                | 2
                | 6 -> br at.[7]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [] [] true) body

        shape.Entry.ContainsKey at.[2] |> shouldEqual false
        shape.Entry.[at.[7]] |> shouldEqual [ other ]
        shape.Entry.[at.[8]] |> shouldEqual [ other ]

    [<Test>]
    let ``an arm the importer folds away contributes no promotion`` () : unit =
        // `ldc.i4.0; brtrue DOUBLE; ldarg0 (float32); br JOIN; DOUBLE: ldc.r8; JOIN: ...`: the
        // double arm is never imported, so the join is single and nothing is widened.
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdcI4_0 // 0
                brtrue 0 // 1 -> DOUBLE
                IlOp.Nullary NullaryIlOp.LdArg0 // 2
                br 0 // 3 -> JOIN
                ldcR8 // 4 DOUBLE
                ldcR4 // 5 JOIN
                add // 6
                pop // 7
                ret // 8
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
        let shape = analyseOrFail (inputs [ single ] [] false) body

        shape.Entry.[at.[5]] |> shouldEqual [ single ]
        shape.Entry.[at.[6]] |> shouldEqual [ single ; single ]
        shape.Entry.ContainsKey at.[4] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a constant does not survive a block boundary`` () : unit =
        // The same shape as above, but the condition arrives at the branch from another block
        // (a `br` over a `nop`, so COND is a branch target), so it is a spill temp to the
        // importer and both arms are imported.
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdcI4_0 // 0
                br 0 // 1 -> COND
                nop // 2
                brtrue 0 // 3 COND -> DOUBLE (a branch target, so a block start)
                IlOp.Nullary NullaryIlOp.LdArg0 // 4
                br 0 // 5 -> JOIN
                ldcR8 // 6 DOUBLE
                pop // 7 JOIN
                ret // 8
            ]

        let body, at = layOutWithBranches [] ops [ 1, 3 ; 3, 6 ; 5, 7 ]
        let shape = analyseOrFail (inputs [ single ] [] false) body

        shape.Entry.[at.[7]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[7], [ 0 ] ])

    [<Test>]
    let ``a br to the next instruction is no block boundary`` () : unit =
        // The JIT merges the two blocks before importing (`DoEarlyBlockMerging`), so the
        // constant reaches the branch and only the float32 arm is imported.
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdcI4_0 // 0
                br 0 // 1 -> COND, the next instruction
                brtrue 0 // 2 COND -> DOUBLE
                IlOp.Nullary NullaryIlOp.LdArg0 // 3
                br 0 // 4 -> JOIN
                ldcR8 // 5 DOUBLE
                pop // 6 JOIN
                ret // 7
            ]

        let body, at = layOutWithBranches [] ops [ 1, 2 ; 2, 5 ; 4, 6 ]
        let shape = analyseOrFail (inputs [ single ] [] false) body

        shape.Entry.[at.[6]] |> shouldEqual [ single ]
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a body compiled without optimisation folds no branch`` () : unit =
        // Under `DisableOptimizations` or `NoOptimization` the JIT imports both arms of a
        // branch on a literal, and the join is typed over both.
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdcI4_0 // 0
                brtrue 0 // 1 -> DOUBLE
                IlOp.Nullary NullaryIlOp.LdArg0 // 2
                br 0 // 3 -> JOIN
                ldcR8 // 4 DOUBLE
                pop // 5 JOIN
                ret // 6
            ]

        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 5 ]

        let shape =
            analyseOrFail
                { inputs [ single ] [] false with
                    Mode = CompilationMode.Unoptimised
                }
                body

        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Entry.[at.[4]] |> shouldEqual []
        shape.Promotions |> shouldEqual (Map.ofList [ at.[5], [ 0 ] ])

    [<Test>]
    let ``the compilation mode follows the assembly's stamp and the method's flags`` () : unit =
        // CoreLib ships optimised; the test assembly is a Debug build, whose `DebuggableAttribute`
        // asks for `DisableOptimizations`. A dynamic method is fully optimised or not at all.
        StackShapeTokens.dynamicCompilationModeOf (corelib ())
        |> shouldEqual CompilationMode.FullyOptimised

        let test =
            Assembly.readFile (LoggerFactory.makeTest () |> snd) typeof<RunResult>.Assembly.Location

        StackShapeTokens.dynamicCompilationModeOf test
        |> shouldEqual CompilationMode.Unoptimised

        // A guest compiled by the test harness is a Debug build too: its emitted methods run
        // unfolded on real .NET, and the analysis must say so. A method of an optimised
        // assembly is compiled at Tier-0, unless it asks for full optimisation or none.
        let compiled (source : string) : DumpedAssembly =
            use image = new System.IO.MemoryStream (Roslyn.compile [ source ])
            Assembly.read (LoggerFactory.makeTest () |> snd) None image

        StackShapeTokens.dynamicCompilationModeOf (
            compiled "public class Program { public static int Main() { return 0; } }"
        )
        |> shouldEqual CompilationMode.Unoptimised

        let methodNamed
            (assembly : DumpedAssembly)
            (name : string)
            : System.Reflection.Metadata.MethodDefinitionHandle
            =
            assembly.Methods
            |> Seq.pick (fun (KeyValue (handle, definition)) -> if definition.Name = name then Some handle else None)

        let release =
            compiled (
                "[assembly: System.Diagnostics.Debuggable(System.Diagnostics.DebuggableAttribute.DebuggingModes.IgnoreSymbolStoreSequencePoints)]\n"
                + "public class Program { public static int Main() { return 0; } "
                + "[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoOptimization)] public static int Slow() { return 1; } "
                + "[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveOptimization)] public static int Fast() { return 2; } }"
            )

        StackShapeTokens.compilationModeOf release (methodNamed release "Main")
        |> shouldEqual CompilationMode.Tier0

        StackShapeTokens.compilationModeOf release (methodNamed release "Slow")
        |> shouldEqual CompilationMode.Unoptimised

        StackShapeTokens.compilationModeOf release (methodNamed release "Fast")
        |> shouldEqual CompilationMode.FullyOptimised

    [<Test>]
    let ``a constant below a conditional branch does not survive the fall-through`` () : unit =
        // `ldc.i4.5; ldarg0; brtrue T; brtrue U; ldc.r4; br J; T: pop; ret; U: ldc.r8; J: pop; ret`.
        // The importer ends a block at the first `brtrue`, spilling the 5 to a temp, so the
        // second `brtrue` is not folded: both its arms are imported and J is a mixed join.
        let ops =
            [
                IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 5) // 0
                IlOp.Nullary NullaryIlOp.LdArg0 // 1
                brtrue 0 // 2 -> T
                brtrue 0 // 3 -> U
                ldcR4 // 4
                br 0 // 5 -> J
                pop // 6 T
                ret // 7
                ldcR8 // 8 U
                pop // 9 J
                ret // 10
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 2 -> brtrue at.[6]
                | 3 -> brtrue at.[8]
                | 5 -> br at.[9]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[4]] |> shouldEqual []
        shape.Entry.[at.[9]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ at.[9], [ 0 ] ])

    [<Test>]
    let ``a folded branch stays folded when its block is revisited`` () : unit =
        // `ldarg0 (float32); HEAD: ldc.i4.0; brtrue DEAD; ldarg1; brtrue BACK; TAIL: pop; ret;
        // BACK: pop; ldc.r8; br HEAD; DEAD: ldc.r8; br TAIL`. The back edge widens HEAD's slot
        // after HEAD's block has been visited, so the block is visited again; its branch is on
        // its own constant both times, and DEAD is never delivered (it would reach TAIL two deep).
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdArg0 // 0
                IlOp.Nullary NullaryIlOp.LdcI4_0 // 1 HEAD
                brtrue 0 // 2 -> DEAD
                IlOp.Nullary NullaryIlOp.LdArg1 // 3
                brtrue 0 // 4 -> BACK
                pop // 5 TAIL
                ret // 6
                pop // 7 BACK
                ldcR8 // 8
                br 0 // 9 -> HEAD
                ldcR8 // 10 DEAD
                br 0 // 11 -> TAIL
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 2 -> brtrue at.[10]
                | 4 -> brtrue at.[7]
                | 9 -> br at.[1]
                | 11 -> br at.[5]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [ single ; other ] [] false) body

        shape.Entry.[at.[1]] |> shouldEqual [ double ]
        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[10] |> shouldEqual false
        shape.Promotions |> shouldEqual (Map.ofList [ at.[1], [ 0 ] ])

    /// `setup; brtrue TAKEN; ldc.r4; br JOIN; TAKEN: ldc.r8; JOIN: pop; ret`: when `setup`
    /// leaves a constant of the block on the stack, only one arm reaches JOIN, and its width
    /// says which. An argument arrives at JOIN as well only when the branch is not folded.
    /// `setup; brtrue TAKEN; ldc.r4; br JOIN; TAKEN: ldc.r8; JOIN: pop; ret`: when `setup`
    /// leaves a constant of the block on the stack, only one arm reaches JOIN, and its width
    /// says which. An argument arrives at JOIN as well only when the branch is not folded.
    let private armReached (setup : IlOp list) : StackShape * int =
        let n = setup.Length

        let ops =
            setup
            @ [
                brtrue 0 // n -> TAKEN
                ldcR4 // n+1
                br 0 // n+2 -> JOIN
                ldcR8 // n+3 TAKEN
                pop // n+4 JOIN
                ret // n+5
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                if i = n then brtrue at.[n + 3]
                elif i = n + 2 then br at.[n + 4]
                else op
            )

        let body, at = layOut [] ops
        analyseOrFail (inputs [] [] false) body, at.[n + 4]

    /// `armReached` under another compilation mode, with arguments for `setup` to load.
    let private armReachedFrom
        (mode : CompilationMode)
        (arguments : SlotShape list)
        (setup : IlOp list)
        : StackShape * int
        =
        let n = setup.Length

        let ops =
            setup
            @ [
                brtrue 0 // n -> TAKEN
                ldcR4 // n+1
                br 0 // n+2 -> JOIN
                ldcR8 // n+3 TAKEN
                pop // n+4 JOIN
                ret // n+5
            ]

        let body, at = layOutWithBranches [] ops [ n, n + 3 ; n + 2, n + 4 ]

        analyseOrFail
            { inputs arguments [] false with
                Mode = mode
            }
            body,
        at.[n + 4]

    /// `armReached` under another compilation mode.
    let private armReachedIn (mode : CompilationMode) (setup : IlOp list) : StackShape * int =
        armReachedFrom mode [] setup

    let private ldcI4Of (v : int) : IlOp =
        IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 v)

    let private ldcI8Of (v : int64) : IlOp =
        IlOp.UnaryConst (UnaryConstIlOp.Ldc_I8 v)

    [<TestCase(true)>]
    [<TestCase(false)>]
    let ``a shift of literals is a constant of the block, with the count masked to the width`` (taken : bool) : unit =
        let shl = IlOp.Nullary NullaryIlOp.Shl
        let shr = IlOp.Nullary NullaryIlOp.Shr
        let shrUn = IlOp.Nullary NullaryIlOp.Shr_un

        let cases =
            [
                // 1 << (33 & 31) = 2
                [ ldcI4Of 1 ; ldcI4Of 33 ; shl ], true
                // 1 >> (32 & 31) = 1; an unmasked count would give 0
                [ ldcI4Of 1 ; ldcI4Of 32 ; shr ], true
                [ ldcI4Of 1 ; ldcI4Of 1 ; shr ], false
                // -1 >>> 31 = 1
                [ ldcI4Of -1 ; ldcI4Of 31 ; shrUn ], true
                [ ldcI4Of 0x40000000 ; ldcI4Of 2 ; shl ], false
                // 1L << (65 & 63) = 2L
                [ ldcI8Of 1L ; ldcI4Of 65 ; shl ], true
                [ ldcI8Of 1L ; ldcI4Of 1 ; shr ], false
                // -1L >>> 63 = 1L
                [ ldcI8Of -1L ; ldcI4Of 63 ; shrUn ], true
                [ ldcI8Of 0x4000000000000000L ; ldcI4Of 2 ; shl ], false
            ]
            |> List.filter (fun (_, t) -> t = taken)

        cases |> List.isEmpty |> shouldEqual false

        for setup, _ in cases do
            let shape, join = armReached setup
            let expected = if taken then double else single
            shape.Entry.[join] |> shouldEqual [ expected ]
            shape.Promotions |> shouldEqual Map.empty

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

    [<TestCase(true)>]
    [<TestCase(false)>]
    let ``checked arithmetic folds exactly when it does not overflow, and unsigned division keeps the JIT's exclusion``
        (folded : bool)
        : unit
        =
        let addOvf = IlOp.Nullary NullaryIlOp.Add_ovf
        let subOvf = IlOp.Nullary NullaryIlOp.Sub_ovf
        let mulOvf = IlOp.Nullary NullaryIlOp.Mul_ovf
        let addOvfUn = IlOp.Nullary NullaryIlOp.Add_ovf_un
        let divUn = IlOp.Nullary NullaryIlOp.Div_un
        let remUn = IlOp.Nullary NullaryIlOp.Rem_un

        // Each case: the literals, whether the analysis folds them, and if so whether the
        // result is non-zero (the branch taken).
        let cases =
            [
                [ ldcI4Of 1 ; ldcI4Of 1 ; subOvf ], Some false
                [ ldcI4Of 2 ; ldcI4Of 3 ; addOvf ], Some true
                [ ldcI4Of System.Int32.MaxValue ; ldcI4Of 1 ; addOvf ], None
                [ ldcI4Of 0x10000 ; ldcI4Of 0x10000 ; mulOvf ], None
                [ ldcI4Of -1 ; ldcI4Of 1 ; addOvfUn ], None // 0xFFFFFFFF + 1 overflows unsigned
                [ ldcI4Of 1 ; ldcI4Of 1 ; addOvfUn ], Some true
                [ ldcI8Of System.Int64.MaxValue ; ldcI8Of 1L ; addOvf ], None
                [ ldcI8Of 1L ; ldcI8Of 1L ; subOvf ], Some false
                [ ldcI4Of System.Int32.MinValue ; ldcI4Of -1 ; divUn ], None
                [ ldcI4Of System.Int32.MinValue ; ldcI4Of -1 ; remUn ], None
                [ ldcI4Of 7 ; ldcI4Of 2 ; divUn ], Some true
                [ ldcI8Of System.Int64.MinValue ; ldcI8Of -1L ; divUn ], None
                [ ldcI4Of 0 ; IlOp.Nullary NullaryIlOp.Conv_ovf_u1 ], Some false
                [ ldcI4Of 200 ; IlOp.Nullary NullaryIlOp.Conv_ovf_u1 ], Some true
                [ ldcI4Of 300 ; IlOp.Nullary NullaryIlOp.Conv_ovf_u1 ], None
                [ ldcI4Of -1 ; IlOp.Nullary NullaryIlOp.Conv_ovf_u4 ], None
                [ ldcI4Of -1 ; IlOp.Nullary NullaryIlOp.Conv_ovf_u4_un ], Some true // 0xFFFFFFFF fits
                [ ldcI4Of -1 ; IlOp.Nullary NullaryIlOp.Conv_ovf_i8_un ], Some true
                [ ldcI8Of 0x100000000L ; IlOp.Nullary NullaryIlOp.Conv_ovf_i4 ], None
                [ ldcI8Of 0L ; IlOp.Nullary NullaryIlOp.Conv_ovf_i2 ], Some false
            ]
            |> List.filter (fun (_, outcome) -> outcome.IsSome = folded)

        cases |> List.isEmpty |> shouldEqual false

        for setup, outcome in cases do
            let shape, join = armReached setup

            match outcome with
            | Some true ->
                shape.Entry.[join] |> shouldEqual [ double ]
                shape.Promotions |> shouldEqual Map.empty
            | Some false ->
                shape.Entry.[join] |> shouldEqual [ single ]
                shape.Promotions |> shouldEqual Map.empty
            | None ->
                // Both arms are imported: the join is double and the float32 arm is cast.
                shape.Entry.[join] |> shouldEqual [ double ]
                shape.Promotions |> shouldEqual (Map.ofList [ join, [ 0 ] ])

    [<Test>]
    let ``the two successors of a dead conditional share a temp, so a live single meets a live double`` () : unit =
        // `ldarg0; brtrue C_ARM; ldc.r4; br B; C_ARM: ldc.r8; br C; DEAD: ldarg0; brtrue C; B: pop;
        // ret; C: pop; ret`: nothing reaches DEAD, but the JIT walks its two successors B and C
        // into one spill clique, so B's float32 is a double, and cast on arrival.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> C_ARM
                ldcR4 // 2
                br 0 // 3 -> B
                ldcR8 // 4 C_ARM
                br 0 // 5 -> C
                ldarg0 // 6 DEAD
                brtrue 0 // 7 -> C, falls through to B
                pop // 8 B
                ret // 9
                pop // 10 C
                ret // 11
            ]

        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 8 ; 5, 10 ; 7, 10 ]
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[8]] |> shouldEqual [ double ]
        shape.Entry.[at.[10]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[6] |> shouldEqual false
        shape.Promotions |> shouldEqual (Map.ofList [ at.[8], [ 0 ] ])

    [<Test>]
    let ``an int32 constant meeting a native-int constant is widened before folding`` () : unit =
        let convI = IlOp.Nullary NullaryIlOp.Conv_I
        let convU = IlOp.Nullary NullaryIlOp.Conv_U
        let ceq = IlOp.Nullary NullaryIlOp.Ceq
        let cgtUn = IlOp.Nullary NullaryIlOp.Cgt_un

        // A comparison retypes the constant, sign-extended, at any tier.
        let comparisons =
            [
                [ ldcI4Of 0 ; convI ; ldcI4Of 0 ; ceq ], true
                [ ldcI4Of -1 ; convI ; ldcI4Of -1 ; ceq ], true // sign-extended: equal
                [ ldcI4Of -1 ; convU ; ldcI4Of -1 ; ceq ], false // 0xFFFFFFFF against -1 widened
                [ ldcI4Of 1 ; convI ; ldcI4Of -1 ; cgtUn ], false // 1 > 0xFFFF...FFFF unsigned: no
                [ ldcI4Of -1 ; convI ; ldcI4Of -1 ; cgtUn ], false // retyped, not cast: equal
            ]

        for setup, taken in comparisons do
            let shape, join = armReached setup
            shape.Entry.[join] |> shouldEqual [ (if taken then double else single) ]
            shape.Promotions |> shouldEqual Map.empty

        // Arithmetic casts the constant, and folds the cast only when fully optimised: at
        // Tier-0 the sum is no constant and both arms are imported.
        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ ldcI4Of 1 ; convI ; ldcI4Of -1 ; add ]

        shape.Entry.[join] |> shouldEqual [ single ]
        shape.Promotions |> shouldEqual Map.empty

        let shape, join =
            armReachedIn CompilationMode.Tier0 [ ldcI4Of 1 ; convI ; ldcI4Of -1 ; add ]

        shape.Entry.[join] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ join, [ 0 ] ])

        // The cast is zero-extending for an unsigned operation: 1 (native) / 0xFFFFFFFF = 0.
        let shape, join =
            armReachedIn
                CompilationMode.FullyOptimised
                [ ldcI4Of 1 ; convI ; ldcI4Of -1 ; IlOp.Nullary NullaryIlOp.Div_un ]

        shape.Entry.[join] |> shouldEqual [ single ]
        shape.Promotions |> shouldEqual Map.empty

        // A conditional branch compares the widened operands itself: `1 (native) blt 2` is taken.
        let ops =
            [
                ldcI4Of 1 // 0
                convI // 1
                ldcI4Of 2 // 2
                IlOp.UnaryConst (UnaryConstIlOp.Blt 0) // 3 -> TAKEN
                ldcR4 // 4
                br 0 // 5 -> JOIN
                ldcR8 // 6 TAKEN
                pop // 7 JOIN
                ret // 8
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 3 -> IlOp.UnaryConst (UnaryConstIlOp.Blt (at.[6] - (at.[3] + 5)))
                | 5 -> br at.[7]
                | _ -> op
            )

        let body, at = layOut [] ops
        let shape = analyseOrFail (inputs [] [] false) body
        shape.Entry.[at.[7]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[4] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``the debuggable stamp disables folding only with its tracking bit set`` () : unit =
        // `Assembly::GetDebuggingCustomAttributes` honours `DisableOptimizations` only under
        // `Default` (JIT tracking), for both constructors.
        let compiled (attribute : string) : DumpedAssembly =
            let source =
                "[assembly: System.Diagnostics.Debuggable("
                + attribute
                + ")]\npublic class Program { public static int Main() { return 0; } }"

            use image = new System.IO.MemoryStream (Roslyn.compile [ source ])
            Assembly.read (LoggerFactory.makeTest () |> snd) None image

        let folds (attribute : string) : bool =
            StackShapeTokens.dynamicCompilationModeOf (compiled attribute)
            <> CompilationMode.Unoptimised

        folds "System.Diagnostics.DebuggableAttribute.DebuggingModes.DisableOptimizations"
        |> shouldEqual true

        folds
            "System.Diagnostics.DebuggableAttribute.DebuggingModes.Default | System.Diagnostics.DebuggableAttribute.DebuggingModes.DisableOptimizations"
        |> shouldEqual false

        folds "false, true" |> shouldEqual true
        folds "true, true" |> shouldEqual false
        folds "true, false" |> shouldEqual true

    [<Test>]
    let ``a folded branch's discarded edge joins no clique`` () : unit =
        // `ldc.r4; ldc.i4.0; brtrue B; A: pop; ldc.r8; B: pop; ret`: the branch folds, so A and B
        // are not siblings; B is reached from A alone, with a double, and A keeps its single.
        let ops =
            [
                ldcR4 // 0
                IlOp.Nullary NullaryIlOp.LdcI4_0 // 1
                brtrue 0 // 2 -> B
                pop // 3 A
                ldcR8 // 4
                pop // 5 B
                ret // 6
            ]

        let body, at = layOutWithBranches [] ops [ 2, 5 ]
        let shape = analyseOrFail (inputs [] [] false) body

        shape.Entry.[at.[3]] |> shouldEqual [ single ]
        shape.Entry.[at.[5]] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual Map.empty

        // Compiled without optimisation the branch is not folded, and the two are siblings.
        let unoptimised =
            analyseOrFail
                { inputs [] [] false with
                    Mode = CompilationMode.Unoptimised
                }
                body

        unoptimised.Entry.[at.[3]] |> shouldEqual [ double ]

        unoptimised.Promotions
        |> shouldEqual (Map.ofList [ at.[3], [ 0 ] ; at.[5], [ 0 ] ])

    [<Test>]
    let ``a spill clique spans successors of dead conditionals transitively`` () : unit =
        // Dead conditionals with successor pairs (A, C) and (C, B): A and B share a temp through
        // C, which nothing reaches.
        let ops =
            [
                ldarg0 // 0
                brtrue 0 // 1 -> BARM
                ldcR4 // 2
                br 0 // 3 -> A
                ldcR8 // 4 BARM
                br 0 // 5 -> B
                ldarg0 // 6 DEAD1
                brtrue 0 // 7 -> C, falls through to A
                pop // 8 A
                ret // 9
                ldarg0 // 10 DEAD2
                brtrue 0 // 11 -> B, falls through to C
                pop // 12 C
                ret // 13
                pop // 14 B
                ret // 15
            ]

        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 8 ; 5, 14 ; 7, 12 ; 11, 14 ]
        let shape = analyseOrFail (inputs [ other ] [] false) body

        shape.Entry.[at.[8]] |> shouldEqual [ double ]
        shape.Entry.[at.[14]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[12] |> shouldEqual false
        shape.Promotions |> shouldEqual (Map.ofList [ at.[8], [ 0 ] ])

    [<Test>]
    let ``dup of a non-zero constant is a temp to fully optimised code and a constant at Tier-0`` () : unit =
        let dup = IlOp.Nullary NullaryIlOp.Dup

        // Tier-0 clones the constant: `1; dup; pop` leaves 1, and the branch folds.
        let shape, join = armReachedIn CompilationMode.Tier0 [ ldcI4Of 1 ; dup ; pop ]
        shape.Entry.[join] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual Map.empty

        // Fully optimised code spills it, so both arms are imported.
        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ ldcI4Of 1 ; dup ; pop ]

        shape.Entry.[join] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ join, [ 0 ] ])

        // Zero is cloned in either mode.
        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ ldcI4Of 0 ; dup ; pop ]

        shape.Entry.[join] |> shouldEqual [ single ]
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a switch on a constant folds only in fully optimised code`` () : unit =
        // `ldc.r4; ldc.i4.1; switch (A, B); br JOIN; A: br JOIN; B: pop; ldc.r8; JOIN: pop; ret`:
        // the fall-through and A deliver the float32 to JOIN, B a double.
        let ops =
            [
                ldcR4 // 0
                ldcI4Of 1 // 1
                IlOp.Switch (ImmutableArray.Create (0, 0)) // 2 -> A, B
                br 0 // 3 fall-through -> JOIN
                br 0 // 4 A -> JOIN
                pop // 5 B
                ldcR8 // 6
                pop // 7 JOIN
                ret // 8
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 2 -> IlOp.Switch (ImmutableArray.Create (at.[4], at.[5]))
                | 3
                | 4 -> br at.[7]
                | _ -> op
            )

        let body, at = layOut [] ops

        let under (mode : CompilationMode) : StackShape =
            analyseOrFail
                { inputs [] [] false with
                    Mode = mode
                }
                body

        // Fully optimised: index 1 selects B alone, and JOIN sees only B's double.
        let optimised = under CompilationMode.FullyOptimised
        optimised.Entry.ContainsKey at.[3] |> shouldEqual false
        optimised.Entry.ContainsKey at.[4] |> shouldEqual false
        optimised.Entry.[at.[7]] |> shouldEqual [ double ]
        optimised.Promotions |> shouldEqual Map.empty

        // Tier-0 imports every target: the float32 arms meet B's double at JOIN and are cast.
        let tier0 = under CompilationMode.Tier0
        tier0.Entry.[at.[3]] |> shouldEqual [ single ]
        tier0.Entry.[at.[7]] |> shouldEqual [ double ]
        tier0.Promotions |> shouldEqual (Map.ofList [ at.[7], [ 0 ] ])

    [<Test>]
    let ``a branch on a literal the analysis does not fold leaves what it reaches unknown`` () : unit =
        // `ldc.r4 1; ldc.r4 2; clt; brtrue A; ldc.r4; br J; A: ldc.r8; J: pop; ret`: the JIT folds
        // the float comparison and imports one arm; the analysis folds only integers, so
        // rather than type J over both arms it leaves both arms and J unknown.
        let clt = IlOp.Nullary NullaryIlOp.Clt

        let ops =
            [
                IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 1.0f) // 0
                IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 2.0f) // 1
                clt // 2
                brtrue 0 // 3 -> A
                ldcR4 // 4
                br 0 // 5 -> J
                ldcR8 // 6 A
                pop // 7 J
                ret // 8
            ]

        let body, at = layOutWithBranches [] ops [ 3, 6 ; 5, 7 ]
        let shape = StackShape.analyse (inputs [] [] false) body

        shape.Invalid.IsEmpty |> shouldEqual true
        shape.Entry.[at.[3]] |> shouldEqual [ other ]

        for i in [ 4 ; 6 ; 7 ] do
            shape.Entry.ContainsKey at.[i] |> shouldEqual false

        shape.Promotions |> shouldEqual Map.empty

        // `ldnull` is a literal of the same kind, and so is what is computed from one.
        let ops =
            [
                IlOp.Nullary NullaryIlOp.LdNull // 0
                IlOp.Nullary NullaryIlOp.LdNull // 1
                IlOp.Nullary NullaryIlOp.Ceq // 2
                brtrue 0 // 3 -> A
                ldcR4 // 4
                br 0 // 5 -> J
                ldcR8 // 6 A
                pop // 7 J
                ret // 8
            ]

        let body, at = layOutWithBranches [] ops [ 3, 6 ; 5, 7 ]
        let shape = StackShape.analyse (inputs [] [] false) body
        shape.Entry.ContainsKey at.[7] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

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

    [<Test>]
    let ``only a branch reads its operands for a literal it cannot fold`` () : unit =
        // `ldc.r4; stloc.s 0; ldc.i4.1; brtrue T; ...`: the store's float operand is no branch
        // condition, and the integer branch after it still folds.
        let ops =
            [
                ldcR4 // 0
                IlOp.UnaryConst (UnaryConstIlOp.Stloc_s 0uy) // 1
                ldcI4Of 1 // 2
                brtrue 0 // 3 -> T
                ldcR4 // 4
                br 0 // 5 -> J
                ldcR8 // 6 T
                pop // 7 J
                ret // 8
            ]

        let body, at = layOutWithBranches [] ops [ 3, 6 ; 5, 7 ]

        let shape =
            analyseOrFail
                { inputs [] [ single ] false with
                    Mode = CompilationMode.FullyOptimised
                }
                body

        shape.Entry.[at.[7]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[4] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a literal converted to a float, and a duplicated float literal, stay literals the JIT may fold`` () : unit =
        let convR4 = IlOp.Nullary NullaryIlOp.Conv_R4
        let clt = IlOp.Nullary NullaryIlOp.Clt
        let ceq = IlOp.Nullary NullaryIlOp.Ceq
        let dup = IlOp.Nullary NullaryIlOp.Dup

        // `1; conv.r4; 2; conv.r4; clt` is a float comparison the JIT folds: undecidable here.
        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ ldcI4Of 1 ; convR4 ; ldcI4Of 2 ; convR4 ; clt ]

        shape.Entry.ContainsKey join |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

        // `ldc.r4 0.0; dup; ceq`: fully optimised code clones a positive zero rather than
        // spilling it, so the comparison is on literals the JIT folds: undecidable here.
        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 0.0f) ; dup ; ceq ]

        shape.Entry.ContainsKey join |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

    [<Test>]
    let ``a comparison between the two copies of a spilled dup folds`` () : unit =
        // Fully optimised code spills `dup` of a non-zero literal to a temp; the two reads of
        // that local compare as one value against itself (`gtFoldExprCompare`), and nothing
        // else is known about them.
        let dup = IlOp.Nullary NullaryIlOp.Dup

        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ ldcI4Of 1 ; dup ; IlOp.Nullary NullaryIlOp.Ceq ]

        shape.Entry.[join] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual Map.empty

        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ ldcI4Of 1 ; dup ; IlOp.Nullary NullaryIlOp.Clt ]

        shape.Entry.[join] |> shouldEqual [ single ]
        shape.Promotions |> shouldEqual Map.empty

        // A conditional branch between the copies folds the same way: `beq` is taken.
        let ops =
            [
                ldcI4Of 1 // 0
                dup // 1
                IlOp.UnaryConst (UnaryConstIlOp.Beq 0) // 2 -> T
                ldcR4 // 3
                br 0 // 4 -> J
                ldcR8 // 5 T
                pop // 6 J
                ret // 7
            ]

        let _, at = layOut [] ops

        let ops =
            ops
            |> List.mapi (fun i op ->
                match i with
                | 2 -> IlOp.UnaryConst (UnaryConstIlOp.Beq (at.[5] - (at.[2] + 5)))
                | 4 -> br at.[6]
                | _ -> op
            )

        let body, at = layOut [] ops

        let shape =
            analyseOrFail
                { inputs [] [] false with
                    Mode = CompilationMode.FullyOptimised
                }
                body

        shape.Entry.[at.[6]] |> shouldEqual [ double ]
        shape.Entry.ContainsKey at.[3] |> shouldEqual false
        shape.Promotions |> shouldEqual Map.empty

        // A single copy, though, is a local the importer does not fold a branch on.
        let shape, join =
            armReachedIn CompilationMode.FullyOptimised [ ldcI4Of 1 ; dup ; pop ]

        shape.Promotions |> shouldEqual (Map.ofList [ join, [ 0 ] ])

    [<Test>]
    let ``a float literal against a runtime value is no literal`` () : unit =
        // `ldarg.0; ldc.r4 0; cgt`: the JIT cannot fold a comparison with a runtime operand,
        // so both arms are imported and the join is typed over both.
        let shape, join =
            armReachedFrom
                CompilationMode.Tier0
                [ other ]
                [
                    ldarg0
                    IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 0.0f)
                    IlOp.Nullary NullaryIlOp.Cgt
                ]

        shape.Entry.[join] |> shouldEqual [ double ]
        shape.Promotions |> shouldEqual (Map.ofList [ join, [ 0 ] ])

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
            // arm i: `ldarg0; ldc.i4 i; beq -> arm i` would need integer comparisons; a chain
            // of `brtrue` on ldarg0 suffices since only the *shapes* matter, and an argument is
            // never a constant of its block, so every arm is imported.
            let armCount = case.Arms.Length

            let prefix =
                [
                    for _ in 1 .. armCount - 1 do
                        yield IlOp.Nullary NullaryIlOp.LdArg0
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
            let shape = analyseOrFail (inputs [ other ] [] false) body

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

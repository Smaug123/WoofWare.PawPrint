namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the stack-shape analysis: the evaluation stack's depth at the entry of every
/// instruction, joined over every path that reaches it.
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

    let private inputs (arguments : int) (locals : int) (returnsValue : bool) : StackShapeInputs =
        {
            Arguments = arguments
            Locals = locals
            ReturnsValue = returnsValue
            Tokens = Map.empty
        }

    /// Analyse, and fail if any reachable instruction could not be typed.
    let private analyseOrFail (inputs : StackShapeInputs) (body : MethodInstructions<TypeDefn>) : StackShape =
        let shape = StackShape.analyse inputs body

        if not shape.Invalid.IsEmpty then
            failwith $"expected every instruction to be typed, but got %O{shape.Invalid}"

        shape

    let private ldc : IlOp = IlOp.Nullary NullaryIlOp.LdcI4_1
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
        let shape = analyseOrFail (inputs 0 0 false) body

        [ 0..6 ]
        |> List.map (fun i -> shape.Entry.[at.[i]])
        |> shouldEqual [ 0 ; 1 ; 2 ; 1 ; 2 ; 1 ; 0 ]

        shape.Reachable |> shouldEqual (Set.ofSeq at.Values)

    [<Test>]
    let ``two arms of one depth meet at a typed join`` () : unit =
        // `ldarg0; brtrue L; ldc; br J; L: ldc; ldc; add; J: pop; ret`
        let ops = [ ldarg0 ; brtrue 0 ; ldc ; br 0 ; ldc ; ldc ; add ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 4 ; 3, 7 ]
        let shape = analyseOrFail (inputs 1 0 false) body

        shape.Entry.[at.[4]] |> shouldEqual 0
        shape.Entry.[at.[6]] |> shouldEqual 2
        shape.Entry.[at.[7]] |> shouldEqual 1
        shape.Entry.[at.[8]] |> shouldEqual 0

    [<Test>]
    let ``a loop's back edge delivers the depth its head already has`` () : unit =
        // `ldc; HEAD: dup; brtrue HEAD; pop; ret`
        let ops = [ ldc ; dup ; brtrue 0 ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 2, 1 ]
        let shape = analyseOrFail (inputs 0 0 false) body

        shape.Entry.[at.[1]] |> shouldEqual 1
        shape.Entry.[at.[2]] |> shouldEqual 2
        shape.Entry.[at.[3]] |> shouldEqual 1

    [<Test>]
    let ``two paths of different depth at a join is a conflict, and what follows only from it is untyped`` () : unit =
        // `ldarg0; brtrue L; br J; L: ldc; br J; J: nop; ret` returning a value: `ret` would
        // underflow on the empty arm, but the arms disagree at J, so J is a conflict and
        // everything past it is neither typed nor invalid.
        let ops = [ ldarg0 ; brtrue 0 ; br 0 ; ldc ; br 0 ; nop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 3 ; 2, 5 ; 4, 5 ]
        let shape = StackShape.analyse (inputs 1 0 true) body

        match Map.tryFind at.[5] shape.Invalid with
        | Some (StackShapeError.DepthMismatch (_, 0, 1) as error) -> error.IsConflict |> shouldEqual true
        | other -> failwith $"expected DepthMismatch at the join, got %O{other}"

        shape.Invalid.ContainsKey at.[6] |> shouldEqual false
        shape.Entry.ContainsKey at.[5] |> shouldEqual false
        shape.Entry.ContainsKey at.[6] |> shouldEqual false
        shape.Reachable.Contains at.[6] |> shouldEqual true
        shape.Entry.[at.[3]] |> shouldEqual 0
        shape.Entry.[at.[4]] |> shouldEqual 1

    [<Test>]
    let ``an arrival at a join that already failed on another arm makes it a conflict`` () : unit =
        // `ldarg0; brtrue GOOD; br JOIN; GOOD: ldc; br JOIN; JOIN: ret` returning a value: JOIN is
        // reached empty first, where `ret` underflows, and then one deep. Two arms that disagree
        // are a conflict whichever arrives first, since the empty one may be an arm the importer
        // never imports.
        let ops = [ ldarg0 ; brtrue 0 ; br 0 ; ldc ; br 0 ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 3 ; 2, 5 ; 4, 5 ]
        let shape = StackShape.analyse (inputs 1 0 true) body

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
        let shape = StackShape.analyse (inputs 1 0 true) body

        shape.Invalid.[at.[8]].IsConflict |> shouldEqual true
        shape.Invalid.ContainsKey at.[9] |> shouldEqual false
        shape.Entry.ContainsKey at.[9] |> shouldEqual false
        shape.Reachable.Contains at.[9] |> shouldEqual true
        shape.Entry.[at.[6]] |> shouldEqual 0

    [<Test>]
    let ``popping an empty stack is invalid IL`` () : unit =
        let body, _ = layOut [] [ pop ; ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs 0 0 false) body).Invalid with
        | Some (StackShapeError.StackUnderflow (0, _, 0) as error) -> error.IsConflict |> shouldEqual false
        | other -> failwith $"expected StackUnderflow at offset 0, got %O{other}"

    [<Test>]
    let ``an untypable arm leaves the rest of the body typed`` () : unit =
        // `ldarg0; brtrue BAD; ldc; ret; BAD: pop; ret`: BAD underflows, and that is recorded
        // against BAD alone; the other arm is typed, and what follows BAD is untyped.
        let ops = [ ldarg0 ; brtrue 0 ; ldc ; ret ; pop ; ret ]
        let body, at = layOutWithBranches [] ops [ 1, 4 ]
        let shape = StackShape.analyse (inputs 1 0 true) body

        shape.Entry.[at.[2]] |> shouldEqual 0
        shape.Entry.[at.[3]] |> shouldEqual 1

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
        let shape = StackShape.analyse (inputs 1 0 false) body

        match Map.tryFind at.[1] shape.Invalid with
        | Some (StackShapeError.BranchOutsideBody (_, target)) -> target |> shouldEqual 1000
        | other -> failwith $"expected BranchOutsideBody at the branch, got %O{other}"

        // The branch delivered nothing, so its fall-through is untyped.
        shape.Entry.ContainsKey at.[2] |> shouldEqual false

    [<Test>]
    let ``an argument or local beyond the signature is invalid at the load`` () : unit =
        let body, _ = layOut [] [ IlOp.Nullary NullaryIlOp.LdArg1 ; ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs 1 0 false) body).Invalid with
        | Some (StackShapeError.ArgumentOutOfRange (0, 1)) -> ()
        | other -> failwith $"expected ArgumentOutOfRange, got %O{other}"

        analyseOrFail (inputs 2 0 false) body |> ignore

        let body, _ = layOut [] [ IlOp.UnaryConst (UnaryConstIlOp.Stloc_s 2uy) ; ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs 0 2 false) body).Invalid with
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
        let shape = analyseOrFail (inputs 0 0 false) body

        [ 0..8 ]
        |> List.map (fun i -> shape.Entry.[at.[i]])
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
        let shape = analyseOrFail (inputs 1 0 false) body

        shape.Entry.[at.[3]] |> shouldEqual 1
        shape.Entry.[at.[5]] |> shouldEqual 1
        shape.Entry.[at.[6]] |> shouldEqual 0

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
            { inputs 0 0 false with
                Tokens = Map.ofList [ at.[2], shape ]
            }

        let shape = analyseOrFail (withCall (TokenShape.Callee (2, true))) body
        shape.Entry.[at.[3]] |> shouldEqual 1
        shape.Entry.[at.[5]] |> shouldEqual 1

        let shape = StackShape.analyse (withCall (TokenShape.Callee (2, false))) body

        match Map.tryFind at.[4] shape.Invalid with
        | Some (StackShapeError.StackUnderflow (_, _, 1)) -> ()
        | other -> failwith $"expected the add after a void call to underflow, got %O{other}"

        match Map.tryFind at.[2] (StackShape.analyse (inputs 0 0 false) body).Invalid with
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
        let shape = StackShape.analyse (inputs 1 0 true) body

        match Map.tryFind at.[3] shape.Invalid with
        | Some (StackShapeError.MissingTokenShape _) -> ()
        | other -> failwith $"expected MissingTokenShape at the call, got %O{other}"

        shape.Invalid.ContainsKey at.[4] |> shouldEqual false
        shape.Entry.ContainsKey at.[4] |> shouldEqual false
        shape.Reachable.Contains at.[4] |> shouldEqual true

    [<Test>]
    let ``a call token that does not name a method has no shape`` () : unit =
        // Invalid IL the instruction refuses if it executes; nothing is claimed ahead of that.
        let corelib =
            Assembly.readFile (LoggerFactory.makeTest () |> snd) typeof<obj>.Assembly.Location

        let field =
            MetadataToken.FieldDefinition (System.Reflection.Metadata.Ecma335.MetadataTokens.FieldDefinitionHandle 1)

        StackShapeTokens.ofMetadataToken corelib UnaryMetadataTokenIlOp.Call field
        |> shouldEqual None

        StackShapeTokens.ofMetadataToken corelib UnaryMetadataTokenIlOp.Newobj field
        |> shouldEqual None

        // Rows the tables do not have, and a signature row that is not there to decode.
        let absent (token : MetadataToken) : unit =
            StackShapeTokens.ofMetadataToken corelib UnaryMetadataTokenIlOp.Call token
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
                StackShapeTokens.ofMetadataToken test UnaryMetadataTokenIlOp.Call (MetadataToken.MemberReference handle)
                |> shouldEqual None
            | MemberSignature.Method _ -> ()

    [<Test>]
    let ``a calli signature's arity read from the blob agrees with the full decoder`` () : unit =
        // CoreLib's standalone signatures are its `calli` targets and its locals signatures;
        // for every one the full decoder accepts, the arity reader must agree, and a locals
        // signature is no call at all.
        let corelib =
            Assembly.readFile (LoggerFactory.makeTest () |> snd) typeof<obj>.Assembly.Location

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
                    UnaryMetadataTokenIlOp.Calli
                    (MetadataToken.StandaloneSignature handle)

            if header.Kind = System.Reflection.Metadata.SignatureKind.Method then
                methods <- methods + 1

                let expected =
                    signature.DecodeMethodSignature (TypeDefn.typeProvider corelib.Name, ())
                    |> TypeMethodSignature.make
                    |> StackShapeTokens.calleeShape

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
            { inputs 0 0 false with
                Tokens = Map.ofList [ at.[2], TokenShape.Callee (1, false) ; at.[3], TokenShape.Callee (0, false) ]
            }

        let shape = analyseOrFail withTokens body
        shape.Entry.[at.[3]] |> shouldEqual 0
        shape.Entry.[at.[4]] |> shouldEqual 1

    [<Test>]
    let ``ret pops the return value exactly when the method returns one`` () : unit =
        let body, _ = layOut [] [ ldc ; ret ]
        analyseOrFail (inputs 0 0 true) body |> ignore

        // A value left on the stack at `ret` is not an underflow, and the analysis does not
        // police it; only the pop can fail, and it does not here.
        (StackShape.analyse (inputs 0 0 false) body).Invalid.IsEmpty |> shouldEqual true

        let body, _ = layOut [] [ ret ]

        match Map.tryFind 0 (StackShape.analyse (inputs 0 0 true) body).Invalid with
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

        StackShapeTokens.returnsValue (MethodReturnType.Returns modifiedVoid)
        |> shouldEqual false

        StackShapeTokens.returnsValue (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Single))
        |> shouldEqual true

        StackShapeTokens.returnsValue MethodReturnType.Void |> shouldEqual false

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

        StackShapeTokens.calleeShape (
            signature (
                System.Reflection.Metadata.SignatureAttributes.Instance
                ||| System.Reflection.Metadata.SignatureAttributes.ExplicitThis
            )
        )
        |> shouldEqual (TokenShape.Callee (2, false))

        StackShapeTokens.calleeShape (signature System.Reflection.Metadata.SignatureAttributes.Instance)
        |> shouldEqual (TokenShape.Callee (3, false))

        StackShapeTokens.calleeShape (signature System.Reflection.Metadata.SignatureAttributes.None)
        |> shouldEqual (TokenShape.Callee (2, false))

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
        let shape = analyseOrFail (inputs 0 0 false) body
        shape.Entry.ContainsKey at.[1] |> shouldEqual false
        shape.Entry.[at.[3]] |> shouldEqual 1

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
            let shape = StackShape.analyse (inputs 1 0 false) body

            shape.Reachable |> shouldEqual (Set.ofSeq at.Values)

            // Each arm's own instructions are typed whatever the join does.
            for i in 0 .. armCount - 1 do
                for k in 0 .. case.Arms.[i] do
                    shape.Entry.[at.[armStart i + k]] |> shouldEqual k

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
                        shape.Entry.[tailAt k] |> shouldEqual depth
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

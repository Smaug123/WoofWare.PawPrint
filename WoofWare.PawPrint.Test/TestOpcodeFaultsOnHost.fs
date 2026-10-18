namespace WoofWare.PawPrint.Test

open System
open System.Reflection.Emit
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The `OpcodeFaults` table checked against the host runtime, for the instructions whose operands
/// are all numbers: arithmetic, comparison, shifts, conversions and `ckfinite`.
///
/// Each instruction runs as a `DynamicMethod` whose body is `ldarg`s, the instruction, `ret`. The
/// result is returned rather than popped, because the JIT is free to drop a computation whose value
/// nothing reads. On a grid of boundary operands, what the host raises must be exactly what the
/// table lists: nothing it leaves out, which would make an analysis reading the table unsound, and
/// nothing it lists needlessly. Beyond the grid, a property checks that random operands raise only
/// what the table lists.
///
/// The host is the oracle here, so a run checks the code the JIT emits for the host's own
/// architecture.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpcodeFaultsOnHost =

    /// A pairing of an instruction with the wrong `OpCodes` field would check one instruction's
    /// entry against another's behaviour.
    [<Test>]
    let ``each instruction is paired with its own encoding`` () : unit =
        let spelling (name : string) : string =
            name.Replace("_", "").Replace(".", "").ToLowerInvariant ()

        for op, opcode, _ in NumericOpcodeGrid.instructions do
            spelling opcode.Name |> shouldEqual (spelling $"%O{op}")

        NumericOpcodeGrid.instructions
        |> List.map (fun (op, _, _) -> op)
        |> List.distinct
        |> List.length
        |> shouldEqual NumericOpcodeGrid.instructions.Length

    [<Test>]
    let ``on boundary operands, each instruction raises exactly what the table lists`` () : unit =
        let mismatches =
            [
                for op, opcode, typing in NumericOpcodeGrid.instructions do
                    let observed =
                        [
                            for operands, result in NumericOpcodeGrid.signatures typing do
                                let method = NumericOpcodeGrid.probe opcode operands result

                                for arguments in
                                    HostFaultProbe.cartesian (List.map NumericOpcodeGrid.boundaries operands) do
                                    match HostFaultProbe.raised method arguments with
                                    | Some name -> name, $"%A{operands} %A{arguments}"
                                    | None -> ()
                        ]
                        |> List.groupBy fst
                        |> List.map (fun (name, witnesses) -> name, snd (List.head witnesses))
                        |> Map.ofList

                    yield! HostFaultProbe.mismatches $"%O{op}" (OpcodeFaults.ofNullary op) Set.empty observed
            ]

        match mismatches with
        | [] -> ()
        | mismatches -> failwith (String.concat Environment.NewLine mismatches)

    /// Any value of the operand's host type, weighted towards the boundaries.
    let private operandValue (operand : NumericOpcodeGrid.Operand) : Gen<obj> =
        let anyInt64 : Gen<int64> =
            Gen.map2
                (fun (high : int) (low : int) -> (int64 high <<< 32) ||| int64 (uint32 low))
                (Gen.choose (Int32.MinValue, Int32.MaxValue))
                (Gen.choose (Int32.MinValue, Int32.MaxValue))

        // Every bit pattern is a float64, which reaches NaN payloads, subnormals and huge
        // magnitudes; a random integer plus a fraction reaches the ranges conversions target.
        let anyFloat : Gen<float> =
            Gen.oneof
                [
                    anyInt64 |> Gen.map BitConverter.Int64BitsToDouble
                    Gen.map2
                        (fun (n : int64) (fraction : int) -> float n + float fraction / 4.0)
                        anyInt64
                        (Gen.choose (-3, 3))
                ]

        let random : Gen<obj> =
            match operand with
            | NumericOpcodeGrid.Operand.Int32 -> Gen.choose (Int32.MinValue, Int32.MaxValue) |> Gen.map box
            | NumericOpcodeGrid.Operand.Int64 -> anyInt64 |> Gen.map box
            | NumericOpcodeGrid.Operand.NativeInt -> anyInt64 |> Gen.map (nativeint >> box)
            | NumericOpcodeGrid.Operand.Float64 -> anyFloat |> Gen.map box
            | NumericOpcodeGrid.Operand.Float32 -> anyFloat |> Gen.map (float32 >> box)

        Gen.frequency [ 1, Gen.elements (NumericOpcodeGrid.boundaries operand) ; 3, random ]

    [<Test>]
    let ``on any operands, each instruction raises only what the table lists`` () : unit =
        let case =
            gen {
                let! op, opcode, typing = Gen.elements NumericOpcodeGrid.instructions
                let! operands, result = Gen.elements (NumericOpcodeGrid.signatures typing)
                let! arguments = operands |> List.map operandValue |> Gen.sequenceToList
                return op, opcode, operands, result, arguments
            }

        let property
            (
                op : NullaryIlOp,
                opcode : OpCode,
                operands : NumericOpcodeGrid.Operand list,
                result : NumericOpcodeGrid.Operand,
                arguments : obj list
            )
            : bool
            =
            match HostFaultProbe.raised (NumericOpcodeGrid.probe opcode operands result) arguments with
            | None -> true
            | Some name -> (HostFaultProbe.listed (OpcodeFaults.ofNullary op)).Contains name

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 20000, Prop.forAll (Arb.fromGen case) property)

    /// Whether `argument`, which the host passes as `operand`, is an integer of the value `value`;
    /// a native int at the host's pointer width.
    let private hasValue (operand : NumericOpcodeGrid.Operand) (argument : obj) (value : IntegerValue) : bool =
        match operand with
        | NumericOpcodeGrid.Operand.Int32 -> IntegerValue.holdsOfInt32 value (unbox<int32> argument)
        | NumericOpcodeGrid.Operand.Int64 -> IntegerValue.holdsOfInt64 value (unbox<int64> argument)
        | NumericOpcodeGrid.Operand.NativeInt ->
            match IntPtr.Size with
            | 8 -> IntegerValue.holdsOfInt64 value (int64 (unbox<nativeint> argument))
            | 4 -> IntegerValue.holdsOfInt32 value (int32 (unbox<nativeint> argument))
            | size -> failwith $"No host has a pointer of %d{size} bytes"
        | NumericOpcodeGrid.Operand.Float32
        | NumericOpcodeGrid.Operand.Float64 -> false

    /// How a run of `op` on `arguments`, pushed in order as `operands`, that raised the exception of
    /// the full name `raised` fails what `OpcodeFaults.requiredOperands` says the fault needs; `None`
    /// where it meets it, or where the fault is not one the table names.
    let private unmetRequirement
        (op : NullaryIlOp)
        (operands : NumericOpcodeGrid.Operand list)
        (arguments : obj list)
        (raised : string)
        : string option
        =
        let fault =
            FSharp.Reflection.FSharpType.GetUnionCases typeof<OpcodeFault>
            |> Array.map (fun case -> FSharp.Reflection.FSharpValue.MakeUnion (case, [||]) :?> OpcodeFault)
            |> Array.tryFind (fun fault -> OpcodeFault.typeName fault = raised)

        match fault with
        | None -> None
        | Some fault ->
            let depth = List.length operands

            OpcodeFaults.requiredOperands (IlOp.Nullary op) fault
            |> List.tryFind (fun required ->
                let index = depth - 1 - required.FromTop
                not (hasValue (List.item index operands) (List.item index arguments) required.Is)
            )
            |> Option.map (fun required ->
                $"%O{op} raised %s{raised} on %A{operands} %A{arguments}, whose operand %d{required.FromTop} is not %O{required.Is}"
            )

    [<Test>]
    let ``on boundary operands, an instruction raises a fault only where its operands are what the fault needs``
        ()
        : unit
        =
        let unmet =
            [
                for op, opcode, typing in NumericOpcodeGrid.instructions do
                    for operands, result in NumericOpcodeGrid.signatures typing do
                        let method = NumericOpcodeGrid.probe opcode operands result

                        for arguments in HostFaultProbe.cartesian (List.map NumericOpcodeGrid.boundaries operands) do
                            match HostFaultProbe.raised method arguments with
                            | Some raised ->
                                match unmetRequirement op operands arguments raised with
                                | Some unmet -> yield unmet
                                | None -> ()
                            | None -> ()
            ]

        match unmet with
        | [] -> ()
        | unmet -> failwith (String.concat Environment.NewLine unmet)

    [<Test>]
    let ``on any operands, an instruction raises a fault only where its operands are what the fault needs`` () : unit =
        // The instructions with a fault that needs something of the operands; the others raise
        // whatever they raise on any operands.
        let narrowed =
            NumericOpcodeGrid.instructions
            |> List.filter (fun (op, _, _) ->
                match OpcodeFaults.ofNullary op with
                | OpcodeFaults.Raises faults ->
                    faults
                    |> List.exists (fun fault -> not (OpcodeFaults.requiredOperands (IlOp.Nullary op) fault).IsEmpty)
                | OpcodeFaults.Unmodelled -> false
            )

        narrowed |> List.isEmpty |> shouldEqual false

        let case =
            gen {
                let! op, opcode, typing = Gen.elements narrowed
                let! operands, result = Gen.elements (NumericOpcodeGrid.signatures typing)
                let! arguments = operands |> List.map operandValue |> Gen.sequenceToList
                return op, opcode, operands, result, arguments
            }

        let property
            (
                op : NullaryIlOp,
                opcode : OpCode,
                operands : NumericOpcodeGrid.Operand list,
                result : NumericOpcodeGrid.Operand,
                arguments : obj list
            )
            : bool
            =
            match HostFaultProbe.raised (NumericOpcodeGrid.probe opcode operands result) arguments with
            | None -> true
            | Some raised -> (unmetRequirement op operands arguments raised).IsNone

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 20000, Prop.forAll (Arb.fromGen case) property)

namespace WoofWare.PawPrint.Test

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FSharp.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for <see cref="IlDecoding.decodeInstructions"/>, which turns a method body's IL
/// bytes into <see cref="IlOp"/>s.
/// </summary>
/// <remarks>
/// The oracle throughout is <c>System.Reflection.Emit.OpCodes</c>: the BCL's own table of
/// every ECMA-335 instruction, giving its encoding, its size, and the kind of operand that
/// follows it. Nothing in that table derives from PawPrint, so a decoder that maps an
/// opcode to the wrong instruction, or reads an operand at the wrong width or signedness,
/// disagrees with it.
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIlDecoding =

    let private sourceAssembly : AssemblyName = AssemblyName "TestIlDecoding"

    /// Every opcode the BCL knows, keyed so that an F# union case name lowercases onto it:
    /// ECMA writes `ldc.i4.s` where the DU writes `Ldc_I4_s`, and the prefix instructions
    /// carry a trailing separator (`unaligned.`) that no case name reproduces.
    let private opCodes : IReadOnlyDictionary<string, OpCode> =
        typeof<OpCodes>.GetFields (BindingFlags.Public ||| BindingFlags.Static)
        |> Array.filter (fun f -> f.FieldType = typeof<OpCode>)
        |> Array.map (fun f -> f.GetValue (null : obj) |> unbox<OpCode>)
        |> Array.map (fun op ->
            let name : string = op.Name
            name.TrimEnd('.').Replace (".", "_"), op
        )
        |> readOnlyDict

    /// The union cases whose names do not lowercase onto their ECMA name, mapped to the
    /// key they should have had. These are naming inconsistencies in the DU itself —
    /// `LdArg0` beside `Ldloc_0`, `LdcI4_m1` beside `Ldc_I4_s` — not decoding questions.
    let private caseNameOverrides : IReadOnlyDictionary<string, string> =
        [
            for i in 0..3 do
                yield $"LdArg%i{i}", $"ldarg_%i{i}"
            for i in 0..8 do
                yield $"LdcI4_%i{i}", $"ldc_i4_%i{i}"
            yield "LdcI4_m1", "ldc_i4_m1"
        ]
        |> readOnlyDict

    /// Cases ECMA-335 defines as assembler aliases which share another instruction's
    /// encoding: `ldind.u8` is `ldind.i8` (III.3.42), `ldelem.u8` is `ldelem.i8`
    /// (III.4.7), and `stelem.u*` are the `stelem.i*` of the same width (III.4.26) —
    /// a store of N bytes does not care about the sign of what it stores. No byte sequence
    /// decodes to them, so they are unreachable from <c>decodeInstructions</c> by
    /// construction; the interpreter nonetheless has arms for them.
    let private aliasCases : Set<string> =
        Set.ofList
            [
                "Ldind_u8"
                "Ldelem_u8"
                "Stelem_u1"
                "Stelem_u2"
                "Stelem_u4"
                "Stelem_u8"
            ]

    /// An instruction's inline operand, in the terms PawPrint's DUs carry them.
    [<RequireQualifiedAccess>]
    type private Operand =
        | None
        | Byte of uint8
        | SByte of int8
        | UInt16 of uint16
        | Int32 of int32
        | Int64 of int64
        | Single of single
        | Double of float
        /// A metadata token: carried on the `IlOp.UnaryMetadataToken` wrapper, not on the
        /// inner case, so the inner case has no field of its own.
        | MetadataToken of int32
        /// Likewise for a string token and `IlOp.UnaryStringToken`.
        | StringToken of int32
        | Switch of int32 list

    /// The operand values to try for an instruction, chosen so that a decoder reading the
    /// wrong width or the wrong signedness cannot reproduce them.
    let private samplesFor (op : OpCode) : Operand list =
        match op.OperandType with
        | OperandType.InlineNone -> [ Operand.None ]
        | OperandType.ShortInlineVar ->
            // 200 is the value that separates a signed read from an unsigned one.
            [ Operand.Byte 0uy ; Operand.Byte 200uy ; Operand.Byte 255uy ]
        | OperandType.ShortInlineI ->
            if op.Value = OpCodes.Unaligned.Value then
                // `unaligned.` is the one instruction whose one-byte operand ECMA-335
                // III.2.5 defines as unsigned — it is an alignment, so 1, 2 or 4. The BCL's
                // table lumps it in with the signed one-byte operands, so say so here.
                [ Operand.Byte 1uy ; Operand.Byte 2uy ; Operand.Byte 4uy ]
            else
                [ Operand.SByte -128y ; Operand.SByte -1y ; Operand.SByte 127y ]
        | OperandType.ShortInlineBrTarget -> [ Operand.SByte -128y ; Operand.SByte -1y ; Operand.SByte 127y ]
        | OperandType.InlineVar -> [ Operand.UInt16 0us ; Operand.UInt16 40000us ; Operand.UInt16 65535us ]
        | OperandType.InlineI
        | OperandType.InlineBrTarget -> [ Operand.Int32 Int32.MinValue ; Operand.Int32 -1 ; Operand.Int32 0x12345678 ]
        | OperandType.InlineI8 -> [ Operand.Int64 Int64.MinValue ; Operand.Int64 0x0123456789ABCDEFL ]
        | OperandType.ShortInlineR -> [ Operand.Single -3.5f ; Operand.Single Single.MaxValue ]
        | OperandType.InlineR -> [ Operand.Double -3.5 ; Operand.Double Double.MaxValue ]
        | OperandType.InlineMethod
        | OperandType.InlineField
        | OperandType.InlineType
        | OperandType.InlineTok
        | OperandType.InlineSig ->
            // The decoder does not check a token's table against the opcode that carries
            // it, so one row from each of two tables exercises the operand either way.
            [ Operand.MetadataToken 0x0A000001 ; Operand.MetadataToken 0x02000010 ]
        | OperandType.InlineString -> [ Operand.StringToken 0x70000004 ]
        | OperandType.InlineSwitch ->
            [
                Operand.Switch []
                Operand.Switch [ 0 ]
                Operand.Switch [ 0 ; -1 ; 0x12345678 ; Int32.MinValue ]
            ]
        | other -> failwith $"TestIlDecoding has no sample operands for OperandType %O{other} (opcode %s{op.Name})"

    let private int32Bytes (v : int32) : byte[] =
        let buf = Array.zeroCreate 4
        BinaryPrimitives.WriteInt32LittleEndian (buf.AsSpan (), v)
        buf

    /// The bytes ECMA-335 III.1.2 says encode this instruction: the opcode (one byte, or
    /// 0xFE followed by a second), then the operand little-endian.
    let private encode (op : OpCode) (operand : Operand) : byte[] =
        let opCodeBytes =
            match op.Size with
            | 1 -> [| byte op.Value |]
            | 2 -> [| byte (uint16 op.Value >>> 8) ; byte op.Value |]
            | size -> failwith $"unexpected opcode size %i{size} for %s{op.Name}"

        let operandBytes =
            match operand with
            | Operand.None -> [||]
            | Operand.Byte b -> [| b |]
            | Operand.SByte b -> [| byte b |]
            | Operand.UInt16 v ->
                let buf = Array.zeroCreate 2
                BinaryPrimitives.WriteUInt16LittleEndian (buf.AsSpan (), v)
                buf
            | Operand.Int32 v -> int32Bytes v
            | Operand.MetadataToken v -> int32Bytes v
            | Operand.StringToken v -> int32Bytes v
            | Operand.Int64 v ->
                let buf = Array.zeroCreate 8
                BinaryPrimitives.WriteInt64LittleEndian (buf.AsSpan (), v)
                buf
            | Operand.Single v ->
                let buf = Array.zeroCreate 4
                BinaryPrimitives.WriteSingleLittleEndian (buf.AsSpan (), v)
                buf
            | Operand.Double v ->
                let buf = Array.zeroCreate 8
                BinaryPrimitives.WriteDoubleLittleEndian (buf.AsSpan (), v)
                buf
            | Operand.Switch targets ->
                let count = Array.zeroCreate 4
                BinaryPrimitives.WriteUInt32LittleEndian (count.AsSpan (), uint32 (List.length targets))
                Array.append count (targets |> List.collect (int32Bytes >> List.ofArray) |> Array.ofList)

        Array.append opCodeBytes operandBytes

    /// The `IlOp` this instruction and operand must decode to. Built by reflection
    /// rather than by a second hand-written table: the *only* PawPrint input is the
    /// union case, so nothing here can inherit a mistake from the decoder's own
    /// opcode table.
    let private expectedOp (case : UnionCaseInfo) (operand : Operand) : Result<IlOp, string> =
        let fieldType : Type option =
            match case.GetFields () with
            | [||] -> None
            | [| f |] -> Some f.PropertyType
            | fields -> failwith $"union case %s{case.Name} has %i{fields.Length} fields; expected at most one"

        let carried : (obj * Type) option =
            match operand with
            | Operand.None -> None
            | Operand.Byte b -> Some (box b, typeof<uint8>)
            | Operand.SByte b -> Some (box b, typeof<int8>)
            | Operand.UInt16 v -> Some (box v, typeof<uint16>)
            | Operand.Int32 v -> Some (box v, typeof<int32>)
            | Operand.Int64 v -> Some (box v, typeof<int64>)
            | Operand.Single v -> Some (box v, typeof<single>)
            | Operand.Double v -> Some (box v, typeof<float>)
            | Operand.Switch targets -> Some (box (ImmutableArray.CreateRange targets), typeof<ImmutableArray<int32>>)
            // These live on the `IlOp` wrapper rather than on the inner case.
            | Operand.MetadataToken _
            | Operand.StringToken _ -> None

        let inner (expectedFieldType : Type option) (args : obj[]) : Result<obj, string> =
            match fieldType, expectedFieldType with
            | None, None -> FSharpValue.MakeUnion (case, args) |> Ok
            | Some actual, Some expected when actual = expected -> FSharpValue.MakeUnion (case, args) |> Ok
            | actual, expected ->
                let describe (t : Type option) =
                    match t with
                    | None -> "no operand"
                    | Some t -> t.FullName

                Error $"declares %s{describe actual} but ECMA-335 gives it %s{describe expected}"

        match operand, case.DeclaringType with
        | Operand.MetadataToken token, t when t = typeof<UnaryMetadataTokenIlOp> ->
            inner None [||]
            |> Result.map (fun op ->
                IlOp.UnaryMetadataToken (
                    unbox<UnaryMetadataTokenIlOp> op,
                    MetadataOperand.FromMetadata (SourcedMetadataToken.ofInt sourceAssembly token)
                )
            )
        | Operand.StringToken token, t when t = typeof<UnaryStringTokenIlOp> ->
            inner None [||]
            |> Result.map (fun op ->
                IlOp.UnaryStringToken (
                    unbox<UnaryStringTokenIlOp> op,
                    StringOperand.FromMetadata (SourcedStringToken.ofInt sourceAssembly token)
                )
            )
        | Operand.Switch targets, t when t = typeof<IlOp> -> IlOp.Switch (ImmutableArray.CreateRange targets) |> Ok
        | _, t when t = typeof<NullaryIlOp> ->
            inner (carried |> Option.map snd) (carried |> Option.map fst |> Option.toArray)
            |> Result.map (unbox<NullaryIlOp> >> IlOp.Nullary)
        | _, t when t = typeof<UnaryConstIlOp> ->
            inner (carried |> Option.map snd) (carried |> Option.map fst |> Option.toArray)
            |> Result.map (unbox<UnaryConstIlOp> >> IlOp.UnaryConst)
        | operand, t -> Error $"case lives on %s{t.FullName} but its operand is %O{operand}"

    /// `IlOp` carries `SourcedMetadataToken`, which has no structural equality because an
    /// `AssemblyName` has none; compare the parts that do.
    let private opsEqual (a : IlOp) (b : IlOp) : bool =
        match a, b with
        | IlOp.Nullary a, IlOp.Nullary b -> a = b
        | IlOp.UnaryConst a, IlOp.UnaryConst b -> a = b
        | IlOp.Switch a, IlOp.Switch b -> List.ofSeq a = List.ofSeq b
        | IlOp.UnaryMetadataToken (a, MetadataOperand.FromMetadata ta),
          IlOp.UnaryMetadataToken (b, MetadataOperand.FromMetadata tb) ->
            a = b
            && ta.Token = tb.Token
            && ta.SourceAssembly.FullName = tb.SourceAssembly.FullName
        | IlOp.UnaryMetadataToken (a, MetadataOperand.FromDynamicScope ia),
          IlOp.UnaryMetadataToken (b, MetadataOperand.FromDynamicScope ib) -> a = b && ia = ib
        | IlOp.UnaryStringToken (a, StringOperand.FromMetadata ta),
          IlOp.UnaryStringToken (b, StringOperand.FromMetadata tb) ->
            a = b
            && ta.Token = tb.Token
            && ta.SourceAssembly.FullName = tb.SourceAssembly.FullName
        | IlOp.UnaryStringToken (a, StringOperand.FromDynamicScope ia),
          IlOp.UnaryStringToken (b, StringOperand.FromDynamicScope ib) -> a = b && ia = ib
        | _, _ -> false

    let private allCases : UnionCaseInfo list =
        [
            typeof<NullaryIlOp>
            typeof<UnaryConstIlOp>
            typeof<UnaryMetadataTokenIlOp>
            typeof<UnaryStringTokenIlOp>
        ]
        |> List.collect (FSharpType.GetUnionCases >> List.ofArray)
        |> List.append (
            FSharpType.GetUnionCases typeof<IlOp>
            |> Array.filter (fun c -> c.Name = "Switch")
            |> List.ofArray
        )

    [<Test>]
    let ``every instruction decodes back to the case that encodes it`` () =
        let failures =
            allCases
            |> List.filter (fun case -> not (aliasCases.Contains case.Name))
            |> List.collect (fun case ->
                let key =
                    match caseNameOverrides.TryGetValue case.Name with
                    | true, k -> k
                    | false, _ -> case.Name.ToLowerInvariant ()

                match opCodes.TryGetValue key with
                | false, _ ->
                    [
                        $"%s{case.Name}: no ECMA instruction is named %s{key}; either the case is misnamed or it is an alias that needs recording in aliasCases"
                    ]
                | true, opCode ->
                    samplesFor opCode
                    |> List.collect (fun operand ->
                        match expectedOp case operand with
                        | Error e -> [ $"%s{case.Name}: %s{e}" ]
                        | Ok expected ->

                        let bytes = encode opCode operand

                        match IlDecoding.decodeInstructions (IlTokenUniverse.Metadata sourceAssembly) bytes with
                        | [ (actual, 0) ] when opsEqual expected actual ->
                            let declaredSize = IlOp.NumberOfBytes actual

                            if declaredSize = bytes.Length then
                                []
                            else
                                [
                                    $"%s{case.Name} with operand %O{operand}: NumberOfBytes says %i{declaredSize} but the instruction encodes to %i{bytes.Length} bytes"
                                ]
                        | decoded ->
                            let rendered =
                                decoded
                                |> List.map (fun (op, off) -> $"%O{op} at %i{off}")
                                |> String.concat "; "

                            [
                                $"%s{case.Name} with operand %O{operand}: expected [%O{expected} at 0], got [%s{rendered}]"
                            ]
                    )
            )

        if not failures.IsEmpty then
            failwith (String.Join ("\n", failures))

    /// The bytes of a `switch` instruction which declares `count` targets, followed by
    /// exactly `targetBytes`, which may be fewer than the `4 * count` the declaration calls for.
    let private switchBytes (count : uint32) (targetBytes : byte[]) : byte[] =
        let countBytes = Array.zeroCreate 4
        BinaryPrimitives.WriteUInt32LittleEndian (countBytes.AsSpan (), count)
        Array.concat [ [| byte OpCodes.Switch.Value |] ; countBytes ; targetBytes ]

    let private nops (n : int) : byte[] = Array.create n (byte OpCodes.Nop.Value)

    /// A well-formed body: some `nop`s, a `switch` over `targets`, and perhaps a `ret`.
    type private WellFormedSwitchBody =
        {
            Prefix : int
            Targets : int32 list
            TrailingRet : bool
        }

    let private wellFormedSwitchGen : Gen<WellFormedSwitchBody> =
        gen {
            let! prefix = Gen.choose (0, 3)
            let! count = Gen.frequency [ 1, Gen.constant 0 ; 4, Gen.choose (1, 64) ]
            let! targets = Gen.listOfLength count (Gen.choose (Int32.MinValue, Int32.MaxValue))
            let! trailingRet = Gen.elements [ true ; false ]

            return
                {
                    Prefix = prefix
                    Targets = targets
                    TrailingRet = trailingRet
                }
        }

    [<Test>]
    let ``a switch whose targets all fit decodes to exactly those targets`` () =
        // The guard against a truncated jump table must not refuse a table that ends exactly
        // where the body does, so make sure that shape is generated.
        let mutable endsTheBody = 0

        let property (body : WellFormedSwitchBody) : unit =
            let targetBytes = body.Targets |> List.map int32Bytes |> Array.concat

            let bytes =
                Array.concat
                    [
                        nops body.Prefix
                        switchBytes (uint32 body.Targets.Length) targetBytes
                        (if body.TrailingRet then
                             [| byte OpCodes.Ret.Value |]
                         else
                             [||])
                    ]

            if not body.TrailingRet then
                endsTheBody <- endsTheBody + 1

            let switchOffset = body.Prefix
            let afterSwitch = switchOffset + 5 + 4 * body.Targets.Length

            let expected =
                [
                    for i in 0 .. body.Prefix - 1 do
                        yield IlOp.Nullary NullaryIlOp.Nop, i
                    yield IlOp.Switch (ImmutableArray.CreateRange body.Targets), switchOffset
                    if body.TrailingRet then
                        yield IlOp.Nullary NullaryIlOp.Ret, afterSwitch
                ]

            let actual =
                IlDecoding.decodeInstructions (IlTokenUniverse.Metadata sourceAssembly) bytes

            List.length actual |> shouldEqual (List.length expected)

            List.zip expected actual
            |> List.iter (fun ((e, eOff), (a, aOff)) ->
                if not (opsEqual e a) then
                    failwith $"expected %O{e} at %i{eOff} but decoded %O{a} at %i{aOff}"

                aOff |> shouldEqual eOff
            )

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen wellFormedSwitchGen) property)
        endsTheBody |> shouldBeGreaterThan 300

    /// A `switch` whose operand is cut short: either within the four-byte count, or within
    /// the jump table the count declares.
    [<RequireQualifiedAccess>]
    type private Truncation =
        /// Only this many of the count's four bytes are present, and nothing after them.
        | WithinCount of present : int
        /// The count is present and declares this many targets, but only `presentBytes` of
        /// the `4 * count` bytes of jump table follow it.
        | WithinTargets of count : uint32 * presentBytes : byte[]

    type private TruncatedSwitchBody =
        {
            Prefix : int
            Truncation : Truncation
        }

    let private truncatedSwitchGen : Gen<TruncatedSwitchBody> =
        let countGen : Gen<uint32> =
            Gen.frequency
                [
                    // A table that would fit in memory, so a decoder that allocates it before
                    // checking gets as far as the truncated read.
                    2, Gen.choose (1, 64) |> Gen.map uint32
                    1, Gen.choose (65, 250_000) |> Gen.map uint32
                    // A table that is at least 2 GB, so a decoder that allocates it before
                    // checking either dies or exhausts the host.
                    2, Gen.choose (Int32.MaxValue / 4 + 1, Int32.MaxValue) |> Gen.map uint32
                    // Counts above Int32.MaxValue cannot be allocated at all.
                    1, Gen.choose (0, Int32.MaxValue) |> Gen.map (fun i -> uint32 i + 0x80000000u)
                    1, Gen.elements [ 0x7FFFFFFFu ; 0x80000000u ; 0xFFFFFFFFu ]
                ]

        gen {
            let! prefix = Gen.choose (0, 3)

            let! truncation =
                Gen.frequency
                    [
                        1, Gen.choose (0, 3) |> Gen.map Truncation.WithinCount
                        3,
                        gen {
                            let! count = countGen
                            let missingAtLeast = 1
                            let longestPrefix = min 64L (4L * int64 count - int64 missingAtLeast)
                            let! present = Gen.choose (0, int longestPrefix)
                            let! bytes = Gen.arrayOfLength present (Gen.choose (0, 255) |> Gen.map byte)
                            return Truncation.WithinTargets (count, bytes)
                        }
                    ]

            return
                {
                    Prefix = prefix
                    Truncation = truncation
                }
        }

    [<Test>]
    let ``a switch whose operand is cut short is refused as a malformed image`` () =
        let mutable withinCount = 0
        let mutable withinSmallTable = 0
        let mutable withinHugeTable = 0

        let property (body : TruncatedSwitchBody) : unit =
            let bytes =
                match body.Truncation with
                | Truncation.WithinCount present ->
                    withinCount <- withinCount + 1

                    Array.concat
                        [
                            nops body.Prefix
                            [| byte OpCodes.Switch.Value |]
                            Array.create present 0uy
                        ]
                | Truncation.WithinTargets (count, presentBytes) ->
                    if count > uint32 (Int32.MaxValue / 4) then
                        withinHugeTable <- withinHugeTable + 1
                    else
                        withinSmallTable <- withinSmallTable + 1

                    Array.concat [ nops body.Prefix ; switchBytes count presentBytes ]

            let outcome =
                try
                    IlDecoding.decodeInstructions (IlTokenUniverse.Metadata sourceAssembly) bytes
                    |> Ok
                with e ->
                    Error e

            match outcome, body.Truncation with
            | Ok decoded, _ ->
                let rendered =
                    decoded
                    |> List.map (fun (op, off) -> $"%O{op} at %i{off}")
                    |> String.concat "; "

                failwith $"a truncated switch decoded to [%s{rendered}]"
            | Error (:? BadImageFormatException), Truncation.WithinCount _ -> ()
            | Error (:? BadImageFormatException as e), Truncation.WithinTargets (count, presentBytes) ->
                // The diagnostic must name the instruction that was cut short, not merely say
                // that a read ran off the end.
                let message : string = e.Message

                for expected in
                    [
                        $"IL offset %i{body.Prefix}"
                        $"declares %u{count} targets"
                        $"%i{presentBytes.Length} bytes of IL remain"
                    ] do
                    if not (message.Contains expected) then
                        failwith $"the diagnostic %A{message} does not mention %A{expected}"
            | Error e, _ ->
                failwith
                    $"a truncated switch raised %s{e.GetType().FullName} rather than BadImageFormatException: %s{e.Message}"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen truncatedSwitchGen) property)
        withinCount |> shouldBeGreaterThan 200
        withinSmallTable |> shouldBeGreaterThan 200
        withinHugeTable |> shouldBeGreaterThan 200

    [<Test>]
    let ``a switch declaring Int32.MaxValue targets with none present is refused without allocating`` () =
        // The jump table this declares would be 8 GB; the decoder must refuse the instruction
        // from the count alone.
        let bytes = [| byte OpCodes.Switch.Value ; 0xFFuy ; 0xFFuy ; 0xFFuy ; 0x7Fuy |]

        let e =
            Assert.Throws<BadImageFormatException> (fun () ->
                IlDecoding.decodeInstructions (IlTokenUniverse.Metadata sourceAssembly) bytes
                |> ignore
            )

        e.Message |> shouldContainText "declares 2147483647 targets"

    [<Test>]
    let ``an empty body decodes to no instructions`` () =
        IlDecoding.decodeInstructions (IlTokenUniverse.Metadata sourceAssembly) [||]
        |> List.isEmpty
        |> shouldEqual true

    [<Test>]
    let ``a short-form local index above 127 keeps its value`` () =
        // `stloc.s 200` is the case that regressed: ECMA-335 III.3.63 gives the short form
        // an unsigned index, so slots 128..255 are reachable, and Roslyn emits it for any
        // method with that many locals.
        let bytes = [| byte OpCodes.Stloc_S.Value ; 200uy ; byte OpCodes.Ret.Value |]

        let expected =
            [
                IlOp.UnaryConst (UnaryConstIlOp.Stloc_s 200uy), 0
                IlOp.Nullary NullaryIlOp.Ret, 2
            ]

        let actual =
            IlDecoding.decodeInstructions (IlTokenUniverse.Metadata sourceAssembly) bytes

        List.length actual |> shouldEqual (List.length expected)

        List.zip expected actual
        |> List.iter (fun ((e, eOff), (a, aOff)) ->
            opsEqual e a |> shouldEqual true
            aOff |> shouldEqual eOff
        )

    /// Every method body in a real assembly, decoded from its bytes. The property is that
    /// the decoder and <see cref="IlOp.NumberOfBytes"/> — written separately, and the
    /// latter what the interpreter advances its program counter by — agree about how long
    /// every instruction is, and that the instructions tile the body exactly. A decoder
    /// that consumed too few or too many bytes for some operand would desynchronise and
    /// almost certainly go on to decode rubbish.
    let private assertDecodesCleanly (path : string) : unit =
        use stream = File.OpenRead path
        use peReader = new PEReader (stream)
        let metadataReader = peReader.GetMetadataReader ()
        let assemblyName = AssemblyName (Path.GetFileNameWithoutExtension path)

        let mutable bodies = 0
        let mutable instructions = 0

        for handle in metadataReader.MethodDefinitions do
            let methodDef = metadataReader.GetMethodDefinition handle

            if methodDef.RelativeVirtualAddress <> 0 then
                let body = peReader.GetMethodBody methodDef.RelativeVirtualAddress
                let ilBytes = body.GetILBytes ()

                let decoded =
                    IlDecoding.decodeInstructions (IlTokenUniverse.Metadata assemblyName) ilBytes

                let describe () =
                    let name = metadataReader.GetString methodDef.Name
                    $"%s{path}: %s{name} (RVA %i{methodDef.RelativeVirtualAddress})"

                match decoded with
                | [] -> failwith $"%s{describe ()}: %i{ilBytes.Length} bytes of IL decoded to no instructions"
                | _ ->

                bodies <- bodies + 1
                instructions <- instructions + List.length decoded

                let mutable expectedOffset = 0

                for op, offset in decoded do
                    if offset <> expectedOffset then
                        failwith
                            $"%s{describe ()}: instruction %O{op} is at offset %i{offset}, but the preceding instructions occupy %i{expectedOffset} bytes"

                    expectedOffset <- expectedOffset + IlOp.NumberOfBytes op

                if expectedOffset <> ilBytes.Length then
                    failwith
                        $"%s{describe ()}: instructions account for %i{expectedOffset} bytes of a %i{ilBytes.Length}-byte body"

        // Guard against the loop having silently found nothing to check.
        bodies |> shouldBeGreaterThan 1000
        instructions |> shouldBeGreaterThan 10000

    [<Test>]
    let ``every method body in CoreLib decodes to instructions that tile it exactly`` () =
        assertDecodesCleanly typeof<obj>.Assembly.Location

namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `StackEffect.ofInstruction` says, for every opcode, how many values it pops and what each
/// value it pushes is. Its oracle is CoreCLR's own opcode table, `opcode.def` in the pinned
/// runtime source, which gives each opcode's pops and the stack type of each push.
[<TestFixture>]
module TestStackEffect =

    let private requireRuntimeSrc () : string =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | dir -> dir

    /// One `OPDEF` row of `opcode.def`.
    type private Row =
        {
            Name : string
            Pop : string
            Push : string
            Operand : string
            Kind : string
            Encoding : byte list
        }

    let private rows () : Row list =
        let path =
            Path.Combine (requireRuntimeSrc (), "src", "coreclr", "inc", "opcode.def")

        let pattern =
            Regex
                @"^OPDEF\(\s*\w+\s*,\s*""([^""]+)""\s*,\s*([\w+]+)\s*,\s*([\w+]+)\s*,\s*(\w+)\s*,\s*(\w+)\s*,\s*(\d)\s*,\s*0x([0-9A-Fa-f]+)\s*,\s*0x([0-9A-Fa-f]+)\s*,"

        File.ReadAllLines path
        |> Array.choose (fun line ->
            let m = pattern.Match line

            if not m.Success then
                None
            else
                let length = int m.Groups.[6].Value
                let first = Convert.ToByte (m.Groups.[7].Value, 16)
                let second = Convert.ToByte (m.Groups.[8].Value, 16)

                Some
                    {
                        Name = m.Groups.[1].Value
                        Pop = m.Groups.[2].Value
                        Push = m.Groups.[3].Value
                        Operand = m.Groups.[4].Value
                        Kind = m.Groups.[5].Value
                        Encoding = if length = 1 then [ second ] else [ first ; second ]
                    }
        )
        |> List.ofArray

    /// Operand bytes of the given `opcode.def` operand kind: zeros, or a token of a table that
    /// kind names.
    let private operandBytes (operand : string) : byte list =
        let token (value : int) =
            BitConverter.GetBytes value |> List.ofArray

        match operand with
        | "InlineNone" -> []
        | "ShortInlineVar"
        | "ShortInlineI"
        | "ShortInlineBrTarget" -> [ 0uy ]
        | "InlineVar" -> [ 0uy ; 0uy ]
        | "InlineI"
        | "InlineBrTarget"
        | "ShortInlineR"
        | "InlineSwitch" -> List.replicate 4 0uy
        | "InlineI8"
        | "InlineR" -> List.replicate 8 0uy
        | "InlineMethod" -> token 0x0A000001
        | "InlineField" -> token 0x04000001
        | "InlineType"
        | "InlineTok" -> token 0x01000001
        | "InlineSig" -> token 0x11000001
        | "InlineString" -> token 0x70000001
        | other -> failwith $"opcode.def operand kind %s{other} has no encoding here"

    /// The `opcode.def` push kind each `Pushed` value is.
    let private pushKind (pushed : Pushed) : string =
        match pushed with
        | Pushed.Null
        | Pushed.String
        | Pushed.Indirect
        | Pushed.Element
        | Pushed.FromToken TokenValue.NewObject
        | Pushed.FromToken TokenValue.Cast
        | Pushed.FromToken TokenValue.Boxed
        | Pushed.FromToken TokenValue.NewArray -> "PushRef"
        | Pushed.Number StackNumber.Int32
        | Pushed.Number StackNumber.NativeInt
        | Pushed.Address
        | Pushed.ArgumentHandle
        | Pushed.TypedReferenceType
        | Pushed.FromToken TokenValue.Handle
        | Pushed.FromToken TokenValue.MethodPointer -> "PushI"
        | Pushed.Number StackNumber.Int64 -> "PushI8"
        | Pushed.Number StackNumber.Float32 -> "PushR4"
        | Pushed.Number StackNumber.Float64 -> "PushR8"
        | Pushed.Argument _
        | Pushed.Local _
        | Pushed.Operand _
        | Pushed.Arithmetic
        | Pushed.Bitwise
        | Pushed.FromToken TokenValue.Field
        | Pushed.FromToken TokenValue.Loaded
        | Pushed.FromToken TokenValue.TypedReference -> "Push1"
        | Pushed.FromToken TokenValue.CallResult -> "VarPush"

    /// What the table needs beyond the instruction: a callee taking three values and returning
    /// one, and a method returning a value.
    let private inputs : StackEffectInputs =
        {
            Arguments = 4
            Locals = 4
            ReturnsValue = true
            Callees =
                Map.ofList
                    [
                        0,
                        {
                            Arguments = 3
                            Returns = true
                        }
                    ]
        }

    /// Opcodes whose pushes `opcode.def` describes differently from what CoreCLR's importer does
    /// with them. `ckfinite` is `Pop1, PushR8` there, but the importer gives the result its
    /// operand's type (`case CEE_CKFINITE` in importer.cpp), so a float32 stays one. `isinst` is
    /// `PopRef, PushI` there, but ECMA-335 III.4.6 has it push an object reference or null, and
    /// the importer pushes a `TYP_REF` (`case CEE_ISINST`), as it does for `castclass`.
    let private importerDiffers : Set<string> = Set.ofList [ "ckfinite" ; "isinst" ]

    /// Every opcode `opcode.def` defines, decoded from its encoding, with what it pops and pushes.
    let private decoded () : (Row * IlOp) list =
        rows ()
        |> List.filter (fun row -> row.Name <> "unused" && row.Kind <> "IInternal")
        |> List.map (fun row ->
            let bytes = row.Encoding @ operandBytes row.Operand |> Array.ofList

            match
                IlDecoding.decodeInstructions (IlTokenUniverse.Metadata (Reflection.AssemblyName "Oracle")) bytes
            with
            | [ instruction, 0 ] -> row, instruction
            | other -> failwith $"%s{row.Name} decoded to %A{other}"
        )

    [<Test>]
    let ``every opcode pops what opcode.def says, and pushes values of the stack types it says`` () : unit =
        let decoded = decoded ()
        decoded.Length |> shouldBeGreaterThan 200

        let disagreements =
            [
                for row, instruction in decoded do
                    let effect =
                        match StackEffect.ofInstruction inputs (Map.ofList [ 0, instruction ]) 0 instruction with
                        | Ok effect -> effect
                        | Error e -> failwith $"%s{row.Name}: %O{e}"

                    let expectedPops =
                        match row.Pop with
                        | "Pop0" -> 0
                        | "VarPop" ->
                            match row.Name with
                            | "ret" -> 1
                            | "calli" -> 4
                            | _ -> 3
                        | pops -> pops.Split('+').Length

                    if effect.Pops <> expectedPops then
                        yield $"%s{row.Name} pops %d{effect.Pops}, opcode.def %s{row.Pop}"

                    let expectedPushes =
                        match row.Push with
                        | "Push0" -> []
                        | pushes -> pushes.Split ('+') |> List.ofArray

                    let pushes = effect.Pushes |> List.map pushKind

                    if not (importerDiffers.Contains row.Name) && pushes <> expectedPushes then
                        yield $"%s{row.Name} pushes %A{pushes}, opcode.def %s{row.Push}"
            ]

        disagreements |> shouldEqual []

    [<Test>]
    let ``what a value is made from is what the opcode pops`` () : unit =
        for row, instruction in decoded () do
            match StackEffect.ofInstruction inputs (Map.ofList [ 0, instruction ]) 0 instruction with
            | Error e -> failwith $"%s{row.Name}: %O{e}"
            | Ok effect ->
                for pushed in effect.Pushes do
                    match pushed with
                    // Through an address alone.
                    | Pushed.Indirect -> row.Pop |> shouldEqual "PopI"
                    // From an array and an index.
                    | Pushed.Element -> row.Pop |> shouldEqual "PopRef+PopI"
                    | Pushed.Arithmetic -> row.Pop |> shouldEqual "Pop1+Pop1"
                    | Pushed.Operand fromTop -> fromTop |> shouldBeSmallerThan effect.Pops
                    | Pushed.Argument _ -> row.Name.StartsWith "ldarg" |> shouldEqual true
                    | Pushed.Local _ -> row.Name.StartsWith "ldloc" |> shouldEqual true
                    | Pushed.Null -> row.Name |> shouldEqual "ldnull"
                    | Pushed.String -> row.Operand |> shouldEqual "InlineString"
                    | _ -> ()

    [<Test>]
    let ``a call pops its callee's arguments and pushes a result only if the callee returns one`` () : unit =
        let callee (arguments : int) (returns : bool) =
            { inputs with
                Callees =
                    Map.ofList
                        [
                            0,
                            {
                                Arguments = arguments
                                Returns = returns
                            }
                        ]
            }

        for row, instruction in decoded () do
            if row.Pop = "VarPop" && row.Name <> "ret" then
                for arguments, returns in [ 0, false ; 2, true ; 5, false ] do
                    match
                        StackEffect.ofInstruction
                            (callee arguments returns)
                            (Map.ofList [ 0, instruction ])
                            0
                            instruction
                    with
                    | Error e -> failwith $"%s{row.Name}: %O{e}"
                    | Ok effect ->
                        effect.Pops
                        |> shouldEqual (if row.Name = "calli" then arguments + 1 else arguments)

                        match row.Name with
                        | "newobj" -> effect.Pushes |> shouldEqual [ Pushed.FromToken TokenValue.NewObject ]
                        | _ ->
                            effect.Pushes
                            |> shouldEqual (
                                if returns then
                                    [ Pushed.FromToken TokenValue.CallResult ]
                                else
                                    []
                            )

                match
                    StackEffect.ofInstruction
                        { inputs with
                            Callees = Map.empty
                        }
                        (Map.ofList [ 0, instruction ])
                        0
                        instruction
                with
                | Error (StackShapeError.MissingTokenShape (0, _)) -> ()
                | other -> failwith $"%s{row.Name} with no callee arity gave %O{other}"

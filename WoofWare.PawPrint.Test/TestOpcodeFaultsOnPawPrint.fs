namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Text
open NUnit.Framework
open WoofWare.PawPrint

/// The instructions whose operands are all numbers, run under PawPrint on the boundary operands
/// that `TestOpcodeFaultsOnHost` runs on the host, and required to raise exactly what the host
/// raises on each.
///
/// For each instruction, the host's probe bodies (`NumericOpcodeGrid.probe`) are emitted as static
/// methods of a fabricated assembly, and a C# driver calls each on every operand list in the order
/// `TestOpcodeFaultsOnHost` enumerates them, printing the index and exception type of every call
/// that raised. The in-process probes are the oracle. The interpreter already checks that every
/// fault it raises is one `OpcodeFaults` lists; this is the other direction, that it raises a fault
/// wherever the host does.
///
/// Only faults are compared, not the values computed; `TestNullaryIlOp` checks those.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Each case runs a guest under the interpreter, about 1.5 minutes of CPU across the fixture;
// `Explicit` keeps it out of a bare `dotnet test`, and CI selects it by category. See AGENTS.md.
[<Category("Guest")>]
[<Explicit>]
module TestOpcodeFaultsOnPawPrint =

    /// Instructions on which PawPrint is known to disagree with the host on every runtime, with the
    /// disagreement. Each is parked until the interpreter is fixed. Every disagreement so far is a
    /// refusal: PawPrint stops the run where the host completes or raises, rather than raising
    /// something else. `parkedOn` folds these into every runtime's parks.
    let private parkedEverywhere : Map<NullaryIlOp, string> = Map.empty

    /// The instructions parked on `runtime`, each with its disagreement.
    let private parkedOn (runtime : EmulatedRuntime) : Map<NullaryIlOp, string> =
        match runtime with
        | EmulatedRuntime.Net10 -> parkedEverywhere
        // Measured: every instruction's driver stops at the thin lock.
        | EmulatedRuntime.Net11 ->
            NumericOpcodeGrid.instructions
            |> List.map (fun (op, _, _) -> op, Net11Frontier.thinLock)
            |> Map.ofList
            |> GuestTable.fold "Net11 parks" parkedEverywhere

    let private csType (operand : NumericOpcodeGrid.Operand) : string =
        match operand with
        | NumericOpcodeGrid.Operand.Int32 -> "int"
        | NumericOpcodeGrid.Operand.Int64 -> "long"
        | NumericOpcodeGrid.Operand.NativeInt -> "nint"
        | NumericOpcodeGrid.Operand.Float32 -> "float"
        | NumericOpcodeGrid.Operand.Float64 -> "double"

    /// A C# expression for exactly `value`, spelled through its bits so that no literal is rounded.
    let private literal (value : obj) : string =
        match value with
        | :? int32 as v -> $"unchecked((int)0x%08X{uint32 v}U)"
        | :? int64 as v -> $"unchecked((long)0x%016X{uint64 v}UL)"
        | :? nativeint as v -> $"(nint)unchecked((long)0x%016X{uint64 (int64 v)}UL)"
        | :? float32 as v ->
            $"BitConverter.Int32BitsToSingle(unchecked((int)0x%08X{uint32 (BitConverter.SingleToInt32Bits v)}U))"
        | :? float as v ->
            $"BitConverter.Int64BitsToDouble(unchecked((long)0x%016X{uint64 (BitConverter.DoubleToInt64Bits v)}UL))"
        | other -> failwith $"no C# literal for %O{other.GetType ()}"

    let private operandName (operand : NumericOpcodeGrid.Operand) : string = $"%O{operand}"

    /// `P::M<k>`, the probe body for the instruction's `k`th signature, for every `k`.
    let private fabricate
        (opcode : OpCode)
        (signatures : (NumericOpcodeGrid.Operand list * NumericOpcodeGrid.Operand) list)
        : byte[]
        =
        let builder = PersistedAssemblyBuilder (AssemblyName "P", typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule "P"

        let p =
            modul.DefineType ("P", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        signatures
        |> List.iteri (fun k (operands, result) ->
            let method =
                p.DefineMethod (
                    $"M%d{k}",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    NumericOpcodeGrid.hostType result,
                    operands |> List.map NumericOpcodeGrid.hostType |> Array.ofList
                )

            let il = method.GetILGenerator ()

            for i in 0 .. operands.Length - 1 do
                il.Emit (OpCodes.Ldarg, int16 i)

            il.Emit opcode
            il.Emit OpCodes.Ret
        )

        p.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Calls `P::M<k>` on every operand list of signature `k`, operand by operand from the boundary
    /// arrays, first operand outermost, numbering the calls from zero across all signatures. Each
    /// result is stored to a static field, which the JIT cannot discard. It prints `<index>
    /// <exception type>` for each call that raised, then `cases <count>`.
    let private driverSource (signatures : (NumericOpcodeGrid.Operand list * NumericOpcodeGrid.Operand) list) : string =
        let source = StringBuilder ()

        let line (text : string) : unit =
            source.AppendLine text |> ignore<StringBuilder>

        line "using System;"
        line "using System.Text;"
        line "public static class Driver"
        line "{"

        line
            "    static int resultInt32; static long resultInt64; static nint resultNativeInt; static float resultFloat32; static double resultFloat64;"

        for operand in signatures |> List.collect fst |> List.distinct do
            let values =
                NumericOpcodeGrid.boundaries operand |> List.map literal |> String.concat ", "

            line $"    static readonly %s{csType operand}[] Values%s{operandName operand} = {{ %s{values} }};"

        line "    public static int Main(string[] args)"
        line "    {"
        line "        var raised = new StringBuilder();"
        line "        int index = 0;"

        signatures
        |> List.iteri (fun k (operands, result) ->
            let loops =
                operands
                |> List.mapi (fun i operand -> $"foreach (var a%d{i} in Values%s{operandName operand})")
                |> String.concat " "

            let arguments = operands |> List.mapi (fun i _ -> $"a%d{i}") |> String.concat ", "

            line
                $"        %s{loops} {{ try {{ result%s{operandName result} = P.M%d{k}(%s{arguments}); }} catch (Exception e) {{ raised.Append(index).Append(' ').Append(e.GetType().FullName).Append('\\n'); }} index++; }}"
        )

        line "        raised.Append(\"cases \").Append(index).Append('\\n');"
        line "        Console.Write(raised.ToString());"
        line "        return 0;"
        line "    }"
        line "}"
        source.ToString ()

    /// What the driver printed: the index and exception type of each call that raised, and the
    /// number of calls.
    let private parseDriverOutput (stdout : string) : Map<int, string> * int =
        let lines =
            stdout.Split ('\n', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

        match List.rev lines with
        | last :: raisedLines when last.StartsWith "cases " ->
            let raised =
                raisedLines
                |> List.rev
                |> List.map (fun line ->
                    match line.Split ' ' with
                    | [| index ; name |] -> Int32.Parse index, name
                    | _ -> failwith $"unparseable driver line: %s{line}"
                )
                |> Map.ofList

            raised, Int32.Parse (last.Substring "cases ".Length)
        | _ -> failwith $"the driver's output did not end with its case count:\n%s{stdout}"

    let private describe (raised : string option) : string = raised |> Option.defaultValue "nothing"

    let instructions : TestCaseData list =
        NumericOpcodeGrid.instructions
        |> List.map (fun (op, _, _) ->
            let case =
                (TestCaseData [| box op |]).SetName $"PawPrint raises what the host raises: %O{op}"

            match (parkedOn (FrameworkUnderTest.runtime ())).TryFind op with
            | Some reason -> case.Explicit reason
            | None -> case
        )

    [<TestCaseSource(nameof instructions)>]
    let ``on boundary operands, PawPrint raises exactly what the host raises`` (op : NullaryIlOp) : unit =
        let opcode, typing =
            NumericOpcodeGrid.instructions
            |> List.pick (fun (candidate, opcode, typing) -> if candidate = op then Some (opcode, typing) else None)

        let signatures = NumericOpcodeGrid.signatures typing

        let calls =
            [
                for operands, result in signatures do
                    for arguments in HostFaultProbe.cartesian (List.map NumericOpcodeGrid.boundaries operands) do
                        operands,
                        arguments,
                        HostFaultProbe.raised (NumericOpcodeGrid.probe opcode operands result) arguments
            ]

        let stdout =
            FabricatedGuest.stdoutOnPawPrint "P" (fabricate opcode signatures) "FaultDriver" (driverSource signatures)

        let raisedOnPawPrint, count = parseDriverOutput stdout

        if count <> calls.Length then
            failwith $"the driver made %d{count} calls, but the host made %d{calls.Length}"

        let mismatches =
            calls
            |> List.indexed
            |> List.choose (fun (index, (operands, arguments, onHost)) ->
                let onPawPrint = raisedOnPawPrint.TryFind index

                if onHost = onPawPrint then
                    None
                else
                    Some
                        $"%O{op} %A{operands} on %A{arguments}: the host raised %s{describe onHost}, PawPrint %s{describe onPawPrint}"
            )

        match mismatches with
        | [] -> ()
        | mismatches ->
            let shown = List.truncate 40 mismatches

            failwith (
                String.concat
                    Environment.NewLine
                    ($"%d{mismatches.Length} of %d{calls.Length} calls disagree; the first %d{shown.Length}:"
                     :: shown)
            )

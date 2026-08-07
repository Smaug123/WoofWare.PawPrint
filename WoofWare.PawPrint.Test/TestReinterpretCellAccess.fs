namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// A differential sweep over reading and writing through a *reinterpreting byref*, taking the real
/// runtime as the oracle.
///
/// `buffer[k]` on an `[InlineArray(N)]` lowers to
/// `Unsafe.Add(ref Unsafe.As&lt;TBuffer, TElem&gt;(ref buffer), k)`, and `buffer[k].Field` walks one step
/// further in. When the element contains an object reference the storage has no byte image at all,
/// so PawPrint cannot serve those accesses bytewise and instead names the storage cell the byte
/// range picks out (`CliType.CellPathsExactlyCovering`). Whether that naming lands on the right
/// cell depends on where the runtime chose to put each field — and auto layout *reorders* fields,
/// promoting references — so the shapes below vary the reference's declared position, count, and
/// nesting depth rather than assuming one arrangement.
///
/// Each program returns a checksum that depends on every field of every slot, so a read or write
/// that lands one cell over changes the answer rather than going unnoticed.
///
/// The `noReferences` shape is a control: its storage *is* byte-addressable, so it must keep going
/// through the bytewise path. It is here to catch the opposite failure — cell naming quietly taking
/// over accesses that the byte path already served correctly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestReinterpretCellAccess =

    /// An element type to build inline arrays over.
    type private ElementShape =
        {
            /// Declarations the element needs, including the element struct itself, named `Elem`.
            Declarations : string
            /// Expression building an `Elem` from the `int` seed `SEED`.
            Make : string
            /// Expression scoring the `Elem`-valued expression `ELEM` to an `int`.
            Score : string
            /// Expression scoring one field read *directly* through the indexer on `BUF` at index
            /// `IDX`, rather than by copying the whole element out first. This is the access that
            /// descends into a cell rather than naming the slot itself.
            FieldProbe : string
        }

    let private elementShapes : Map<string, ElementShape> =
        [
            // Reference declared last. Auto layout promotes it to offset 0, so `Tag` ends up
            // *after* it — the arrangement that makes a field probe a genuine descent to a
            // non-zero offset rather than a whole-cell read.
            "refLastDeclared",
            {
                Declarations =
                    "private sealed class Box { public int V; } private struct Elem { public byte Tag; public Box Payload; }"
                Make = "new Elem { Tag = (byte)(SEED + 1), Payload = new Box { V = (SEED + 1) * 10 } }"
                Score = "((ELEM).Tag * 7 + ((ELEM).Payload == null ? -1 : (ELEM).Payload.V))"
                FieldProbe = "(BUF[IDX].Tag * 13 + (BUF[IDX].Payload == null ? -1 : BUF[IDX].Payload.V))"
            }
            // Reference declared first. Same promoted layout, different declaration order, so a
            // resolver keyed off declaration order rather than laid-out offset would diverge here
            // but not above.
            "refFirstDeclared",
            {
                Declarations =
                    "private sealed class Box { public int V; } private struct Elem { public Box Payload; public byte Tag; }"
                Make = "new Elem { Payload = new Box { V = (SEED + 1) * 10 }, Tag = (byte)(SEED + 1) }"
                Score = "((ELEM).Tag * 7 + ((ELEM).Payload == null ? -1 : (ELEM).Payload.V))"
                FieldProbe = "(BUF[IDX].Tag * 13 + (BUF[IDX].Payload == null ? -1 : BUF[IDX].Payload.V))"
            }
            // Two references: both promoted, so the second sits at a non-zero offset and naming it
            // requires the offset arithmetic to be right rather than merely non-zero.
            "twoReferences",
            {
                Declarations =
                    "private sealed class Box { public int V; } private struct Elem { public Box A; public Box B; }"
                Make = "new Elem { A = new Box { V = SEED + 1 }, B = new Box { V = (SEED + 1) * 100 } }"
                Score = "(((ELEM).A == null ? -1 : (ELEM).A.V) * 7 + ((ELEM).B == null ? -1 : (ELEM).B.V))"
                FieldProbe =
                    "((BUF[IDX].A == null ? -1 : BUF[IDX].A.V) * 13 + (BUF[IDX].B == null ? -1 : BUF[IDX].B.V))"
            }
            // A reference among wider primitives: the promoted reference displaces an `int` and a
            // `long`, so every field lands at an offset that depends on the promotion having
            // happened.
            "refAmongWiderPrimitives",
            {
                Declarations =
                    "private sealed class Box { public int V; } private struct Elem { public int X; public Box P; public long Y; }"
                Make = "new Elem { X = SEED + 1, P = new Box { V = SEED + 2 }, Y = (SEED + 3) * 1000L }"
                Score = "((ELEM).X * 7 + ((ELEM).P == null ? -1 : (ELEM).P.V) * 3 + (int)((ELEM).Y))"
                FieldProbe = "(BUF[IDX].X * 13 + (BUF[IDX].P == null ? -1 : BUF[IDX].P.V) * 5 + (int)(BUF[IDX].Y))"
            }
            // Control: no references anywhere, so the storage *is* byte-addressable and these
            // accesses must continue to be served bytewise. If cell naming started taking these
            // over, this row would be the one that noticed.
            "noReferences",
            {
                Declarations = "private struct Elem { public byte Tag; public int X; public long Y; }"
                Make = "new Elem { Tag = (byte)(SEED + 1), X = SEED + 2, Y = (SEED + 3) * 1000L }"
                Score = "((ELEM).Tag * 7 + (ELEM).X * 3 + (int)((ELEM).Y))"
                FieldProbe = "(BUF[IDX].Tag * 13 + BUF[IDX].X * 5 + (int)(BUF[IDX].Y))"
            }
        ]
        |> Map.ofList

    let private cases : string[] =
        elementShapes |> Map.toSeq |> Seq.map fst |> Array.ofSeq

    /// Three slots: enough that a slot-stride error shows up as a wrong answer rather than as an
    /// aliased pair that happens to cancel.
    let private source (shape : ElementShape) : string =
        let template =
            """
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class Probe
{
    DECLARATIONS

    [InlineArray(3)]
    private struct Buf
    {
        private Elem _item;
    }

    public static int Main()
    {
        Buf buf = default;

        // Before any write: every slot must read back as default through the indexer. A read that
        // named the wrong cell would most likely still be "some default" here, which is why the
        // written values below are all distinct and position-dependent.
        int acc = 0;
        acc = acc * 31 + SCORE0_PRE;
        acc = acc * 31 + SCORE1_PRE;
        acc = acc * 31 + SCORE2_PRE;

        // Writes through the indexer, at constant indices.
        buf[0] = MAKE0;
        buf[1] = MAKE1;
        buf[2] = MAKE2;

        // Whole-element reads back through the indexer.
        acc = acc * 31 + SCORE0;
        acc = acc * 31 + SCORE1;
        acc = acc * 31 + SCORE2;

        // Field reads directly through the indexer: these descend into the slot rather than
        // copying it out first.
        acc = acc * 31 + PROBE0;
        acc = acc * 31 + PROBE1;
        acc = acc * 31 + PROBE2;

        // Overwrite the middle slot only; its neighbours must be undisturbed.
        buf[1] = MAKE9;
        acc = acc * 31 + SCORE0;
        acc = acc * 31 + SCORE1;
        acc = acc * 31 + SCORE2;

        return acc;
    }
}
"""

        let elemAt (i : int) = $"buf[%d{i}]"

        template
            .Replace("DECLARATIONS", shape.Declarations)
            .Replace("MAKE0", shape.Make.Replace ("SEED", "0"))
            .Replace("MAKE1", shape.Make.Replace ("SEED", "1"))
            .Replace("MAKE2", shape.Make.Replace ("SEED", "2"))
            .Replace("MAKE9", shape.Make.Replace ("SEED", "9"))
            .Replace("SCORE0_PRE", shape.Score.Replace ("ELEM", elemAt 0))
            .Replace("SCORE1_PRE", shape.Score.Replace ("ELEM", elemAt 1))
            .Replace("SCORE2_PRE", shape.Score.Replace ("ELEM", elemAt 2))
            .Replace("SCORE0", shape.Score.Replace ("ELEM", elemAt 0))
            .Replace("SCORE1", shape.Score.Replace ("ELEM", elemAt 1))
            .Replace("SCORE2", shape.Score.Replace ("ELEM", elemAt 2))
            .Replace("PROBE0", shape.FieldProbe.Replace("BUF", "buf").Replace ("IDX", "0"))
            .Replace("PROBE1", shape.FieldProbe.Replace("BUF", "buf").Replace ("IDX", "1"))
            .Replace ("PROBE2", shape.FieldProbe.Replace("BUF", "buf").Replace ("IDX", "2"))

    let private assy = typeof<RunResult>.Assembly

    let private runUnderPawPrint (sourceName : string) (image : byte[]) : int =
        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let outcome =
            try
                Program.run
                    loggerFactory
                    (Some sourceName)
                    peImage
                    { HostConfig.Default dotnetRuntimes with
                        Kernel = KernelConfig.Default
                    }
            with _ ->
                for message in messages () do
                    Console.Error.WriteLine $"{message}"

                reraise ()

        match outcome with
        | RunOutcome.NormalExit (terminalState, terminatingThread)
        | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> i
            | [] -> failwith $"%s{sourceName}: expected the program to return an int, but it returned void"
            | ret :: _ -> failwith $"%s{sourceName}: expected the program to return an int, but it returned %O{ret}"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"%s{sourceName}: guest threw an unhandled exception: %O{exn.ExceptionObject}"
        | RunOutcome.FailFast (_, _, message) ->
            let message = message |> Option.defaultValue "<no message>"
            failwith $"%s{sourceName}: guest called Environment.FailFast: %s{message}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"%s{sourceName}: guest was terminated by POSIX signal %O{signal}"

    [<TestCaseSource(nameof cases)>]
    let ``Reinterpreting byref access matches the real runtime`` (case : string) : unit =
        let text = source elementShapes.[case]
        let image = Roslyn.compile [ text ]

        let expected =
            match RealRuntime.executeWithRealRuntime [||] image with
            | RealRuntimeResult.NormalExit exitCode -> exitCode
            | RealRuntimeResult.UnhandledException exn ->
                failwith $"%s{case}: real runtime threw unhandled %s{exn.GetType().Name}: %s{exn.Message}"

        let actual = runUnderPawPrint case image

        if actual <> expected then
            failwith
                $"%s{case}: PawPrint and the real runtime disagree on the access checksum.\n  real runtime: %d{expected}\n  PawPrint:     %d{actual}\n\nSource:\n%s{text}"

        actual |> shouldEqual expected

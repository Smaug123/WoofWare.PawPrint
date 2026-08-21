namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// A differential sweep over the *bulk byte-range* primitives — `Span<T>.CopyTo`, which bottoms out
/// in `Buffer.Memmove` and, for `T` containing references, in
/// `Buffer.BulkMoveWithWriteBarrierInternal` — taking the real runtime as the oracle.
///
/// PawPrint stores values as typed cells rather than as flat bytes, so `CellAwareMemOps` serves
/// these by walking the range and taking whole typed cells where it can, falling back to a byte
/// walk otherwise. Storage containing object references has no byte image at all, so for it the
/// cell step is not an optimisation but the only route: what the sweep is really checking is that
/// the cell the step picks is the right one, for cursors that land at every offset of every shape
/// below.
///
/// The shapes vary how deeply the cell that should move is buried and what surrounds it: a slot of
/// an `[InlineArray(N)]` (the whole buffer is one indivisible cell, so a single-element copy is a
/// strict sub-range of it), a plain field, a field of a field, and a field of a field alongside a
/// byte-addressable sibling. The `noReference*` rows are controls: their storage *is*
/// byte-addressable, so those copies must keep going through the byte path, and they are here to
/// catch cell naming quietly taking over moves that already worked.
///
/// Each scenario returns a checksum depending on every slot, so a move landing one cell over
/// changes the answer rather than going unnoticed. Scenarios are grouped in threes because a real
/// process's exit code on Unix is only 8 bits: PawPrint runs once and returns all three groups
/// packed into an int32, while the oracle is run once per group.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBulkMoveCellAccess =

    /// A storage shape to copy in and out of. `Declarations` must define the element type, a
    /// four-slot buffer type `Buf`, and the three accessors the template calls: `Slot`, `SetSlot`
    /// and `SpanOf`. Keeping those per shape is what lets the template read a buffer back by
    /// ordinary field access — a route with no bulk move in it — so the checksum measures the copy
    /// rather than measuring itself.
    type private BufferShape =
        {
            Declarations : string
            /// Expression building an element from the `int` seed `SEED`.
            Make : string
            /// Expression scoring the element-valued expression `ELEM` to an `int`.
            ScoreElem : string
        }

    /// The four slots as named fields, reached by ordinary `ldfld`, with the span built by
    /// reinterpreting the buffer as its element type.
    let private fieldBuffer (elementDeclarations : string) (elementType : string) : string =
        $"""
    %s{elementDeclarations}

    private struct Buf {{ public %s{elementType} A; public %s{elementType} B; public %s{elementType} C; public %s{elementType} D; }}

    private static %s{elementType} Slot(ref Buf b, int i) => i switch {{ 0 => b.A, 1 => b.B, 2 => b.C, _ => b.D }};

    private static void SetSlot(ref Buf b, int i, %s{elementType} v)
    {{
        switch (i) {{ case 0: b.A = v; break; case 1: b.B = v; break; case 2: b.C = v; break; default: b.D = v; break; }}
    }}

    private static Span<%s{elementType}> SpanOf(ref Buf b) => MemoryMarshal.CreateSpan(ref Unsafe.As<Buf, %s{elementType}>(ref b), 4);
"""

    /// The four slots as an `[InlineArray(4)]`, where PawPrint models the whole buffer as one cell
    /// and a slot is a sub-cell of it.
    let private inlineArrayBuffer (elementDeclarations : string) (elementType : string) : string =
        $"""
    %s{elementDeclarations}

    [InlineArray(4)]
    private struct Buf {{ private %s{elementType} _item; }}

    private static %s{elementType} Slot(ref Buf b, int i) => b[i];

    private static void SetSlot(ref Buf b, int i, %s{elementType} v) {{ b[i] = v; }}

    private static Span<%s{elementType}> SpanOf(ref Buf b) => b;
"""

    let private box = "private sealed class Box { public int V; }"

    let private shapes : Map<string, BufferShape> =
        [
            // The report's shape: an inline array over a bare object reference. The buffer is one
            // 32-byte cell with no byte image, so copying a single 8-byte element out of it needs
            // the slot naming rather than a whole-cell move.
            "inlineArrayOfReferences",
            {
                Declarations = inlineArrayBuffer box "object"
                Make = "(object)new Box { V = SEED + 1 }"
                ScoreElem = "((ELEM) == null ? -1 : ((Box)(ELEM)).V)"
            }
            // The deepest shape: inline-array slots whose element is a struct *containing* a
            // reference. The buffer is one indivisible 64-byte cell with no byte image, a slot is a
            // 16-byte sub-cell of it, and auto layout promotes the reference so `Tag` sits at a
            // non-zero offset *within* the slot — two levels of descent from the storage the byref
            // names.
            "inlineArrayOfReferenceStructs",
            {
                Declarations = inlineArrayBuffer $"%s{box} private struct W {{ public byte Tag; public Box P; }}" "W"
                Make = "new W { Tag = (byte)(SEED + 1), P = new Box { V = SEED + 1 } }"
                ScoreElem = "((ELEM).Tag * 7 + ((ELEM).P == null ? -1 : (ELEM).P.V))"
            }
            // The same element, but as four declared fields rather than inline-array slots: the
            // cells are the same width and at the same offsets, reached by a different route.
            "referenceFields",
            {
                Declarations = fieldBuffer box "Box"
                Make = "new Box { V = SEED + 1 }"
                ScoreElem = "((ELEM) == null ? -1 : (ELEM).V)"
            }
            // One level deeper: each slot is a struct wrapping the reference, so the cell that
            // moves and the cell that encloses it have the same extent. Both are legitimate
            // answers, and the step must not be confused by there being two.
            "nestedReferenceFields",
            {
                Declarations = fieldBuffer $"%s{box} private struct W {{ public Box P; }}" "W"
                Make = "new W { P = new Box { V = SEED + 1 } }"
                ScoreElem = "((ELEM).P == null ? -1 : (ELEM).P.V)"
            }
            // A reference beside a byte-addressable sibling. Auto layout promotes the reference, so
            // `N` sits at a non-zero offset inside each slot: the enclosing cell has no byte image
            // while one of its children does, which is the shape where a cell step and a byte step
            // have to interleave correctly.
            "mixedReferenceFields",
            {
                Declarations = fieldBuffer $"%s{box} private struct W {{ public Box P; public long N; }}" "W"
                Make = "new W { P = new Box { V = SEED + 1 }, N = (SEED + 2) * 1000L }"
                ScoreElem = "((((ELEM).P == null) ? -1 : (ELEM).P.V) * 7 + (int)((ELEM).N))"
            }
            // Control: an inline array with no references anywhere, so its storage *is*
            // byte-addressable and these copies must keep going through the byte path.
            "noReferenceInlineArray",
            {
                Declarations = inlineArrayBuffer "" "long"
                Make = "(SEED + 1) * 1000L"
                ScoreElem = "(int)(ELEM)"
            }
            // Control, field-rooted: the same, reached the way the reference shapes above are.
            "noReferenceFields",
            {
                Declarations = fieldBuffer "" "long"
                Make = "(SEED + 1) * 1000L"
                ScoreElem = "(int)(ELEM)"
            }
        ]
        |> Map.ofList

    let private cases : string[] = shapes |> Map.toSeq |> Seq.map fst |> Array.ofSeq

    let private source (shape : BufferShape) : string =
        let template =
            """
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestBulkMoveCellAccessSweep
{
    DECLARATIONS

    private sealed class Holder { public Buf B; }

    private static Buf StaticBuf;

    private static int Mix(int acc, int x) => acc * 31 + x;

    private static int ScoreBuf(ref Buf b)
    {
        int acc = 0;
        for (int i = 0; i < 4; i++) acc = Mix(acc, SCORE_SLOT);
        return acc;
    }

    private static int ScoreArr(ELEMTYPE[] arr)
    {
        int acc = 0;
        for (int i = 0; i < arr.Length; i++) acc = Mix(acc, SCORE_ARR);
        return acc;
    }

    private static void Fill(ref Buf b, int seedBase)
    {
        for (int i = 0; i < 4; i++) SetSlot(ref b, i, MAKE_LOOP);
    }

    // Reached through an *argument* root. The buffer must be passed by value: a `ref Buf`
    // parameter carries the caller's byref, so it would root at the caller's local and this would
    // silently be the same case as everything above.
    private static int ViaArg(Buf b, ELEMTYPE[] arr)
    {
        Array.Clear(arr);
        SpanOf(ref b).CopyTo(arr);
        return ScoreArr(arr);
    }

    // Copies that cross between two distinct storages.
    private static int Group0()
    {
        ELEMTYPE[] arr = new ELEMTYPE[4];
        int acc = 0;

        // Whole buffer out to an array.
        Buf b1 = default;
        Fill(ref b1, 0);
        SpanOf(ref b1).CopyTo(arr);
        acc = Mix(acc, ScoreArr(arr));

        // One element out of the middle: a strict sub-range of the buffer's storage, and the
        // report's own case.
        Array.Clear(arr);
        SpanOf(ref b1).Slice(1, 1).CopyTo(arr.AsSpan(0, 1));
        acc = Mix(acc, ScoreArr(arr));

        // A run from the middle to the middle: neither cursor starts at its storage's origin, so
        // the two intra-cell offsets differ throughout.
        Array.Clear(arr);
        SpanOf(ref b1).Slice(1, 2).CopyTo(arr.AsSpan(2, 2));
        acc = Mix(acc, ScoreArr(arr));

        // Array back into a buffer: the descending side is now the destination.
        Buf b2 = default;
        for (int i = 0; i < 4; i++) arr[i] = MAKE_ARR;
        arr.AsSpan().CopyTo(SpanOf(ref b2));
        acc = Mix(acc, ScoreBuf(ref b2));

        // One element from one buffer to another. Both sides could name a cell far wider than the
        // copy — the whole buffer, at the same offset, of the same type — so this is where a step
        // that forgot to cap its width by the bytes remaining would overrun and take all four
        // slots. The two buffers are seeded differently so that overrunning shows up.
        Buf b3 = default;
        Fill(ref b3, 100);
        Buf b4 = default;
        Fill(ref b4, 200);
        SpanOf(ref b3).Slice(0, 1).CopyTo(SpanOf(ref b4).Slice(0, 1));
        acc = Mix(acc, ScoreBuf(ref b4));

        return acc;
    }

    // Copies within one storage, where direction matters, plus zeroing.
    private static int Group1()
    {
        int acc = 0;

        // Overlapping shift down: destination before source, so the loop runs forwards.
        Buf b3 = default;
        Fill(ref b3, 20);
        SpanOf(ref b3).Slice(1, 3).CopyTo(SpanOf(ref b3).Slice(0, 3));
        acc = Mix(acc, ScoreBuf(ref b3));

        // Overlapping shift up: destination after source, so the loop runs backwards and the
        // cursor is each move's *last* byte rather than its first.
        Buf b4 = default;
        Fill(ref b4, 30);
        SpanOf(ref b4).Slice(0, 3).CopyTo(SpanOf(ref b4).Slice(1, 3));
        acc = Mix(acc, ScoreBuf(ref b4));

        // A single slot moved backwards within one storage: both cursors sit inside the *same*
        // cell at *different* offsets, which is the case a step requiring one shared intra-cell
        // offset cannot serve.
        Buf b5 = default;
        Fill(ref b5, 40);
        SpanOf(ref b5).Slice(2, 1).CopyTo(SpanOf(ref b5).Slice(0, 1));
        acc = Mix(acc, ScoreBuf(ref b5));

        // Zeroing a sub-range. `tryWholeCellZeroAt` gates on the same root predicate, so this is
        // cover for the reclassification on the zeroing side.
        Buf b7 = default;
        Fill(ref b7, 80);
        SpanOf(ref b7).Slice(1).Clear();
        acc = Mix(acc, ScoreBuf(ref b7));

        return acc;
    }

    // The same copy from each root a buffer can sit in.
    private static int Group2()
    {
        ELEMTYPE[] arr = new ELEMTYPE[4];
        int acc = 0;

        // Static-field root.
        Fill(ref StaticBuf, 50);
        Array.Clear(arr);
        SpanOf(ref StaticBuf).CopyTo(arr);
        acc = Mix(acc, ScoreArr(arr));

        // Argument root.
        Buf b6 = default;
        Fill(ref b6, 60);
        acc = Mix(acc, ViaArg(b6, arr));

        // Heap-object-field root.
        Holder h = new Holder();
        Fill(ref h.B, 70);
        Array.Clear(arr);
        SpanOf(ref h.B).CopyTo(arr);
        acc = Mix(acc, ScoreArr(arr));

        return acc;
    }

    // 251 is the largest prime an exit code can carry, and folding into it keeps every group
    // comparable against a real process.
    private static int Reduce(int x) => ((x % 251) + 251) % 251;

    public static int Main(string[] argv)
    {
        if (argv.Length == 0) return Reduce(Group0()) | (Reduce(Group1()) << 8) | (Reduce(Group2()) << 16);
        if (argv[0] == "0") return Reduce(Group0());
        if (argv[0] == "1") return Reduce(Group1());
        return Reduce(Group2());
    }
}
"""

        // `ELEMTYPE` is read off the generated `SpanOf`, so a shape cannot get it out of step with
        // its own declarations.
        let elementType =
            let marker = "private static Span<"

            let start = shape.Declarations.IndexOf (marker, StringComparison.Ordinal)

            if start < 0 then
                failwith "shape declarations must define SpanOf, from which the element type is read"

            let from = start + marker.Length
            let stop = shape.Declarations.IndexOf ('>', from)
            shape.Declarations.Substring (from, stop - from)

        template
            .Replace("DECLARATIONS", shape.Declarations)
            .Replace("SCORE_SLOT", shape.ScoreElem.Replace ("ELEM", "Slot(ref b, i)"))
            .Replace("SCORE_ARR", shape.ScoreElem.Replace ("ELEM", "arr[i]"))
            .Replace("MAKE_LOOP", shape.Make.Replace ("SEED", "(seedBase + i)"))
            .Replace("MAKE_ARR", shape.Make.Replace ("SEED", "(i + 10)"))
            .Replace ("ELEMTYPE", elementType)

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
                Program.run loggerFactory (Some sourceName) peImage (HostConfig.Default dotnetRuntimes)
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
        | RunOutcome.Aborted (_, _, fatal) ->
            let message = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"%s{sourceName}: guest aborted (%O{fatal.Code}): %s{message}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"%s{sourceName}: guest was terminated by POSIX signal %O{signal}"

    /// A same-width copy whose two ends are differently *typed* — `long` cells into `double` cells,
    /// which `MemoryMarshal.Cast` makes ordinary. Both sides are byte-addressable, so the byte path
    /// serves it and the reinterpreted bit patterns must come back out unchanged.
    ///
    /// Cell-aware classification of stack slots is what brings this shape to the cell step at
    /// all, so this test is cover for that classification rather than an assertion about any
    /// one guard.
    ///
    /// In particular it does **not** pin `cellsHaveCompatibleShape`, and that is measured, not
    /// assumed: the guard fires on exactly this copy (`Numeric Int64` against
    /// `Numeric Float64`), yet removing it changes no answer here, because PawPrint's read path
    /// carries the 64-bit payload through either cell shape. The guard refuses shapes the real
    /// runtime accepts, so no differential test can distinguish it — the same position #797
    /// records for `isCellIdentityCompatible` one layer up. Do not delete the guard on the
    /// strength of this test.
    [<Test>]
    let ``a same-width copy between differently-typed cells goes by bytes`` () : unit =
        let text =
            """
using System;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

public class TestBulkMoveShapeMismatch
{
    [InlineArray(4)]
    private struct DoubleBuf { private double _item; }

    public static int Main()
    {
        long[] src = new long[4];
        for (int i = 0; i < 4; i++) src[i] = 0x4010000000000000L + (i + 1);

        DoubleBuf dst = default;
        MemoryMarshal.Cast<long, double>(src.AsSpan()).CopyTo(dst);

        int acc = 0;
        for (int i = 0; i < 4; i++) acc = acc * 31 + (int)(BitConverter.DoubleToInt64Bits(dst[i]) & 0xFFFF);
        return ((acc % 251) + 251) % 251;
    }
}
"""

        let image = Roslyn.compile [ text ]

        let expected =
            match RealRuntime.executeWithRealRuntime [||] image with
            | RealRuntimeResult.NormalExit exitCode -> exitCode
            | RealRuntimeResult.UnhandledException report ->
                failwith $"real runtime terminated with an unhandled exception:\n%s{report}"
            | RealRuntimeResult.Aborted (code, report) -> failwith $"real runtime aborted (%O{code}):\n%s{report}"

        runUnderPawPrint "shapeMismatch" image |> shouldEqual expected

    [<TestCaseSource(nameof cases)>]
    let ``Bulk moves through typed cells match the real runtime`` (case : string) : unit =
        let text = source shapes.[case]
        let image = Roslyn.compile [ text ]

        // The oracle is a real process, so it carries one group per run; PawPrint costs seconds per
        // run and carries all three at once.
        let measure (group : int) : int =
            match RealRuntime.executeWithRealRuntime [| string group |] image with
            | RealRuntimeResult.NormalExit exitCode -> exitCode
            | RealRuntimeResult.UnhandledException report ->
                failwith $"%s{case}: real runtime terminated with an unhandled exception:\n%s{report}"
            | RealRuntimeResult.Aborted (code, report) ->
                failwith $"%s{case}: real runtime aborted (%O{code}):\n%s{report}"

        let expected = (measure 0) ||| ((measure 1) <<< 8) ||| ((measure 2) <<< 16)

        let actual = runUnderPawPrint case image

        if actual <> expected then
            let decode (packed : int) =
                $"group0=%d{packed &&& 0xFF}, group1=%d{(packed >>> 8) &&& 0xFF}, group2=%d{(packed >>> 16) &&& 0xFF}"

            failwith
                $"%s{case}: PawPrint and the real runtime disagree on the bulk-move checksum.\n  real runtime: %s{decode expected}\n  PawPrint:     %s{decode actual}\n\nSource:\n%s{text}"

        actual |> shouldEqual expected

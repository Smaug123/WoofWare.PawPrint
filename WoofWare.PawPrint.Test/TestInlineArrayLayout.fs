namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// A differential sweep over `[InlineArray(N)]` layout, taking the *real runtime* as the oracle
/// rather than a table of constants a human transcribed once.
///
/// CoreCLR lays an inline array out as if it had its one declared instance field, sizes *that*
/// completely — the rounding at the end of the layout algorithm included — and then multiplies the
/// result by N (`MethodTableBuilder::PlaceInstanceFields`, methodtablebuilder.cpp:8612 for the
/// auto-layout route, `HandleSequentialLayout` :8663 for sequential). PawPrint materialises N
/// storage slots (`InlineArrayStorage.expand`) but places only the first, striding the rest by the
/// rounded element size, so the two agree by construction rather than by coincidence. The sweep is
/// what holds that claim honest at the edges: it varies the element's size, alignment, GC content
/// and nesting, the declared `LayoutKind`, and `Pack`, and requires the two runtimes to agree for
/// every combination.
///
/// Each generated program measures four sizes — N = 1, 2 and 3, plus the buffer's placement inside
/// a larger struct — from a single compilation. PawPrint returns all four packed into one int32,
/// which is read directly off the evaluation stack; the real runtime is a real process, whose exit
/// code on Unix is only 8 bits, so it is run once per measurement. On disagreement the assertion
/// message decodes both sides, so a failure names the shape.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter, which is where essentially all of the suite's
// time goes; `Explicit` keeps it out of a bare `dotnet test` so local iteration is
// quick. CI selects it by category and so runs it. See AGENTS.md.
[<Category("Guest")>]
[<Explicit>]
module TestInlineArrayLayout =

    /// An element type to build inline arrays over: the declarations it needs, and the name to use
    /// as the field's type.
    type private ElementShape =
        {
            Declarations : string
            TypeName : string
        }

    let private elementShapes : Map<string, ElementShape> =
        [
            "byte",
            {
                Declarations = ""
                TypeName = "byte"
            }
            "short",
            {
                Declarations = ""
                TypeName = "short"
            }
            "int",
            {
                Declarations = ""
                TypeName = "int"
            }
            "long",
            {
                Declarations = ""
                TypeName = "long"
            }
            // Normalised by the type loader to `ELEMENT_TYPE_I`, so it buckets as a pointer-sized
            // primitive rather than as a by-value struct.
            "nint",
            {
                Declarations = ""
                TypeName = "IntPtr"
            }
            // A GC reference: forces the whole aggregate through auto layout, and makes the slot
            // count visible to the GC rather than only to `sizeof`.
            "reference",
            {
                Declarations = "private sealed class Box { public int V; }"
                TypeName = "Box"
            }
            // Three bytes: a size that is not a power of two and does not divide the pointer size,
            // so a stride computed by rounding the *aggregate* rather than the slot would diverge.
            "threeBytes",
            {
                Declarations =
                    "[StructLayout(LayoutKind.Sequential, Pack = 1)] private struct Three { public byte A; public byte B; public byte C; }"
                TypeName = "Three"
            }
            // Five declared bytes, eight after the slot's own alignment rounding: the case where
            // "round the slot then multiply" and "multiply then round" give different answers.
            "fiveBytes",
            {
                Declarations = "private struct Five { public int I; public byte B; }"
                TypeName = "Five"
            }
            // A by-value element that itself contains a reference: placed at pointer alignment,
            // and its GC pointer series is what CoreCLR multiplies by N.
            "mixedRef",
            {
                Declarations =
                    "private sealed class Box2 { public int V; } private struct MixedRef { public byte B; public Box2 O; }"
                TypeName = "MixedRef"
            }
            // An inline array of inline arrays: two levels of replication.
            "nestedInlineArray",
            {
                Declarations = "[InlineArray(3)] private struct Inner { private int _item; }"
                TypeName = "Inner"
            }
            // Five bytes with an alignment of four: a declared `Size` is a floor and the alignment
            // rounding is its alternative rather than its sequel (`CalculateSizeWithMetadataSize`,
            // classlayoutinfo.cpp:326-341), so this element's own size is *not* a multiple of its
            // own alignment. That is the only way to make "round the slot then multiply" and
            // "multiply then round" disagree on the sequential route: every other element here is
            // already its own rounded size.
            "sizeFloor",
            {
                Declarations = "[StructLayout(LayoutKind.Sequential, Size = 5)] private struct Floor { public int I; }"
                TypeName = "Floor"
            }
        ]
        |> Map.ofList

    /// The `StructLayout` the buffer types themselves carry.
    ///
    /// `Pack` applies to the single-slot layout, so it can change the stride — except when the
    /// element holds a GC reference, where CoreCLR discards it along with the rest of the declared
    /// layout. Both behaviours are swept.
    ///
    /// `auto` is the other axis: a C# struct with no attribute is `Sequential` in metadata, so
    /// without an explicit `LayoutKind.Auto` this sweep never reaches the auto route at all —
    /// which is where the element rounding differs most visibly, and where the aggregate's
    /// *alignment* stops being its element's.
    let private bufferAttributes : Map<string, string> =
        [
            "packDefault", ""
            "pack1", "[StructLayout(LayoutKind.Sequential, Pack = 1)]"
            "pack2", "[StructLayout(LayoutKind.Sequential, Pack = 2)]"
            "auto", "[StructLayout(LayoutKind.Auto)]"
        ]
        |> Map.ofList

    /// Combinations for which there is no oracle, because real .NET cannot run them.
    ///
    /// An auto-layout inline array whose *multiplied* size is below the pointer size and is not a
    /// power of two gets exactly that as its alignment: nothing is recorded as a custom field
    /// alignment (the element's `minAlign` equals `min(elementSize, sizeof(void*))`), so
    /// `MethodTable::GetFieldAlignmentRequirement` falls through to `min(3, 8)` for three bytes and
    /// `min(6, 8)` for three shorts. The type *loads* — this is a computed alignment, not a
    /// rejection — but the JIT then refuses to compile any method that mentions it, with
    /// `InvalidProgramException: The metadata is corrupt`. Every other combination in the sweep
    /// escapes it: two bytes give 2, and anything reaching 8 or more is capped at the pointer size.
    ///
    /// PawPrint has no JIT, so it lays such a type out with the alignment CoreCLR computed and runs
    /// the program. That divergence is recorded in `docs/divergences.md`; what is asserted below is
    /// only that the exclusion is still warranted, so that a future runtime which fixes the
    /// upstream bug fails this fixture rather than quietly keeping a stale carve-out.
    let private unrunnableCases : string list = [ "byte/auto" ; "short/auto" ]

    let private cases : string list =
        [
            for element in elementShapes |> Map.toSeq |> Seq.map fst do
                for attribute in bufferAttributes |> Map.toSeq |> Seq.map fst do
                    let case = $"%s{element}/%s{attribute}"

                    if not (List.contains case unrunnableCases) then
                        yield case
        ]

    let private source (element : ElementShape) (attribute : string) : string =
        $"""
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestInlineArrayLayoutSweep
{{
    %s{element.Declarations}

    %s{attribute} [InlineArray(1)] private struct B1 {{ private %s{element.TypeName} _item; }}
    %s{attribute} [InlineArray(2)] private struct B2 {{ private %s{element.TypeName} _item; }}
    %s{attribute} [InlineArray(3)] private struct B3 {{ private %s{element.TypeName} _item; }}

    // Where a whole inline array lands inside a larger struct, and where the field after it lands.
    // The alignment that decides this is the aggregate's own, and on the auto route that is not
    // always its element's: CoreCLR records a custom field alignment only when the element's
    // rounded alignment differs from `min(elementSize, sizeof(void*))`, and otherwise answers from
    // the *multiplied* size (`MethodTable::GetFieldAlignmentRequirement`, methodtable.cpp:8853).
    private struct Holder {{ public byte Lead; public B3 Buf; public byte Tail; }}

    public static int Main(string[] argv)
    {{
        int s1 = Unsafe.SizeOf<B1>();
        int s2 = Unsafe.SizeOf<B2>();
        int s3 = Unsafe.SizeOf<B3>();
        int h = Unsafe.SizeOf<Holder>();

        // Keep every measurement inside one byte; every shape in the sweep is far below this, so
        // tripping it means the layout is wrong by orders of magnitude rather than that the
        // encoding is too tight.
        bool oversized = s1 < 0 || s1 > 127 || s2 < 0 || s2 > 127 || s3 < 0 || s3 > 127 || h < 0 || h > 127;

        // With no argument, report all four measurements packed into the return value. Only
        // PawPrint calls it this way, because PawPrint's result is read straight off the
        // evaluation stack as a full int32.
        if (argv.Length == 0) return oversized ? -1 : (s1 | (s2 << 8) | (s3 << 16) | (h << 24));

        // A Unix process exit code is 8 bits, so a real process cannot return the packed value at
        // all; it reports one measurement per run instead, selected here. 255 is the oversized
        // sentinel, unambiguous because every real measurement is at most 127.
        if (oversized) return 255;

        switch (argv[0])
        {{
            case "0": return s1;
            case "1": return s2;
            case "2": return s3;
            case "3": return h;
            // Unreachable: the sweep passes 0..3. Falling back to one of the measurements would
            // let a mis-indexed caller read a plausible-looking size for the wrong slot, so make
            // it loud; the oracle reports the crash and the test names it.
            default: throw new ArgumentOutOfRangeException(nameof(argv), argv[0], "unknown measurement selector");
        }}
    }}
}}
"""

    /// Both oversized sentinels are rejected before this is reached, so every value it sees is four
    /// packed measurements.
    let private decode (packed : int) : string =
        let s1 = packed &&& 0xFF
        let s2 = (packed >>> 8) &&& 0xFF
        let s3 = (packed >>> 16) &&& 0xFF
        let h = (packed >>> 24) &&& 0xFF
        $"sizeof B1=%d{s1}, B2=%d{s2}, B3=%d{s3}, Holder=%d{h}"

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
        | RunOutcome.NormalExit (terminalState, _)
        | RunOutcome.ProcessExit (terminalState, _) -> terminalState.LatchedExitCode
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"%s{sourceName}: guest threw an unhandled exception: %O{exn.ExceptionObject}"
        | RunOutcome.Aborted (_, _, fatal) ->
            let message = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"%s{sourceName}: guest aborted (%O{fatal.Code}): %s{message}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"%s{sourceName}: guest was terminated by POSIX signal %O{signal}"

    [<TestCaseSource(nameof cases)>]
    let ``Inline array layout matches the real runtime`` (case : string) : unit =
        let elementKey, attributeKey =
            match case.Split '/' with
            | [| elementKey ; attributeKey |] -> elementKey, attributeKey
            | _ -> failwith $"malformed case key %s{case}"

        let text = source elementShapes.[elementKey] bufferAttributes.[attributeKey]
        let image = Roslyn.compile [ text ]

        // The oracle runs the guest as a real process, whose exit code on Unix is 8 bits, so it can
        // carry one measurement per run rather than all four packed. Taking four runs on that side
        // and one on PawPrint's is the cheap way round: a guest process costs tens of milliseconds
        // and a PawPrint run of this sweep costs seconds.
        let measure (index : int) : int =
            match RealRuntime.executeWithRealRuntime [| string index |] image with
            | RealRuntimeResult.NormalExit 255 ->
                failwith $"%s{case}: the real runtime reported a size too large to encode; the sweep needs widening"
            | RealRuntimeResult.NormalExit exitCode -> exitCode
            | RealRuntimeResult.UnhandledException report ->
                failwith $"%s{case}: real runtime terminated with an unhandled exception:\n%s{report}"
            | RealRuntimeResult.Aborted (code, report) ->
                failwith $"%s{case}: real runtime aborted (%O{code}):\n%s{report}"

        // The four oracle processes run while PawPrint interprets the same guest; the two
        // sides do not interact. See `DifferentialOracle.alongsideInterpreted`.
        let expected, actual =
            DifferentialOracle.alongsideInterpreted
                (fun () ->
                    (measure 0)
                    ||| ((measure 1) <<< 8)
                    ||| ((measure 2) <<< 16)
                    ||| ((measure 3) <<< 24)
                )
                (fun () -> runUnderPawPrint case image)

        if actual = -1 then
            failwith $"%s{case}: PawPrint reported a size too large to encode; the sweep needs widening"

        if actual <> expected then
            failwith
                $"%s{case}: PawPrint and the real runtime disagree.\n  real runtime: %s{decode expected}\n  PawPrint:     %s{decode actual}"

        actual |> shouldEqual expected

    /// The carve-out above is a claim about the *host* runtime, so it is measured, not assumed. A
    /// runtime that fixed the upstream bug would make these shapes oracle-checkable again, and this
    /// test is what would say so.
    [<TestCaseSource(nameof unrunnableCases)>]
    let ``The excluded shapes are ones the real runtime refuses to run`` (case : string) : unit =
        let elementKey, attributeKey =
            match case.Split '/' with
            | [| elementKey ; attributeKey |] -> elementKey, attributeKey
            | _ -> failwith $"malformed case key %s{case}"

        let text = source elementShapes.[elementKey] bufferAttributes.[attributeKey]
        let image = Roslyn.compile [ text ]

        match RealRuntime.executeWithRealRuntime [| "0" |] image with
        | RealRuntimeResult.UnhandledException report when report.Contains "InvalidProgramException" -> ()
        | RealRuntimeResult.UnhandledException report ->
            failwith
                $"%s{case}: the real runtime failed, but not with the InvalidProgramException this exclusion is for:\n%s{report}"
        | RealRuntimeResult.NormalExit exitCode ->
            failwith
                $"%s{case}: the real runtime now runs this shape (exit code %d{exitCode}), so it has an oracle again and belongs in the sweep rather than in `unrunnableCases`"
        | RealRuntimeResult.Aborted (code, report) -> failwith $"%s{case}: real runtime aborted (%O{code}):\n%s{report}"

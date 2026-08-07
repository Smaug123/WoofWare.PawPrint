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
/// CoreCLR lays an inline array out as if it had its one declared instance field, then multiplies
/// the resulting instance size by N (`MethodTableBuilder::PlaceInstanceFields`,
/// methodtablebuilder.cpp:8612 for the auto-layout route, :8663 for sequential). PawPrint instead
/// replicates the field into N storage slots and runs its ordinary layout algorithms over them
/// (`InlineArrayStorage.expand`). Those are only the same thing because every slot is identical —
/// an argument that is easy to state and easy to get wrong at the edges, which is what this sweep
/// is for: it varies the element's size, alignment, GC content and nesting, and `Pack`, and
/// requires the two runtimes to agree for every combination.
///
/// Each generated program packs four measured sizes into its exit code, so one compilation and one
/// run per runtime covers N = 1, 2 and 3 plus the element's placement inside a larger struct. On
/// disagreement the assertion message decodes both sides, so a failure names the shape.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
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
        ]
        |> Map.ofList

    /// `Pack` applies to the single-slot layout, so it can change the stride — except when the
    /// element holds a GC reference, where CoreCLR discards it along with the rest of the declared
    /// layout. Both behaviours are swept.
    let private packAttributes : Map<string, string> =
        [
            "packDefault", ""
            "pack1", "[StructLayout(LayoutKind.Sequential, Pack = 1)]"
            "pack2", "[StructLayout(LayoutKind.Sequential, Pack = 2)]"
        ]
        |> Map.ofList

    let private cases : string list =
        [
            for element in elementShapes |> Map.toSeq |> Seq.map fst do
                for pack in packAttributes |> Map.toSeq |> Seq.map fst do
                    yield $"%s{element}/%s{pack}"
        ]

    let private source (element : ElementShape) (pack : string) : string =
        $"""
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

public class TestInlineArrayLayoutSweep
{{
    %s{element.Declarations}

    %s{pack} [InlineArray(1)] private struct B1 {{ private %s{element.TypeName} _item; }}
    %s{pack} [InlineArray(2)] private struct B2 {{ private %s{element.TypeName} _item; }}
    %s{pack} [InlineArray(3)] private struct B3 {{ private %s{element.TypeName} _item; }}

    // The element's alignment — not the aggregate's size — decides where a whole inline array
    // lands inside a larger struct, and where the field after it lands.
    private struct Holder {{ public byte Lead; public B3 Buf; public byte Tail; }}

    public static int Main(string[] argv)
    {{
        int s1 = Unsafe.SizeOf<B1>();
        int s2 = Unsafe.SizeOf<B2>();
        int s3 = Unsafe.SizeOf<B3>();
        int h = Unsafe.SizeOf<Holder>();

        // Keep every measurement inside one byte so all four fit in the exit code; every shape in
        // the sweep is far below this, so tripping it means the layout is wrong by orders of
        // magnitude rather than that the encoding is too tight.
        if (s1 < 0 || s1 > 127 || s2 < 0 || s2 > 127 || s3 < 0 || s3 > 127 || h < 0 || h > 127) return -1;

        return s1 | (s2 << 8) | (s3 << 16) | (h << 24);
    }}
}}
"""

    let private decode (packed : int) : string =
        if packed = -1 then
            "<a measured size did not fit in one byte>"
        else
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
    let ``Inline array layout matches the real runtime`` (case : string) : unit =
        let elementKey, packKey =
            match case.Split '/' with
            | [| elementKey ; packKey |] -> elementKey, packKey
            | _ -> failwith $"malformed case key %s{case}"

        let text = source elementShapes.[elementKey] packAttributes.[packKey]
        let image = Roslyn.compile [ text ]

        let expected =
            match RealRuntime.executeWithRealRuntime [||] image with
            | RealRuntimeResult.NormalExit exitCode -> exitCode
            | RealRuntimeResult.UnhandledException exn ->
                failwith $"%s{case}: real runtime threw unhandled %s{exn.GetType().Name}: %s{exn.Message}"

        if expected = -1 then
            failwith $"%s{case}: the real runtime reported a size too large to encode; the sweep needs widening"

        let actual = runUnderPawPrint case image

        if actual <> expected then
            failwith
                $"%s{case}: PawPrint and the real runtime disagree.\n  real runtime: %s{decode expected}\n  PawPrint:     %s{decode actual}"

        actual |> shouldEqual expected

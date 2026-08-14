namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection.Metadata
open System.Text
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestSequencePoints =

    let private loc (line : int) : SourceLocation =
        {
            DocumentPath = "Doc.cs"
            StartLine = line
            StartColumn = 1
            EndLine = line
            EndColumn = 10
        }

    [<Test>]
    let ``resolve reports the last sequence point at or before the offset`` () : unit =
        let points =
            MethodSequencePoints.ofSeq [ 0, SequencePoint.Source (loc 10) ; 7, SequencePoint.Source (loc 11) ]

        MethodSequencePoints.resolve 0 points |> shouldEqual (Some (loc 10))
        MethodSequencePoints.resolve 3 points |> shouldEqual (Some (loc 10))
        MethodSequencePoints.resolve 6 points |> shouldEqual (Some (loc 10))
        MethodSequencePoints.resolve 7 points |> shouldEqual (Some (loc 11))
        MethodSequencePoints.resolve 99 points |> shouldEqual (Some (loc 11))

    [<Test>]
    let ``an offset before the first sequence point has no source`` () : unit =
        let points = MethodSequencePoints.ofSeq [ 4, SequencePoint.Source (loc 10) ]

        MethodSequencePoints.resolve 0 points |> shouldEqual None
        MethodSequencePoints.resolve 3 points |> shouldEqual None
        MethodSequencePoints.resolve 4 points |> shouldEqual (Some (loc 10))

    [<Test>]
    let ``an empty method has no source anywhere`` () : unit =
        let points = MethodSequencePoints.ofSeq []

        MethodSequencePoints.isEmpty points |> shouldEqual true
        MethodSequencePoints.resolve 0 points |> shouldEqual None

    /// This is the reason `SequencePoint` has a `Hidden` case at all. Dropping hidden points
    /// at parse time would leave the *preceding* source span apparently covering the hidden
    /// range, so compiler-generated IL would be attributed to whichever user line happened to
    /// precede it.
    [<Test>]
    let ``a hidden sequence point masks the preceding source`` () : unit =
        let points =
            MethodSequencePoints.ofSeq
                [
                    0, SequencePoint.Source (loc 10)
                    5, SequencePoint.Hidden
                    9, SequencePoint.Source (loc 12)
                ]

        MethodSequencePoints.resolve 4 points |> shouldEqual (Some (loc 10))
        MethodSequencePoints.resolve 5 points |> shouldEqual None
        MethodSequencePoints.resolve 8 points |> shouldEqual None
        MethodSequencePoints.resolve 9 points |> shouldEqual (Some (loc 12))

    [<Test>]
    let ``ofSeq sorts by offset, and the last point at a repeated offset wins`` () : unit =
        let points =
            MethodSequencePoints.ofSeq
                [
                    9, SequencePoint.Source (loc 12)
                    0, SequencePoint.Source (loc 10)
                    9, SequencePoint.Source (loc 13)
                ]

        MethodSequencePoints.resolve 1 points |> shouldEqual (Some (loc 10))
        MethodSequencePoints.resolve 9 points |> shouldEqual (Some (loc 13))

    // ---- Reading symbols out of a real image ----

    /// Line numbers are load-bearing in the assertions below, so the source is assembled from
    /// explicit lines rather than a triple-quoted literal whose leading newline and indentation
    /// are easy to disturb. `Triple` occupies lines 3-7, with its opening brace on line 4.
    let private source : string =
        String.concat
            "\n"
            [
                "public static class Sut" // 1
                "{" // 2
                "    public static int Triple(int x)" // 3
                "    {" // 4
                "        int y = x * 3;" // 5
                "        return y;" // 6
                "    }" // 7
                "    public static int Main()" // 8
                "    {" // 9
                "        return Triple(0);" // 10
                "    }" // 11
                "}" // 12
            ]

    let private readImage (image : byte[]) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (image, false)
        AssemblyApi.read loggerFactory None stream

    let private tripleHandle (assy : DumpedAssembly) : MethodDefinitionHandle =
        assy.Methods
        |> Seq.pick (fun kvp -> if kvp.Value.Name = "Triple" then Some kvp.Key else None)

    let private tripleMethod
        (assy : DumpedAssembly)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        assy.Methods
        |> Seq.pick (fun kvp -> if kvp.Value.Name = "Triple" then Some kvp.Value else None)

    /// The rendering prose diagnostics use — `GuestLocation`'s thread summaries. Column
    /// information is deliberately dropped: the line wants to be greppable and to paste into an
    /// editor, and `path:line` is what does that.
    [<Test>]
    let ``a source location renders as path and line`` () : unit =
        let location =
            {
                DocumentPath = "/src/Foo.cs"
                StartLine = 17
                StartColumn = 5
                EndLine = 19
                EndColumn = 30
            }

        string location |> shouldEqual "/src/Foo.cs:17"

    [<Test>]
    let ``TryResolveMethodSource resolves a metadata-backed method`` () : unit =
        let assy = readImage (Roslyn.compileWithSymbols [ source ])

        match assy.TryResolveMethodSource (tripleMethod assy) 0 with
        | None -> failwith "expected a source location for Triple's first instruction"
        | Some location ->
            location.DocumentPath |> shouldEqual "File0.cs"
            location.StartLine |> shouldEqual 4

    [<Test>]
    let ``TryResolveMethodSource has nothing to say without symbols`` () : unit =
        let assy = readImage (Roslyn.compile [ source ])

        assy.TryResolveMethodSource (tripleMethod assy) 0 |> shouldEqual None

    [<Test>]
    let ``an image compiled with symbols exposes its sequence points`` () : unit =
        let assy = readImage (Roslyn.compileWithSymbols [ source ])
        let handle = tripleHandle assy

        let points =
            match assy.SequencePoints.TryGetValue (ComparableMethodDefinitionHandle.Make handle) with
            | false, _ -> failwith "expected sequence points for Triple"
            | true, v -> v

        let lines =
            MethodSequencePoints.toList points
            |> List.choose (fun (_, point) ->
                match point with
                | SequencePoint.Source loc -> Some loc.StartLine
                | SequencePoint.Hidden -> None
            )

        // Every point attributed to Triple must fall inside Triple's own source span. A
        // row-for-row misjoin between the PDB's MethodDebugInformation table and the PE's
        // MethodDef table would show up here as lines belonging to some other member.
        lines |> List.isEmpty |> shouldEqual false
        lines |> List.filter (fun l -> l < 4 || l > 7) |> shouldEqual []
        lines |> List.contains 5 |> shouldEqual true
        lines |> List.contains 6 |> shouldEqual true

        match assy.TryResolveSourceLocation handle 0 with
        | None -> failwith "expected a source location for Triple's first instruction"
        | Some loc ->
            // Roslyn names the syntax tree File0.cs; see Roslyn.compileAssemblyWithResources.
            loc.DocumentPath |> shouldEqual "File0.cs"
            // A debug-configuration C# method opens with a `nop` attributed to its brace.
            loc.StartLine |> shouldEqual 4

    [<Test>]
    let ``an image compiled without symbols has no sequence points`` () : unit =
        let assy = readImage (Roslyn.compile [ source ])

        assy.SequencePoints.Count |> shouldEqual 0
        assy.TryResolveSourceLocation (tripleHandle assy) 0 |> shouldEqual None

    /// `SequencePoints` must be data rather than a view onto the still-open image: the PDB is
    /// parsed eagerly during `read`, so resolution keeps working once the assembly (and hence
    /// its `PEReader`) has been disposed. A lazily-reading implementation fails this.
    [<Test>]
    let ``sequence points survive disposal of the assembly`` () : unit =
        let assy = readImage (Roslyn.compileWithSymbols [ source ])
        let handle = tripleHandle assy

        (assy :> IDisposable).Dispose ()

        // The premise, asserted rather than assumed: disposal really does invalidate reads
        // through the PE reader. Without this the test below would pass for a lazy
        // implementation too, and so would prove nothing.
        let peReaderIsUnusable =
            try
                assy.PeReader.GetMetadataReader () |> ignore<MetadataReader>
                false
            with :? ObjectDisposedException ->
                true

        peReaderIsUnusable |> shouldEqual true

        assy.TryResolveSourceLocation handle 0 |> Option.isSome |> shouldEqual true

    /// Materialise an assembly, and optionally its sidecar PDB, into a directory of its own.
    /// Fresh per call because `Assembly.readFile`'s parse cache is process-wide and keyed by
    /// path: reusing a path would have one test observing another's cache entry.
    let private writeTempAssembly (image : byte[]) (pdb : byte[] option) : string =
        let unique = Guid.NewGuid().ToString "N"
        let dir = Path.Combine (Path.GetTempPath (), $"pawprint-pdb-%s{unique}")

        Directory.CreateDirectory dir |> ignore<DirectoryInfo>
        let dll = Path.Combine (dir, "PawPrintTestAssembly.dll")
        File.WriteAllBytes (dll, image)

        match pdb with
        | Some pdb -> File.WriteAllBytes (Path.ChangeExtension (dll, ".pdb"), pdb)
        | None -> ()

        dll

    let private readFile (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        AssemblyApi.readFile loggerFactory path

    [<Test>]
    let ``a side-by-side pdb supplies sequence points`` () : unit =
        let image, pdb = Roslyn.compileWithSidecarSymbols [ source ]
        let assy = readFile (writeTempAssembly image (Some pdb))

        assy.SequencePoints.Count |> shouldBeGreaterThan 0

        match assy.TryResolveSourceLocation (tripleHandle assy) 0 with
        | None -> failwith "expected a source location for Triple's first instruction"
        | Some loc -> loc.StartLine |> shouldEqual 4

    /// PawPrint looks for `<assembly file>.pdb` and pays no attention to the file name recorded
    /// in the image's CodeView entry, so an assembly renamed after it was built keeps its symbols
    /// as long as the PDB was renamed with it. Depending on the recorded name instead would also
    /// mean deciding whether this filesystem is case-sensitive, and losing the symbols of any
    /// assembly opened under casing that differs from what the compiler wrote down.
    [<Test>]
    let ``a renamed assembly finds the sidecar named after it`` () : unit =
        let image, pdb = Roslyn.compileWithSidecarSymbols [ source ]

        let unique = Guid.NewGuid().ToString "N"
        let dir = Path.Combine (Path.GetTempPath (), $"pawprint-pdb-%s{unique}")
        Directory.CreateDirectory dir |> ignore<DirectoryInfo>

        // Deliberately *not* the assembly name the compiler recorded, which is
        // PawPrintTestAssembly.
        let dll = Path.Combine (dir, "SomethingElse.dll")
        File.WriteAllBytes (dll, image)
        File.WriteAllBytes (Path.ChangeExtension (dll, ".pdb"), pdb)

        (readFile dll).SequencePoints.Count |> shouldBeGreaterThan 0

    /// `originalPath` is whatever the caller was given, and for an in-memory image that may be a
    /// descriptive label rather than a filename. Failing to interpret it must cost the symbols
    /// and nothing else — certainly not the assembly.
    [<TestCase "">]
    [<TestCase "\000not a path">]
    [<TestCase "<in-memory>">]
    let ``an originalPath that is not a path costs symbols, not the assembly`` (label : string) : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (Roslyn.compile [ source ], false)
        let assy = AssemblyApi.read loggerFactory (Some label) stream

        assy.SequencePoints.Count |> shouldEqual 0
        assy.Methods.Count |> shouldBeGreaterThan 0

    /// Locate a named stream within a standalone portable PDB by walking the metadata root.
    /// A hard-coded offset would quietly stop pointing at the heap the moment a compiler changed
    /// its layout, and the test built on it would go vacuous rather than fail.
    let private streamOffset (name : string) (pdb : byte[]) : int =
        let mutable p = 12
        let versionLength = BitConverter.ToInt32 (pdb, p)
        p <- p + 4 + ((versionLength + 3) / 4) * 4
        p <- p + 2 // Flags
        let streamCount = int (BitConverter.ToUInt16 (pdb, p))
        p <- p + 2

        let mutable found = -1

        for _ in 1..streamCount do
            let offset = BitConverter.ToInt32 (pdb, p)
            p <- p + 8
            let nameStart = p

            while pdb.[p] <> 0uy do
                p <- p + 1

            let thisName = Encoding.ASCII.GetString (pdb, nameStart, p - nameStart)
            p <- nameStart + ((p - nameStart + 1 + 3) / 4) * 4

            if thisName = name then
                found <- offset

        if found < 0 then
            failwith $"portable PDB has no %s{name} stream"

        found

    let private readFileLogging (path : string) : (unit -> LogLine list) * DumpedAssembly =
        let logs, loggerFactory = LoggerFactory.makeTest ()
        logs, AssemblyApi.readFile loggerFactory path

    /// Both damage tests assert that the *guard* fired, not merely that no symbols came back.
    /// Without that, either would also pass against an implementation that quietly failed to
    /// find the PDB at all — which is precisely how an earlier version of this test went vacuous.
    let private shouldHaveReportedUnusableSymbols (logs : unit -> LogLine list) : unit =
        logs ()
        |> List.exists (fun line -> line.Message.Contains "malformed debug information")
        |> shouldEqual true

    /// Symbols are a diagnostic aid, so damaged ones must cost us the symbols and nothing else.
    /// This damage spares the header, the `#Pdb` id and the `#GUID` stream, so the PDB opens and
    /// is still accepted as belonging to this image; it fails part-way through *enumeration*,
    /// which is a different code path from failing to open.
    [<Test>]
    let ``a side-by-side pdb with a damaged heap costs symbols, not the assembly`` () : unit =
        let image, pdb = Roslyn.compileWithSidecarSymbols [ source ]
        let damaged = Array.copy pdb
        let blobHeap = streamOffset "#Blob" pdb

        // The front of the blob heap is where the document-name and sequence-point blobs live.
        for i in blobHeap .. min (blobHeap + 200) (damaged.Length - 1) do
            damaged.[i] <- 0xFFuy

        let logs, assy = readFileLogging (writeTempAssembly image (Some damaged))

        assy.SequencePoints.Count |> shouldEqual 0
        // The assembly itself must have parsed perfectly well.
        assy.Methods.Count |> shouldBeGreaterThan 0
        shouldHaveReportedUnusableSymbols logs

    /// The sibling of the test above, for the other arm of the guard: this PDB fails as it is
    /// opened rather than as it is enumerated.
    [<Test>]
    let ``a truncated side-by-side pdb costs symbols, not the assembly`` () : unit =
        let image, pdb = Roslyn.compileWithSidecarSymbols [ source ]

        let logs, assy =
            readFileLogging (writeTempAssembly image (Some pdb.[0 .. pdb.Length / 2]))

        assy.SequencePoints.Count |> shouldEqual 0
        assy.Methods.Count |> shouldBeGreaterThan 0
        shouldHaveReportedUnusableSymbols logs

    /// The parse cache is keyed on the assembly file, but a cached entry carries the *symbols*
    /// that were beside it at the time. Adding a PDB to an already-cached assembly must
    /// therefore miss the cache, or the first caller's lack of symbols is served forever.
    [<Test>]
    let ``adding a side-by-side pdb invalidates the cached parse`` () : unit =
        let image, pdb = Roslyn.compileWithSidecarSymbols [ source ]
        let dll = writeTempAssembly image None

        (readFile dll).SequencePoints.Count |> shouldEqual 0

        File.WriteAllBytes (Path.ChangeExtension (dll, ".pdb"), pdb)

        (readFile dll).SequencePoints.Count |> shouldBeGreaterThan 0

    let private domainAssembly () : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        AssemblyApi.readFile loggerFactory typeof<DumpedAssembly>.Assembly.Location

    /// The `Hidden` case is only load-bearing if the *parser* records it; the unit tests above
    /// exercise `resolve` on hand-built points and would all still pass if hidden points were
    /// dropped at read time. F# emits them over compiler-generated IL, so a real F# assembly is
    /// the corpus that pins the parse down.
    [<Test>]
    let ``hidden sequence points survive parsing`` () : unit =
        let assy = domainAssembly ()

        let hidden =
            assy.SequencePoints
            |> Seq.sumBy (fun kvp ->
                MethodSequencePoints.toList kvp.Value
                |> List.sumBy (fun (_, point) ->
                    match point with
                    | SequencePoint.Hidden -> 1
                    | SequencePoint.Source _ -> 0
                )
            )

        hidden |> shouldBeGreaterThan 0

    /// Exhaustive rather than sampled: the corpus is every method of a real, symbol-bearing
    /// assembly, so there is nothing to gain from generating a subset of it.
    ///
    /// Sequence point offsets are required to fall on instruction boundaries, and `Locations`
    /// is keyed by exactly those boundaries. The property therefore catches both a misjoined
    /// PDB and a PDB that does not belong to this image at all (a stale `.pdb` beside a
    /// rebuilt assembly), either of which would otherwise misattribute lines in silence.
    [<Test>]
    let ``every sequence point offset is an instruction boundary`` () : unit =
        let assy = domainAssembly ()

        // The whole test is vacuous if this assembly turns out to carry no symbols, which is
        // exactly what would happen if DebugType stopped being `embedded` in Directory.Build.props.
        assy.SequencePoints.Count |> shouldBeGreaterThan 100

        let mutable verified = 0

        for KeyValue (handle, points) in assy.SequencePoints do
            match assy.Methods.TryGetValue handle.Get with
            | false, _ -> failwith $"PDB names method row %O{handle.Get} which the PE does not define"
            | true, method ->
                match MethodInfo.tryIlBody method with
                | None -> ()
                | Some body ->
                    for offset, _ in MethodSequencePoints.toList points do
                        if not (body.Locations.ContainsKey offset) then
                            failwith
                                $"Sequence point at IL offset %d{offset} in %s{method.RequiredDeclaringType.Name}.%s{method.Name} is not an instruction boundary"

                        verified <- verified + 1

        // Likewise: a corpus that reached no IL bodies would pass the loop trivially.
        verified |> shouldBeGreaterThan 1000

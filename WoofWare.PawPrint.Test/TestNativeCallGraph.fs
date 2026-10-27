namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `native-call-graph/` writes, for each runtime `flake.nix` pins and each Linux target, the call
/// graph of the C++ that `NativeContractTable.tsv` cites, and the flake checks hold the checked-in
/// graphs to what it writes. These tests hold each graph to the repository as it stands: to the
/// commit `flake.nix` pins for its runtime, and to the routines the contract's two tables name.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeCallGraph =

    /// The Linux targets each runtime has a graph for.
    let private targets : string list = [ "linux-x64" ; "linux-arm64" ]

    // Not __SOURCE_DIRECTORY__: a ContinuousIntegrationBuild remaps source paths to `/_/`.
    let private repoRoot : string =
        let rec walk (dir : string) : string =
            if String.IsNullOrEmpty dir then
                failwith $"Could not locate WoofWare.PawPrint.slnx by walking up from %s{AppContext.BaseDirectory}"
            elif File.Exists (Path.Combine (dir, "WoofWare.PawPrint.slnx")) then
                dir
            else
                walk (Path.GetDirectoryName dir)

        walk AppContext.BaseDirectory

    /// Each runtime `flake.nix` pins, by name, with the dotnet/runtime commit it pins its source at,
    /// refusing any shape of the `runtimes` set other than the one this module was written against.
    let private pinnedCommits : Lazy<Map<string, string>> =
        lazy
            (let flake = File.ReadAllText (Path.Combine (repoRoot, "flake.nix"))

             let pins =
                 Regex.Matches (
                     flake,
                     @"^\s*(?<name>net\d+) = \{\s*$(?:(?!^\s*\};).)*?^\s*sourceRev = ""(?<rev>[0-9a-f]{40})"";",
                     RegexOptions.Multiline ||| RegexOptions.Singleline
                 )

             if pins.Count = 0 then
                 failwith "flake.nix pins no runtime's source in the shape this test reads"

             pins
             |> Seq.map (fun m -> m.Groups.["name"].Value, m.Groups.["rev"].Value)
             |> Map.ofSeq)

    /// One graph, as the generator writes it.
    type private Graph =
        {
            /// The `name: value` lines of its header.
            Header : Map<string, string>
            /// The keys of its `routine` rows, each naming a definition.
            Defined : Set<string>
            /// The keys naming a template no unit instantiates.
            Uninstantiated : Set<string>
            /// The keys naming nothing a unit defines.
            Undefined : Set<string>
        }

    let private graph (runtime : string) (target : string) : Graph =
        let resource =
            $"WoofWare.PawPrint.Test.NativeCallGraph.NativeCallGraph.%s{runtime}.%s{target}.tsv"

        use stream = typeof<Graph>.Assembly.GetManifestResourceStream resource

        if isNull stream then
            failwith $"No graph is embedded as %s{resource}"

        use reader = new StreamReader (stream)
        let lines = reader.ReadToEnd().Split '\n' |> Array.filter (fun line -> line <> "")

        let header =
            lines
            |> Seq.takeWhile (fun line -> line.StartsWith '#')
            |> Seq.choose (fun line ->
                let m = Regex.Match (line, @"^# (?<name>[a-z]+): (?<value>.*)$")

                if m.Success then
                    Some (m.Groups.["name"].Value, m.Groups.["value"].Value)
                else
                    None
            )
            |> Map.ofSeq

        let rows = lines |> Array.filter (fun line -> not (line.StartsWith '#'))

        let keysOf (kind : string) : Set<string> =
            rows
            |> Seq.map (fun line -> line.Split '\t')
            |> Seq.filter (fun fields -> fields.[0] = kind)
            |> Seq.map (fun fields -> fields.[1])
            |> Set.ofSeq

        {
            Header = header
            Defined = keysOf "routine"
            Uninstantiated = keysOf "uninstantiated"
            Undefined = keysOf "undefined"
        }

    let graphs : TestCaseData list =
        [
            for runtime in [ "net10" ; "net11" ] do
                for target in targets do
                    yield TestCaseData(runtime, target).SetArgDisplayNames (runtime, target)
        ]

    [<Test>]
    let ``there is a graph for each runtime flake.nix pins, and for no other`` () : unit =
        let embedded =
            typeof<Graph>.Assembly.GetManifestResourceNames ()
            |> Seq.choose (fun name ->
                let m =
                    Regex.Match (name, @"^WoofWare\.PawPrint\.Test\.NativeCallGraph\.NativeCallGraph\.(.+)\.tsv$")

                if m.Success then Some m.Groups.[1].Value else None
            )
            |> Set.ofSeq

        let pinned =
            [
                for runtime in pinnedCommits.Force().Keys do
                    for target in targets do
                        yield $"%s{runtime}.%s{target}"
            ]
            |> Set.ofList

        embedded |> shouldEqual pinned

        graphs
        |> List.map (fun case -> $"%O{case.Arguments.[0]}.%O{case.Arguments.[1]}")
        |> Set.ofList
        |> shouldEqual pinned

    [<TestCaseSource(nameof graphs)>]
    let ``a graph describes the commit flake.nix pins for its runtime, for its target``
        (runtime : string)
        (target : string)
        : unit
        =
        let g = graph runtime target
        g.Header.["runtime"] |> shouldEqual runtime
        g.Header.["target"] |> shouldEqual target
        g.Header.["commit"] |> shouldEqual (pinnedCommits.Force ()).[runtime]

    /// Whether a path, as the contract's tables write one, names a file of C or C++.
    let private isCxx (path : string) : bool =
        [ ".c" ; ".cpp" ; ".h" ; ".hpp" ; ".inl" ]
        |> List.contains (Path.GetExtension path)

    /// Every source a row of the contract table cites that is C or C++, as path:symbol.
    let private cxxCitations : Lazy<Set<string>> =
        lazy
            (NativeContractTable.rows.Force ()
             |> Seq.collect (fun row -> row.Sources)
             |> Seq.filter (fun source -> isCxx source.Path)
             |> Seq.map (fun source -> $"%s{source.Path}:%s{source.Symbol}")
             |> Set.ofSeq)

    /// Every callee `NativeContractCallees.tsv` vouches for by the path:symbol of a file of C or C++,
    /// as the file writes it; not one of the C library, nor the calls of one routine.
    let private cxxVouches : Lazy<Set<string>> =
        lazy
            (use stream =
                typeof<Graph>.Assembly.GetManifestResourceStream "WoofWare.PawPrint.Test.NativeContractCallees.tsv"

             use reader = new StreamReader (stream)

             reader.ReadToEnd().Split '\n'
             |> Seq.filter (fun line -> line.Trim () <> "" && not (line.StartsWith '#'))
             |> Seq.map (fun line -> line.Split('\t').[0])
             |> Seq.filter (fun callee ->
                 let colon = callee.IndexOf ':'
                 let path = callee.Substring (0, colon)
                 path <> "-" && not (callee.Contains "::<") && isCxx path
             )
             |> Set.ofSeq)

    [<TestCaseSource(nameof graphs)>]
    let ``a graph describes exactly the C++ the contract's tables name`` (runtime : string) (target : string) : unit =
        let g = graph runtime target
        let described = Set.unionMany [ g.Defined ; g.Uninstantiated ; g.Undefined ]

        Set.difference described (Set.union (cxxCitations.Force ()) (cxxVouches.Force ()))
        |> shouldEqual Set.empty

        Set.difference (Set.union (cxxCitations.Force ()) (cxxVouches.Force ())) described
        |> shouldEqual Set.empty

        // A key is in rows of one kind only.
        Set.intersect g.Defined (Set.union g.Uninstantiated g.Undefined)
        |> shouldEqual Set.empty

        Set.intersect g.Uninstantiated g.Undefined |> shouldEqual Set.empty

    /// Each graph, but the table's citations name .NET 10's source, which .NET 11's moves and removes
    /// some of.
    let graphsOfTheTablesRuntime : TestCaseData list =
        [
            for runtime in [ "net10" ; "net11" ] do
                for target in targets do
                    let case = TestCaseData(runtime, target).SetArgDisplayNames (runtime, target)

                    yield
                        if runtime = "net10" then
                            case
                        else
                            case.Ignore Net11Frontier.nativeContractTable
        ]

    [<TestCaseSource(nameof graphsOfTheTablesRuntime)>]
    let ``every C++ routine the contract table cites has a definition in the graph``
        (runtime : string)
        (target : string)
        : unit
        =
        let g = graph runtime target
        Set.difference (cxxCitations.Force ()) g.Defined |> shouldEqual Set.empty

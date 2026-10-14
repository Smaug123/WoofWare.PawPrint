namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `DirectoryEntryPal.nodeType` transcribes the PAL's `DT_*` numbering, so
/// nothing in the type system keeps its numbers right. Its oracle is upstream's
/// own declaration in the pinned `Interop.ReadDir.cs`: the numbers the guest's
/// CoreLib compares against.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryEntryPal =

    /// `DT_SOCK = 12,` and friends.
    let private nodeTypeEntry : Regex =
        Regex @"\b(?<name>DT_[A-Z]+)\s*=\s*(?<value>\d+)"

    /// The name upstream gives each kind's `DT_*` value.
    let private upstreamName (kind : DirectoryEntryKind) : string =
        match kind with
        | DirectoryEntryKind.RegularFile -> "DT_REG"
        | DirectoryEntryKind.Directory -> "DT_DIR"
        | DirectoryEntryKind.Symlink -> "DT_LNK"
        | DirectoryEntryKind.CharacterDevice -> "DT_CHR"
        | DirectoryEntryKind.Socket -> "DT_SOCK"
        | DirectoryEntryKind.Fifo -> "DT_FIFO"

    [<Test>]
    let ``every DirectoryEntryKind is numbered as the pinned Interop.ReadDir.cs numbers it`` () : unit =
        let path =
            let dir = FrameworkUnderTest.requireRuntimeSource ()

            Path.Combine (
                dir,
                "src",
                "libraries",
                "Common",
                "src",
                "Interop",
                "Unix",
                "System.Native",
                "Interop.ReadDir.cs"
            )

        if not (File.Exists path) then
            failwith
                $"expected the pinned NodeType declaration at %s{path}. If the sparse checkout in flake.nix no longer includes src/libraries/Common/src/Interop/Unix/System.Native, DirectoryEntryPal.nodeType has lost its oracle."

        let pinned =
            nodeTypeEntry.Matches (File.ReadAllText path)
            |> Seq.map (fun m -> m.Groups.["name"].Value, Int32.Parse m.Groups.["value"].Value)
            |> Map.ofSeq

        // Guard against the regex silently matching nothing, which would make
        // every assertion below vacuous: upstream declares nine node types.
        pinned |> Map.count |> shouldEqual 9

        let kinds =
            FSharpType.GetUnionCases typeof<DirectoryEntryKind>
            |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> DirectoryEntryKind)

        kinds |> Array.length |> shouldEqual 6

        for kind in kinds do
            match Map.tryFind (upstreamName kind) pinned with
            | None -> failwith $"the pinned Interop.ReadDir.cs no longer declares %s{upstreamName kind}"
            | Some value -> DirectoryEntryPal.nodeType kind |> shouldEqual value

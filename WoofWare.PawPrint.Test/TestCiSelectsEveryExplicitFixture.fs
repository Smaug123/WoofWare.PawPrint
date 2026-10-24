namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework

/// A bare `dotnet test` skips every `[<Explicit>]` fixture, so CI runs each of them only through
/// the `test-guest` job, whose runners each select one category from its matrix's `category`
/// list. A fixture carrying none of those categories never runs in CI, and nothing reports that;
/// one carrying two runs twice. A fixture carrying one but not `[<Explicit>]` runs in the default
/// suite as well.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCiSelectsEveryExplicitFixture =

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

    /// The categories the `test-guest` job's runners select, refusing any shape of the workflow
    /// other than the one this module was written against.
    let private ciCategories () : Set<string> =
        let workflow =
            File.ReadAllText (Path.Combine (repoRoot, ".github", "workflows", "ci.yaml"))

        let lists =
            Regex.Matches (workflow, @"^\s*category: \[(?<items>[^\]]*)\]\s*$", RegexOptions.Multiline)

        let categories =
            match List.ofSeq lists with
            | [ only ] ->
                only.Groups.["items"].Value.Split ','
                |> Array.map (fun item -> item.Trim ())
                |> Array.toList
            | other ->
                failwith $"expected exactly one `category: [ ... ]` matrix list in ci.yaml, found %d{other.Length}"

        for category in categories do
            if not (Regex.IsMatch (category, "^[A-Za-z]+$")) then
                failwith $"expected each category in ci.yaml's `category` list to be a plain name, found %s{category}"

        // The list means nothing unless the guest runners filter on it, and by nothing else.
        Regex.Matches (workflow, "--filter \"(?<filter>[^\"]*)\"")
        |> Seq.map (fun m -> m.Groups.["filter"].Value)
        |> List.ofSeq
        |> shouldEqual [ "TestCategory=${{ matrix.category }}" ]

        let distinct = Set.ofList categories
        distinct.Count |> shouldEqual categories.Length
        distinct

    let private categoriesOf (t : Type) : Set<string> =
        t.GetCustomAttributes<CategoryAttribute> false
        |> Seq.map (fun a -> a.Name)
        |> Set.ofSeq

    [<Test>]
    let ``every Explicit fixture carries exactly one category CI selects, and only those do`` () : unit =
        let selected = ciCategories ()

        let types = Assembly.GetExecutingAssembly().GetTypes ()

        let explicitFixtures =
            types |> Array.filter (fun t -> t.IsDefined (typeof<ExplicitAttribute>, false))

        // Guards against the predicate matching nothing, which would pass vacuously.
        explicitFixtures.Length |> shouldBeGreaterThan 20

        let unselected =
            explicitFixtures
            |> Array.choose (fun t ->
                let ours = Set.intersect (categoriesOf t) selected

                if ours.Count = 1 then
                    None
                else
                    Some $"%s{t.FullName} carries %d{ours.Count} of %A{selected}"
            )

        let notExplicit =
            types
            |> Array.filter (fun t ->
                not (t.IsDefined (typeof<ExplicitAttribute>, false))
                && not (Set.intersect (categoriesOf t) selected).IsEmpty
            )
            |> Array.map (fun t -> $"%s{t.FullName} carries a category CI selects, but is not Explicit")

        Array.append unselected notExplicit |> Array.sort |> shouldBeEmpty

        // A category no fixture carries is a runner that runs nothing.
        for category in selected do
            explicitFixtures
            |> Array.exists (fun t -> (categoriesOf t).Contains category)
            |> fun carried ->
                if not carried then
                    failwith $"no fixture carries %s{category}, which ci.yaml runs a runner for"

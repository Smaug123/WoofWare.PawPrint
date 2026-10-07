namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Xml
open System.Xml.Linq
open FsUnitTyped
open NUnit.Framework

/// A published package ships its assembly's XML documentation file, which a consumer's IDE reads
/// to show each member's documentation. The F# compiler escapes a `///` comment's text only when
/// the comment does not open with a tag; one that does is copied into the file verbatim, so a raw
/// `&`, a `<` that opens no tag, or an entity XML does not define makes the whole file malformed,
/// and an IDE then shows no documentation for any member of that package.
///
/// The published projects are read from the solution, so a new one is checked without anyone
/// remembering to list it here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPublishedXmlDocumentation =

    /// Published projects that ship no XML documentation file, because they do not set
    /// `GenerateDocumentationFile`. A project that turns it on must leave this set.
    let private undocumented : Set<string> = set [ "WoofWare.PawPrint" ]

    /// Fewer `<member>` elements than this means the file documents almost nothing, which is no
    /// evidence that the library's docstrings are well-formed. The smallest published library
    /// documents more than 150.
    [<Literal>]
    let private MinimumMembers : int = 100

    type private TestAssemblyMarker = class end

    /// MSBuild copies each referenced project's documentation file beside its assembly in this
    /// project's output.
    let private testOutputDir : string =
        Path.GetDirectoryName typeof<TestAssemblyMarker>.Assembly.Location

    // Not __SOURCE_DIRECTORY__: a ContinuousIntegrationBuild remaps source paths to `/_/`.
    let private repoRoot : string =
        let rec walk (dir : string) : string =
            if String.IsNullOrEmpty dir then
                failwith $"Could not locate WoofWare.PawPrint.slnx by walking up from %s{testOutputDir}"
            elif File.Exists (Path.Combine (dir, "WoofWare.PawPrint.slnx")) then
                dir
            else
                walk (Path.GetDirectoryName dir)

        walk testOutputDir

    type private Project =
        {
            /// The project file's name without its extension, which is its assembly's name.
            Name : string
            IsPackable : bool
            GeneratesDocumentation : bool
        }

    /// The unconditional value of an MSBuild property set directly in the project file, refusing
    /// any shape this module would have to evaluate MSBuild to read correctly.
    let private property (projectPath : string) (doc : XDocument) (name : string) : string option =
        match doc.Descendants (XName.Get name) |> Seq.toList with
        | [] -> None
        | [ e ] ->
            if
                not (isNull (e.Attribute (XName.Get "Condition")))
                || not (isNull (e.Parent.Attribute (XName.Get "Condition")))
            then
                failwith $"%s{projectPath} sets %s{name} conditionally; this test reads only unconditional properties"

            Some (e.Value.Trim ())
        | _ -> failwith $"%s{projectPath} sets %s{name} more than once; this test reads it only when it is set once"

    let private parseBool (projectPath : string) (name : string) (value : string) : bool =
        match value.ToLowerInvariant () with
        | "true" -> true
        | "false" -> false
        | _ -> failwith $"%s{projectPath} sets %s{name} to %s{value}, which is neither true nor false"

    let private readProject (relativePath : string) : Project =
        let projectPath = Path.Combine (repoRoot, relativePath)
        let doc = XDocument.Load projectPath
        let property = property projectPath doc

        if (property "AssemblyName").IsSome then
            failwith
                $"%s{projectPath} sets AssemblyName; this test assumes a project's assembly is named after its file"

        let isPackable =
            match property "IsPackable" with
            | None ->
                // The SDK's default depends on what kind of project it is; making each project say
                // keeps this test from having to know those rules.
                failwith $"%s{projectPath} does not state IsPackable; state it explicitly"
            | Some value -> parseBool projectPath "IsPackable" value

        {
            Name = Path.GetFileNameWithoutExtension relativePath
            IsPackable = isPackable
            GeneratesDocumentation =
                property "GenerateDocumentationFile"
                |> Option.map (parseBool projectPath "GenerateDocumentationFile")
                |> Option.defaultValue false
        }

    let private publishedProjects () : Project list =
        let solution = XDocument.Load (Path.Combine (repoRoot, "WoofWare.PawPrint.slnx"))

        solution.Descendants (XName.Get "Project")
        |> Seq.map (fun p -> p.Attribute(XName.Get "Path").Value)
        |> Seq.map readProject
        |> Seq.filter (fun p -> p.IsPackable)
        |> Seq.toList

    [<Test>]
    let ``every published project generates XML documentation unless it is named as undocumented`` () : unit =
        let published = publishedProjects ()

        published
        |> List.filter (fun p -> not p.GeneratesDocumentation)
        |> List.map (fun p -> p.Name)
        |> Set.ofList
        |> shouldEqual undocumented

    [<Test>]
    let ``every published project's XML documentation file is well-formed`` () : unit =
        let documented =
            publishedProjects ()
            |> List.filter (fun p -> p.GeneratesDocumentation)
            |> List.map (fun p -> p.Name)

        // Guards against reading no projects at all, which would pass vacuously.
        documented |> shouldNotEqual []

        let problems =
            documented
            |> List.choose (fun name ->
                let path = Path.Combine (testOutputDir, name + ".xml")

                if not (File.Exists path) then
                    Some
                        $"%s{name}: no documentation file at %s{path}; WoofWare.PawPrint.Test must reference the project, directly or transitively"
                else
                    try
                        let members = XDocument.Load(path).Descendants (XName.Get "member") |> Seq.length

                        if members < MinimumMembers then
                            Some $"%s{name}: %i{members} <member> elements, fewer than %i{MinimumMembers}"
                        else
                            None
                    with :? XmlException as e ->
                        Some $"%s{name}: %s{e.Message}"
            )

        problems |> shouldEqual []

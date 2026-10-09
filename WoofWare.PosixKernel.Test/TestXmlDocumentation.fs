namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open System.Xml.Linq
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The package ships `WoofWare.PosixKernel.xml`, which a consumer's IDE reads to
/// show each member's documentation. The F# compiler copies a `///` comment's
/// text into it verbatim, so a raw `&`, or a `<` that opens no tag, makes the
/// whole file malformed, and an IDE then shows no documentation for any member
/// of the package.
[<TestFixture>]
module TestXmlDocumentation =

    /// The documentation file the build wrote beside the library, which MSBuild
    /// copies into this project's output along with the assembly itself.
    let private documentationFile () : string =
        let assemblyPath = typeof<UnixError>.Assembly.Location
        Path.ChangeExtension (assemblyPath, ".xml")

    [<Test>]
    let ``the generated XML documentation file is well-formed`` () : unit =
        let path = documentationFile ()

        if not (File.Exists path) then
            failwith $"expected the library's XML documentation beside the assembly, at %s{path}"

        // Throws XmlException, naming the line and column, on the first malformed construct.
        let doc = XDocument.Load path

        // A well-formed but empty file would be no evidence that the docstrings are; the
        // library documents a few thousand members.
        let members = doc.Descendants (XName.Get "member") |> Seq.length
        members |> shouldBeGreaterThan 1000

    let private kindOf (m : MemberInfo) : SourceConstructFlags option =
        m.GetCustomAttributes (typeof<CompilationMappingAttribute>, false)
        |> Seq.cast<CompilationMappingAttribute>
        |> Seq.tryHead
        |> Option.map (fun (a : CompilationMappingAttribute) ->
            a.SourceConstructFlags &&& SourceConstructFlags.KindMask
        )

    let private withoutArity (name : string) : string =
        match name.IndexOf '`' with
        | -1 -> name
        | i -> name.Substring (0, i)

    /// The name a member has in F# source, where the compiler gave it another.
    let private sourceName (m : MemberInfo) : string =
        m.GetCustomAttributes (typeof<CompilationSourceNameAttribute>, false)
        |> Seq.cast<CompilationSourceNameAttribute>
        |> Seq.tryHead
        |> Option.map (fun (a : CompilationSourceNameAttribute) -> a.SourceName)
        |> Option.defaultValue (withoutArity m.Name)

    /// A type's name as a docstring writes it: a module without the `Module`
    /// suffix the compiler adds when a type shares its name, and a nested type
    /// after its container's name.
    let rec private dottedName (t : Type) : string =
        let own =
            let name = withoutArity t.Name

            if kindOf t = Some SourceConstructFlags.Module && name.EndsWith "Module" then
                name.Substring (0, name.Length - "Module".Length)
            else
                name

        if isNull t.DeclaringType then
            own
        else
            $"%s{dottedName t.DeclaringType}.%s{own}"

    let rec private reachable (t : Type) : bool =
        if t.IsNested then
            t.IsNestedPublic && reachable t.DeclaringType
        else
            t.IsPublic

    let private allMembers : BindingFlags =
        BindingFlags.Public
        ||| BindingFlags.NonPublic
        ||| BindingFlags.Static
        ||| BindingFlags.Instance
        ||| BindingFlags.DeclaredOnly

    /// Every name a docstring could give one of the library's types, modules,
    /// functions, fields or union cases, with whether a client can reach
    /// something of that name: `true` if any member so named is public.
    let private namesInSource (assembly : Assembly) : Map<string, bool> =
        let types =
            assembly.GetTypes ()
            |> Array.filter (fun (t : Type) -> not (t.Name.Contains "@") && not (t.FullName.Contains "<"))

        seq {
            for t in types do
                let typeName = dottedName t
                yield typeName, reachable t

                for p in t.GetProperties allMembers do
                    let getter = p.GetGetMethod true

                    if not (isNull getter) then
                        yield $"%s{typeName}.%s{sourceName p}", reachable t && getter.IsPublic

                for m in t.GetMethods allMembers do
                    if not m.IsSpecialName && not (m.Name.Contains "@") then
                        // A union case with fields is made by a method `New<Case>`.
                        let name =
                            if kindOf m = Some SourceConstructFlags.UnionCase && m.Name.StartsWith "New" then
                                m.Name.Substring 3
                            else
                                sourceName m

                        yield $"%s{typeName}.%s{name}", reachable t && m.IsPublic

                for f in t.GetFields allMembers do
                    if f.IsLiteral || (f.IsStatic && not (f.Name.Contains "@")) then
                        yield $"%s{typeName}.%s{f.Name}", reachable t && f.IsPublic
        }
        |> Seq.groupBy fst
        |> Seq.map (fun (name, entries) -> name, Seq.exists snd entries)
        |> Map.ofSeq

    /// Every type of `assembly`, by the name the documentation file gives it.
    let private typesByDocId (assembly : Assembly) : Map<string, Type> =
        assembly.GetTypes ()
        |> Array.map (fun (t : Type) -> t.FullName.Replace ('+', '.'), t)
        |> Map.ofArray

    /// Whether a client can reach the member the documentation file names by
    /// `docId` (`T:`, `M:`, `P:`, `F:` or `E:`, then the member's full name), or
    /// `None` where the member cannot be found to say.
    let private documentedMemberReachable (types : Map<string, Type>) (docId : string) : bool option =
        let path =
            match docId.IndexOf '(' with
            | -1 -> docId.Substring 2
            | i -> docId.Substring (2, i - 2)

        let asMember () : bool option =
            let split = path.LastIndexOf '.'
            let declaring = path.Substring (0, split)

            let name =
                match path.IndexOf ("``", split) with
                | -1 -> path.Substring (split + 1)
                | i -> path.Substring (split + 1, i - split - 1)

            match Map.tryFind declaring types with
            | None -> None
            | Some t ->
                let candidates =
                    if name = "#ctor" then
                        t.GetConstructors allMembers |> Array.map (fun c -> c.IsPublic)
                    else
                        Array.concat
                            [
                                // A union case with fields, where the union is a struct or has one
                                // case, is documented as a type but compiled as its maker `New<Case>`.
                                t.GetMethods allMembers
                                |> Array.filter (fun m -> m.Name = name || m.Name = $"New%s{name}")
                                |> Array.map (fun m -> m.IsPublic)
                                t.GetProperties allMembers
                                |> Array.filter (fun p -> p.Name = name)
                                |> Array.map (fun p ->
                                    let getter = p.GetGetMethod true
                                    not (isNull getter) && getter.IsPublic
                                )
                                t.GetFields allMembers
                                |> Array.filter (fun f -> f.Name = name)
                                |> Array.map (fun f -> f.IsPublic)
                            ]

                if Array.isEmpty candidates then
                    None
                else
                    Some (reachable t && Array.contains true candidates)

        match docId.[0] with
        | 'T' ->
            match Map.tryFind path types with
            | Some t -> Some (reachable t)
            // A union case without its own class is documented as a type that does
            // not exist: see `asMember`.
            | None -> asMember ()
        | _ -> asMember ()

    let private codeSpan : Regex = Regex "`([^`]+)`"

    let private dottedIdentifier : Regex =
        Regex @"(?<![\w.])([A-Z][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*)"

    /// The names `text` mentions that the library has but a client cannot reach.
    /// A dotted name is read as the longest prefix of it that names something,
    /// so `UnixSystem.tasks` is the function and not the module.
    let private unreachableNames (names : Map<string, bool>) (text : string) : string list =
        [
            for span in dottedIdentifier.Matches (text.Replace ("WoofWare.PosixKernel.", "")) do
                let parts = span.Groups.[1].Value.Split '.'

                let named =
                    [ parts.Length .. -1 .. 1 ]
                    |> List.tryPick (fun (length : int) ->
                        let prefix = String.Join (".", parts.[.. length - 1])

                        Map.tryFind prefix names
                        |> Option.map (fun (isReachable : bool) -> prefix, isReachable)
                    )

                match named with
                | Some (name, false) -> yield name
                | Some (_, true)
                | None -> ()
        ]

    /// A client reads a public member's documentation in its IDE, so a name it
    /// mentions must be one the client can look up: pointing at an internal
    /// member sends the reader somewhere it cannot follow.
    [<Test>]
    let ``public documentation names nothing a client cannot reach`` () : unit =
        let assembly = typeof<UnixError>.Assembly
        let names = namesInSource assembly
        let types = typesByDocId assembly
        let doc = XDocument.Load (documentationFile ())

        // Controls, so that this cannot pass by reading no names at all: one name each way.
        Map.tryFind "UnixSystem.processorCount" names |> shouldEqual (Some true)
        Map.tryFind "UnixMachineState.processorCount" names |> shouldEqual (Some false)

        // A union case without its own class is documented as a type that does not exist.
        documentedMemberReachable types "T:WoofWare.PosixKernel.WakePrimitive.SignalDeliverable"
        |> shouldEqual (Some true)

        let documented =
            doc.Descendants (XName.Get "member")
            |> Seq.map (fun (memberElement : XElement) ->
                memberElement.Attribute(XName.Get "name").Value, memberElement
            )
            |> List.ofSeq

        // A member this cannot place would be skipped silently, so every one must be placed.
        documented
        |> List.filter (fun (id : string, _) -> documentedMemberReachable types id = None)
        |> List.map fst
        |> shouldEqual []

        let offences =
            [
                for id, memberElement in documented do
                    if documentedMemberReachable types id = Some true then
                        let mentions =
                            [
                                for span in codeSpan.Matches memberElement.Value do
                                    yield span.Groups.[1].Value
                                for c in memberElement.Descendants (XName.Get "c") do
                                    yield c.Value
                                for element in memberElement.Descendants () do
                                    match element.Attribute (XName.Get "cref") with
                                    | null -> ()
                                    | cref -> yield cref.Value.Substring (cref.Value.IndexOf ':' + 1)
                            ]

                        for name in mentions |> List.collect (unreachableNames names) |> List.distinct do
                            yield $"%s{id} mentions `%s{name}`"
            ]

        offences |> shouldEqual []

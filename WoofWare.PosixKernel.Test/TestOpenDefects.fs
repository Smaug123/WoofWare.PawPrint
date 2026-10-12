namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open System.Text.RegularExpressions
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// A defect that a `checkInvariants` function reports is open: its cases are
/// hidden, and it has no patterns at all. It is a bug in the library, which a
/// client reports with its `describe` and never branches on. These tests hold
/// the suffix `Defect` to meaning such a union, or the error of a parser.
[<TestFixture>]
module TestOpenDefects =

    /// The defects whose cases are hidden, and which have no patterns.
    let private openDefects : Type list =
        [
            typeof<FileDescriptorRegistryDefect>
            typeof<SimulatedMachineDefect<int>>
            typeof<UnixSystemDefect<int>>
            typeof<VirtualFileSystemDefect>
        ]

    /// Public unions named `*Defect` that are the errors of parsers, which say
    /// what was wrong with the input and keep public cases.
    let private closedDefects : Set<string> =
        Set.ofList [ "UnixByteStringDefect" ; "UnixPathTextDefect" ]

    let private library : Assembly = typeof<UnixError>.Assembly

    let private openDefectCases () : TestCaseData seq =
        openDefects |> List.map (fun (t : Type) -> TestCaseData t) |> Seq.ofList

    /// Public as well as non-public: with `NonPublic` alone, a union whose
    /// cases are public answers none.
    let private everyCase : BindingFlags =
        BindingFlags.Public ||| BindingFlags.NonPublic

    let private hiddenCases (t : Type) : UnionCaseInfo list =
        FSharpType.GetUnionCases (t, everyCase) |> List.ofArray

    /// A nested `Reason` module of a union's module, if it has one: the
    /// patterns a defect does not have.
    let private reasonModule (t : Type) : Type option =
        let name = $"%s{LibraryNames.dottedName t}.Reason"

        library.GetTypes ()
        |> Array.filter (fun (m : Type) ->
            LibraryNames.kindOf m = Some SourceConstructFlags.Module
            && LibraryNames.dottedName m = name
        )
        |> Array.tryExactlyOne

    /// The `describe` of an open defect's module, instantiated for `t`'s type
    /// arguments where the union is generic.
    let private describeOf (t : Type) : MethodInfo =
        let name = LibraryNames.dottedName t

        let candidates =
            library.GetExportedTypes ()
            |> Array.filter (fun (m : Type) ->
                LibraryNames.kindOf m = Some SourceConstructFlags.Module
                && LibraryNames.dottedName m = name
            )
            |> Array.collect (fun (m : Type) ->
                m.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
                |> Array.filter (fun (method : MethodInfo) -> method.Name = "describe")
            )

        match candidates with
        | [| describe |] ->
            if describe.IsGenericMethodDefinition then
                describe.MakeGenericMethod (t.GetGenericArguments ())
            else
                describe
        | candidates -> failwith $"expected exactly one public %s{name}.describe, but found %d{candidates.Length}"

    [<TestCaseSource(nameof openDefectCases)>]
    let ``an open defect hides its cases, has no patterns, and describes each case by its name`` (t : Type) : unit =
        FSharpType.IsUnion t |> shouldEqual false
        FSharpType.IsUnion (t, everyCase) |> shouldEqual true
        reasonModule t |> shouldEqual None

        let describe = describeOf t

        describe.GetParameters ()
        |> Array.map (fun (p : ParameterInfo) -> p.ParameterType)
        |> shouldEqual [| t |]

        describe.ReturnType |> shouldEqual typeof<string>

        let cases = hiddenCases t
        // A control, so that this cannot pass by checking nothing.
        cases |> shouldNotEqual []

        // A bug report names the rule that broke, whatever the values that
        // broke it, so each field holds its type's default.
        for case in cases do
            let fields =
                case.GetFields ()
                |> Array.map (fun (f : PropertyInfo) ->
                    if f.PropertyType.IsValueType then
                        Activator.CreateInstance f.PropertyType
                    else
                        null
                )

            let defect = FSharpValue.MakeUnion (case, fields, true)
            let described = describe.Invoke ((null : obj), [| defect |]) :?> string

            if not (Regex.IsMatch (described, $@"\b%s{case.Name}\b")) then
                failwith
                    $"expected %s{LibraryNames.dottedName t}.describe to name the case %s{case.Name}, but it said: %s{described}"

    [<Test>]
    let ``a public union named Defect is the error of a parser, or is open`` () : unit =
        let defects =
            library.GetExportedTypes ()
            |> Array.filter (fun (t : Type) -> not t.IsNested && (LibraryNames.withoutArity t.Name).EndsWith "Defect")
            |> List.ofArray

        let named (types : Type list) : Set<string> =
            types
            |> List.map (fun (t : Type) -> LibraryNames.withoutArity t.Name)
            |> Set.ofList

        let publicUnions = defects |> List.filter FSharpType.IsUnion |> named

        let hiddenUnions =
            defects
            |> List.filter (fun (t : Type) -> not (FSharpType.IsUnion t) && FSharpType.IsUnion (t, everyCase))
            |> named

        let declaredOpen = named openDefects

        // Controls, so that this cannot pass by finding nothing.
        publicUnions |> shouldContain "UnixByteStringDefect"
        hiddenUnions |> shouldContain "UnixSystemDefect"

        let faults =
            [
                for name in Set.difference publicUnions closedDefects do
                    yield
                        $"%s{name} is a public union named Defect. If it is a bug report, hide its cases (internal representation, a describe, and an entry in openDefects); if it is the error of a parser, add it to closedDefects"
                for name in Set.difference closedDefects publicUnions do
                    yield $"%s{name} is listed as a public union named Defect, and is not one. Remove it from the list"
                for name in Set.difference hiddenUnions declaredOpen do
                    yield
                        $"%s{name} hides its cases and is not in openDefects, so nothing checks its describe. Add it there"
                for name in Set.difference declaredOpen hiddenUnions do
                    yield $"%s{name} is in openDefects, and is not a union with hidden cases"
            ]

        faults |> shouldEqual []

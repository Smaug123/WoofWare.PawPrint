namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// An F# string literal written `"$foo: {bar}"` — dollar *inside* the quotes — compiles as
/// those exact characters, silently: no interpolation happens and no warning fires. The
/// intended spelling was `$"foo: {bar}"`. Every such literal reaches the built assembly's
/// user-string heap verbatim, so scanning that heap catches the whole class wherever it
/// occurs, including in failure messages whose arms no test can reach.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNoMissedInterpolation =

    /// Every string literal in the assembly's user-string heap, by walking it end to end.
    let private userStrings (assemblyPath : string) : string list =
        use file = File.OpenRead assemblyPath
        use pe = new PEReader (file)
        let reader = pe.GetMetadataReader ()

        // Offset 0 is the heap's mandatory empty entry; the first real string starts at 1.
        let rec loop (handle : UserStringHandle) (acc : string list) : string list =
            if handle.IsNil then
                List.rev acc
            else
                loop (reader.GetNextHandle handle) (reader.GetUserString handle :: acc)

        loop (MetadataTokens.UserStringHandle 1) []

    let private assembliesUnderTest : (string * string) list =
        [
            "WoofWare.PawPrint", typeof<WoofWare.PawPrint.IlMachineState>.Assembly.Location
            "WoofWare.PawPrint.Domain", typeof<WoofWare.PawPrint.TypeDefn>.Assembly.Location
        ]

    [<Test>]
    let ``no string literal is a missed interpolation`` () : unit =
        for name, path in assembliesUnderTest do
            let strings = userStrings path

            // Guard against a vacuously-passing walk: both assemblies carry far more
            // literals than this. If the enumeration convention ever changes, fail
            // here rather than reporting a clean scan of nothing.
            strings.Length |> shouldBeGreaterThan 100

            let offenders =
                strings
                |> List.filter (fun s -> s.StartsWith "$" && s.Contains "{")
                |> List.distinct

            if not offenders.IsEmpty then
                let rendered = offenders |> List.map (sprintf "%A") |> String.concat "; "

                failwith
                    $"%s{name} contains %d{offenders.Length} string literal(s) that look like a missed interpolation (a `$` inside the quotes rather than before them): %s{rendered}"

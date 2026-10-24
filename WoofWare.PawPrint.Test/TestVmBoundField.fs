namespace WoofWare.PawPrint.Test

open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open WoofWare.PawPrint

/// `VmBoundField.all` is exactly the CoreLib fields that the pinned `vm/corelib.h` of some supported
/// runtime binds by name.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestVmBoundField =

    /// The namespace each `g_<token>NS` of `vm/namespace.h` spells, by its token: each is a string
    /// literal or another such macro, or several pasted together.
    let private namespaces (path : string) : Map<string, string> =
        let defines =
            File.ReadAllLines path
            |> Array.choose (fun line ->
                let m = Regex.Match (line, @"^#define\s+g_(\w+)NS\s+(.*)")

                if m.Success then
                    Some (m.Groups.[1].Value, (m.Groups.[2].Value.Split "//").[0].Trim ())
                else
                    None
            )
            |> Map.ofArray

        let rec resolve (token : string) : string =
            Regex.Matches (defines.[token], "\"[^\"]*\"|g_(\\w+)NS")
            |> Seq.map (fun part ->
                if part.Value.StartsWith "\"" then
                    part.Value.Trim '"'
                else
                    resolve part.Groups.[1].Value
            )
            |> String.concat ""

        defines |> Map.map (fun token _ -> resolve token)

    /// Every field the `corelib.h` under `root` binds: each `DEFINE_FIELD`, of the class its id
    /// names by a `DEFINE_CLASS` (one commented out among them, for a class `binder.h` gives the
    /// id), and each `DEFINE_FIELD_U`, of the class of the `DEFINE_CLASS_U` before it.
    let private bound (root : string) : Set<string * string> =
        let vm = Path.Combine (root, "src", "coreclr", "vm")
        let namespaces = namespaces (Path.Combine (vm, "namespace.h"))
        let fullName (token : string) (name : string) = $"%s{namespaces.[token]}.%s{name}"

        let classRow =
            Regex @"^(?://\s*)?DEFINE_CLASS\(\s*(\w+)\s*,\s*(\w+)\s*,\s*([^\s,)]+)\s*\)"

        let classURow = Regex @"^DEFINE_CLASS_U\(\s*(\w+)\s*,\s*([^\s,)]+)\s*,\s*(\w+)\s*\)"
        let fieldRow = Regex @"^DEFINE_FIELD\(\s*(\w+)\s*,\s*(\w+)\s*,\s*(\w+)\s*\)"
        let fieldURow = Regex @"^DEFINE_FIELD_U\(\s*(\w+)\s*,\s*(\w+)\s*,\s*(\w+)\s*\)"

        let _, _, fields =
            ((Map.empty, None, Set.empty), File.ReadAllLines (Path.Combine (vm, "corelib.h")))
            ||> Array.fold (fun (classes, current, fields) line ->
                let line = line.Trim ()
                let classMatch = classRow.Match line
                let classUMatch = classURow.Match line
                let fieldMatch = fieldRow.Match line
                let fieldUMatch = fieldURow.Match line

                if classMatch.Success then
                    let named = fullName classMatch.Groups.[2].Value classMatch.Groups.[3].Value
                    Map.add classMatch.Groups.[1].Value named classes, current, fields
                elif classUMatch.Success then
                    classes, Some (fullName classUMatch.Groups.[1].Value classUMatch.Groups.[2].Value), fields
                elif fieldMatch.Success then
                    match Map.tryFind fieldMatch.Groups.[1].Value classes with
                    | Some named -> classes, current, Set.add (named, fieldMatch.Groups.[3].Value) fields
                    | None -> failwith $"corelib.h binds a field of a class it names no DEFINE_CLASS for: %s{line}"
                elif fieldUMatch.Success then
                    match current with
                    | Some named -> classes, current, Set.add (named, fieldUMatch.Groups.[1].Value) fields
                    | None -> failwith $"corelib.h has a DEFINE_FIELD_U row before any DEFINE_CLASS_U: %s{line}"
                else
                    classes, current, fields
            )

        fields

    [<Test>]
    let ``the table is every field corelib.h binds by name, on every supported runtime`` () : unit =
        let expected =
            EmulatedRuntime.supported
            |> List.map (FrameworkUnderTest.requireRuntimeSourceOf >> bound)
            |> Set.unionMany

        let table = VmBoundField.all.Force ()
        let missing = Set.difference expected table
        let extra = Set.difference table expected

        if not missing.IsEmpty || not extra.IsEmpty then
            let rows (fields : Set<string * string>) =
                fields
                |> Seq.map (fun (typeName, fieldName) -> $"%s{typeName}\t%s{fieldName}")
                |> String.concat "\n"

            failwith
                $"VmBoundFields.tsv lacks these rows corelib.h binds:\n%s{rows missing}\nand has these it does not:\n%s{rows extra}"

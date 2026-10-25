namespace WoofWare.PawPrint.Test

open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `VmLaidOutClass.all` is exactly the CoreLib types that the pinned C++ of some supported runtime
/// lays out as a class of its own.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestVmLaidOutClass =

    /// Every type the `corelib.h` under `root` checks the layout of with a `DEFINE_CLASS_U` row, with
    /// the C++ class the row names, or, where it names `NoClass`, the one class its `DEFINE_FIELD_U`
    /// rows lay the type's fields out in.
    let private byRow (root : string) : Set<string * string> =
        let vm = Path.Combine (root, "src", "coreclr", "vm")
        let namespaces = TestVmBoundField.namespaces (Path.Combine (vm, "namespace.h"))

        let classURow =
            Regex @"^DEFINE_CLASS_U\(\s*(\w+)\s*,\s*([^\s,)]+)\s*,\s*([\w:]+)\s*\)"

        let fieldURow = Regex @"^DEFINE_FIELD_U\(\s*(\w+)\s*,\s*([\w:]+)\s*,\s*(\w+)\s*\)"

        // Each class row with the containing classes of the field rows under it, latest first.
        let classes =
            ([], File.ReadAllLines (Path.Combine (vm, "corelib.h")))
            ||> Array.fold (fun classes line ->
                let line = line.Trim ()
                let classUMatch = classURow.Match line
                let fieldUMatch = fieldURow.Match line

                if classUMatch.Success then
                    let typeName =
                        $"%s{namespaces.[classUMatch.Groups.[1].Value]}.%s{classUMatch.Groups.[2].Value}"

                    (typeName, classUMatch.Groups.[3].Value, []) :: classes
                elif fieldUMatch.Success then
                    match classes with
                    | (typeName, native, containers) :: rest ->
                        (typeName, native, fieldUMatch.Groups.[2].Value :: containers) :: rest
                    | [] -> failwith $"corelib.h has a DEFINE_FIELD_U row before any DEFINE_CLASS_U: %s{line}"
                elif line.StartsWith "DEFINE_CLASS_U(" || line.StartsWith "DEFINE_FIELD_U(" then
                    failwith $"corelib.h has a layout row that the check cannot read: %s{line}"
                else
                    classes
            )

        classes
        |> List.map (fun (typeName, native, containers) ->
            match native, List.distinct containers with
            | "NoClass", [ container ] -> typeName, container
            | "NoClass", containers ->
                failwith $"corelib.h lays %s{typeName} out in no one class: its fields are laid out in %A{containers}"
            | native, containers when containers |> List.forall (fun container -> container = native) ->
                typeName, native
            | native, containers ->
                failwith $"corelib.h lays %s{typeName} out as %s{native}, but lays its fields out in %A{containers}"
        )
        |> Set.ofList

    /// A `class` or `struct` the C++ declares with a body: its name, its base class, and the file.
    type private Declaration =
        {
            Name : string
            Base : string option
            File : string
        }

    let private declarationLine =
        Regex @"^\s*(?:class|struct)\s+(\w+)(?:\s+final)?\s*(?::\s*(?:public|protected|private)?\s*(\w+))?\s*(?:\{.*)?$"

    /// Every class or struct the C++ under `vm` declares with a body.
    let private declarations (vm : string) : Declaration list =
        Directory.EnumerateFiles (vm, "*", SearchOption.AllDirectories)
        |> Seq.filter (fun path ->
            match Path.GetExtension path with
            | ".h"
            | ".hpp"
            | ".inl"
            | ".cpp" -> true
            | _ -> false
        )
        |> Seq.collect (fun path ->
            File.ReadAllLines path
            |> Seq.choose (fun line ->
                let m = declarationLine.Match line

                // A line ending in `;` declares a class without defining it.
                if m.Success && not (line.TrimEnd().EndsWith ';') then
                    Some
                        {
                            Name = m.Groups.[1].Value
                            Base =
                                if m.Groups.[2].Success then
                                    Some m.Groups.[2].Value
                                else
                                    None
                            File = Path.GetFileName path
                        }
                else
                    None
            )
        )
        |> List.ofSeq

    /// The C++ classes under `root`'s `vm` that lay a managed object out: each that derives from
    /// `Object`, and each `object.h` declares beside those.
    let private nativeClasses (root : string) : Set<string> =
        let declared = declarations (Path.Combine (root, "src", "coreclr", "vm"))

        let bases =
            declared
            |> List.groupBy (fun declaration -> declaration.Name)
            |> List.map (fun (name, declarations) -> name, declarations |> List.choose (fun d -> d.Base) |> Set.ofList)
            |> Map.ofList

        let rec derivesFromObject (seen : Set<string>) (name : string) : bool =
            name = "Object"
            || (not (seen.Contains name)
                && match bases.TryFind name with
                   | Some bases -> bases |> Set.exists (derivesFromObject (seen.Add name))
                   | None -> false)

        declared
        |> List.filter (fun declaration ->
            declaration.File = "object.h" || derivesFromObject Set.empty declaration.Name
        )
        |> List.map (fun declaration -> declaration.Name)
        |> Set.ofList

    /// The C++ classes among those that lay out no CoreLib type, each with why not.
    let private laysOutNoCoreLibType : Map<string, string> =
        Map.ofList
            [
                "Array", "an array's instance, which no type definition declares"
                "PtrArray", "an array of references' instance, which no type definition declares"
                "BaseObjectWithCachedData",
                "a base the C++ factors out of the reflection objects, with no managed class"
                "NativeAssemblyNameParts", "a struct the VM fills and hands managed code a pointer to, not an object"
                "StackTraceArray", "a view over the byte array an exception's stack trace is kept in"
                "ArrayHeader", "the header of that byte array"
            ]

    let private roots () : string list =
        EmulatedRuntime.supported |> List.map FrameworkUnderTest.requireRuntimeSourceOf

    [<Test>]
    let ``the table is every type the C++ of a supported runtime lays out`` () : unit =
        let roots = roots ()
        let byRow = roots |> List.map byRow |> Set.unionMany
        let natives = roots |> List.map nativeClasses |> Set.unionMany
        let table = VmLaidOutClass.all.Force ()
        let tableNatives = table |> Set.map snd

        let missing = Set.difference byRow table

        let unaccounted =
            natives
            |> Set.filter (fun native ->
                not (tableNatives.Contains native)
                && not (laysOutNoCoreLibType.ContainsKey native)
            )

        let extra =
            table
            |> Set.filter (fun (typeName, native) ->
                not (byRow.Contains (typeName, native))
                && (not (natives.Contains native) || laysOutNoCoreLibType.ContainsKey native)
            )

        let stale =
            laysOutNoCoreLibType
            |> Map.toList
            |> List.filter (fun (native, _) -> not (natives.Contains native))

        if
            not missing.IsEmpty
            || not unaccounted.IsEmpty
            || not extra.IsEmpty
            || not stale.IsEmpty
        then
            let rows (classes : Set<string * string>) =
                classes
                |> Seq.map (fun (typeName, native) -> $"%s{typeName}\t%s{native}")
                |> String.concat "\n"

            let unaccounted = String.concat "\n" unaccounted

            failwith
                $"VmLaidOutClasses.tsv lacks these rows corelib.h checks the layout of:\n%s{rows missing}\nand lays out no type as these C++ classes:\n%s{unaccounted}\nand has these rows nothing in the runtime's C++ lays out:\n%s{rows extra}\nand the check excuses these C++ classes the runtime no longer declares:\n%A{stale}"

    [<Test>]
    let ``each type the table names without a corelib.h row is one the CoreLib under test declares`` () : unit =
        let byRow = roots () |> List.map byRow |> Set.unionMany
        let corelib = TestIntrinsicBody.coreLib AuditedCoreLib.UnderTest

        let declared =
            corelib.TypeDefs.Values
            |> Seq.map (TypeInfo.fullName (fun handle -> corelib.TypeDefs.[handle]))
            |> Set.ofSeq

        let handWritten =
            VmLaidOutClass.all.Force () |> Set.filter (fun row -> not (byRow.Contains row))

        // The check says something only where the table has rows no corelib.h row gives it.
        handWritten.IsEmpty |> shouldEqual false

        handWritten
        |> Set.filter (fun (typeName, _) -> not (declared.Contains typeName))
        |> shouldEqual Set.empty

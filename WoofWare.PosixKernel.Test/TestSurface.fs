namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open ApiSurface
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// The library's public API, held to `WoofWare.PosixKernel/SurfaceBaseline.txt`, which the
/// library embeds. A change to the API fails the first test until the baseline is regenerated
/// by `Update API surface` and committed, so every API change shows up in review as a diff of
/// that file. The baseline also records the tag of each case of each public union, and a union
/// that loses or reorders cases, or gains a case anywhere but at its end, fails a second test.
/// A public record that is new since the baseline, or a field a committed record gains, may not
/// reuse a field label that another committed record has, which a third test checks.
[<TestFixture>]
module TestSurface =

    let private assembly = typeof<UnixError>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () : unit = ApiSurface.assertIdentical assembly

    [<Test>]
    let ``Every public union keeps its committed cases at their tags`` () : unit =
        let committed =
            ApiSurface.ofAssemblyBaseline assembly
            |> ApiSurface.toString
            |> UnionCaseOrder.parse

        let current =
            ApiSurface.ofAssembly assembly |> ApiSurface.toString |> UnionCaseOrder.parse

        match UnionCaseOrder.violations committed current with
        | [] -> ()
        | violations ->
            let listed =
                violations
                |> List.map (fun (v : CaseOrderViolation) -> "  " + UnionCaseOrder.describe v)
                |> String.concat "\n"

            Assert.Fail (
                "Public unions no longer keep the cases SurfaceBaseline.txt records at the tags it records:\n"
                + listed
                + "\nAn F# client compiled against the committed API may identify a case by its tag, which is "
                + "its position, so for these it would run the wrong arm even for cases it already knew, which "
                + "an appended case does not make it do. From 1.0 either is a major version (README, \"Unions "
                + "that gain cases\"). Before 1.0 such a change may be made deliberately: regenerate the baseline with `nix develop -c dotnet test "
                + "WoofWare.PosixKernel.Test/WoofWare.PosixKernel.Test.fsproj --filter "
                + "\"FullyQualifiedName~TestSurface&Name~Update\"` and commit it with the change."
            )

    /// Every public record in the built assembly, with its field labels.
    let internal publicRecords (built : Assembly) : Map<string, Set<string>> =
        built.GetExportedTypes ()
        |> Array.filter (fun (t : Type) -> FSharpType.IsRecord (t, BindingFlags.Public))
        |> Array.map (fun (t : Type) ->
            let labels =
                FSharpType.GetRecordFields (t, BindingFlags.Public)
                |> Array.map (fun (p : PropertyInfo) -> p.Name)
                |> Set.ofArray

            t.FullName, labels
        )
        |> Map.ofArray

    [<Test>]
    let ``No record newly reuses a field label that a committed record has`` () : unit =
        let baseline = ApiSurface.ofAssemblyBaseline assembly |> ApiSurface.toString
        let current = publicRecords assembly

        // An ApiSurface listing tells neither a record from a class nor a field from a property
        // declared as a member. So a committed record is a record of the built assembly that the
        // baseline lists, and its committed labels are those of its labels now that the baseline
        // lists as its properties. A record deleted since the baseline constrains nothing, since
        // no client code compiled against this version can mean it.
        let committed =
            current
            |> Map.filter (fun (record : string) _ -> RecordLabels.listsType baseline record)
            |> Map.map (fun (record : string) (labels : Set<string>) ->
                Set.intersect labels (RecordLabels.listedProperties baseline record)
            )

        match RecordLabels.collisions committed current with
        | [] -> ()
        | collisions ->
            let listed =
                collisions
                |> List.map (fun (c : LabelCollision) -> "  " + RecordLabels.describe c)
                |> String.concat "\n"

            Assert.Fail (
                "Public records newly use field labels that other records in SurfaceBaseline.txt have:\n"
                + listed
                + "\nF# resolves a label that several records share to the most recently declared of them, "
                + "in a record pattern, in an `x.Label` access on a value whose type is not yet inferred, "
                + "and in a record expression whose labels do not single out one record, so client code "
                + "that meant the committed record may now mean this one and stop compiling. Before 1.0, "
                + "rename the field. Regenerating the baseline instead grandfathers the collision, so "
                + "that is a deliberate choice."
            )

    /// Rewrites the source tree's baseline from the built assembly, and touches nothing else.
    [<Test ; Explicit>]
    let ``Update API surface`` () : unit =
        // Not `ApiSurface.writeAssemblyBaseline`: that also edits version.json, raising the major
        // version whenever anything is removed from the API, and below 1.0 a breaking change is not
        // meant to move the package to 1.0. When the package reaches 1.0, switch to
        // `writeAssemblyBaseline`, and add a test calling `MonotonicVersion.validate`.
        let baselinePath =
            match Assembly.findProjectFiles (fun _ -> [ "SurfaceBaseline.txt" ]) assembly with
            | [ path ] -> path
            | paths -> failwith $"expected exactly one SurfaceBaseline.txt location, found %A{paths}"

        // The same bytes `writeAssemblyBaseline` writes: UTF-8 without a byte-order mark, and no
        // trailing newline.
        File.WriteAllText (baselinePath, ApiSurface.ofAssembly assembly |> ApiSurface.toString)

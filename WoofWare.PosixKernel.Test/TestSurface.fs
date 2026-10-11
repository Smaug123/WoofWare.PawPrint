namespace WoofWare.PosixKernel.Test

open System.IO
open ApiSurface
open NUnit.Framework
open WoofWare.PosixKernel

/// The library's public API, held to `WoofWare.PosixKernel/SurfaceBaseline.txt`, which the
/// library embeds. A change to the API fails the first test until the baseline is regenerated
/// by `Update API surface` and committed, so every API change shows up in review as a diff of
/// that file. The baseline also records the tag of each case of each public union, and a union
/// that gains a case anywhere but at its end, or loses or reorders cases, fails a second test.
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
                + "its position, so it would run the wrong arm for these. From 1.0 a union may only gain cases "
                + "at its end (README, \"Unions that gain cases\"). Before 1.0 such a change may be made "
                + "deliberately: regenerate the baseline with `nix develop -c dotnet test "
                + "WoofWare.PosixKernel.Test/WoofWare.PosixKernel.Test.fsproj --filter "
                + "\"FullyQualifiedName~TestSurface&Name~Update\"` and commit it with the change."
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

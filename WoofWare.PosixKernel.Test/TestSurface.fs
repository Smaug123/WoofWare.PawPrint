namespace WoofWare.PosixKernel.Test

open System.IO
open ApiSurface
open NUnit.Framework
open WoofWare.PosixKernel

/// The library's public API, held to `WoofWare.PosixKernel/SurfaceBaseline.txt`, which the
/// library embeds. A change to the API fails the first test until the baseline is regenerated
/// by the second and committed, so every API change shows up in review as a diff of that file.
[<TestFixture>]
module TestSurface =

    let private assembly = typeof<UnixError>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () : unit = ApiSurface.assertIdentical assembly

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

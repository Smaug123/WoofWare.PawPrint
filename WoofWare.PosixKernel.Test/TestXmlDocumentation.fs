namespace WoofWare.PosixKernel.Test

open System.IO
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

namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework

/// An instruction whose dispatch fails raises the failure's exception through
/// `IlMachineStateExecution.raiseDispatchFailure`, which refuses unless `OpcodeFaults` says the
/// instruction's resolution can fail that way. So the interpreter may name
/// `DispatchFailure.exceptionType` only there, and where the failure is a callee's rather than an
/// instruction's, as an allowance below says.
///
/// The scan is over the text of every F# file under `WoofWare.PawPrint/`, embedded into the test
/// assembly by the project file, comments included.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDispatchFailureRatchet =

    let private pattern = Regex @"DispatchFailure\.exceptionType\b"

    type private Allowance =
        {
            File : string
            /// Exactly how many times `File` names it, so a second use there is not waved through
            /// by the first one's reason.
            Count : int
            Reason : string
        }

    let private allowances : Allowance list =
        [
            {
                File = "IlMachineStateExecution.fs"
                Count = 1
                Reason = "`raiseDispatchFailure` itself, which the second test pins it to"
            }
            {
                File = "Native/NativeDelegate.fs"
                Count = 1
                Reason = "`CreateDelegate`'s own failure, which reaches the instruction calling it from a callee"
            }
        ]

    /// Every way `sources` (file name, text) departs from the rule.
    let private violations (sources : (string * string) list) : string list =
        let files = sources |> List.map fst |> Set.ofList

        [
            for allowance in allowances do
                if not (files.Contains allowance.File) then
                    yield $"an allowance names %s{allowance.File}, which is not a source of WoofWare.PawPrint"

            for file, text in sources do
                let found = pattern.Matches(text).Count

                let expected =
                    allowances
                    |> List.tryFind (fun allowance -> allowance.File = file)
                    |> Option.map _.Count
                    |> Option.defaultValue 0

                if found <> expected then
                    yield
                        $"%s{file} names DispatchFailure.exceptionType %d{found} time(s), where %d{expected} are allowed: raise an instruction's dispatch failure through IlMachineStateExecution.raiseDispatchFailure"
        ]

    /// The prefix the project file gives each embedded source's resource name.
    [<Literal>]
    let private ResourcePrefix = "PawPrintLibrarySource/"

    let private librarySources () : (string * string) list =
        let assembly = typeof<Allowance>.Assembly

        [
            for resource in assembly.GetManifestResourceNames () do
                if resource.StartsWith (ResourcePrefix, StringComparison.Ordinal) then
                    use stream = assembly.GetManifestResourceStream resource
                    use reader = new StreamReader (stream)
                    yield resource.Substring(ResourcePrefix.Length).Replace ('\\', '/'), reader.ReadToEnd ()
        ]

    [<Test>]
    let ``only raiseDispatchFailure raises an instruction's dispatch failure`` () : unit =
        let sources = librarySources ()

        // Guards against the embedding matching nothing, which would pass vacuously.
        sources.Length |> shouldBeGreaterThan 100

        violations sources |> shouldBeEmpty

    [<Test>]
    let ``the one use in IlMachineStateExecution is raiseDispatchFailure's`` () : unit =
        let text =
            librarySources ()
            |> List.find (fun (name, _) -> name = "IlMachineStateExecution.fs")
            |> snd

        let definition = Regex(@"\n    and raiseDispatchFailure\b").Match text
        definition.Success |> shouldEqual true

        let next =
            text.IndexOf ("\n    and ", definition.Index + 1, StringComparison.Ordinal)

        let found = pattern.Match text
        found.Success |> shouldEqual true

        (found.Index > definition.Index && (next < 0 || found.Index < next))
        |> shouldEqual true

    [<TestCase("UnaryMetadataCallOps.fs")>]
    [<TestCase("Native/NativeDelegate.fs")>]
    [<TestCase("IlMachineStateExecution.fs")>]
    let ``a further use is reported`` (file : string) : unit =
        let sources =
            librarySources ()
            |> List.map (fun (name, text) ->
                if name = file then
                    name, text + "\n// raises DispatchFailure.exceptionType baseClassTypes failure"
                else
                    name, text
            )

        sources |> List.map fst |> shouldContain file

        violations sources
        |> List.filter (fun violation -> violation.StartsWith (file + " names", StringComparison.Ordinal))
        |> List.length
        |> shouldEqual 1

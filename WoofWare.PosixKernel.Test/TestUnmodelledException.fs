namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// A call this library cannot answer throws: `UnmodelledException` where its
/// model does not cover what was asked, and the BCL's own exception where no
/// client following the documentation could have asked it. The refusals behind
/// those throws are internal values, so no public type is named for one.
///
/// Each public entry point is its internal `tryX` with the refusal raised:
/// these tests hold the two to agreeing, answer for answer and refusal for
/// throw, over representative families of calls.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnmodelledException =

    let private library : Assembly = typeof<UnixError>.Assembly

    /// The public types whose names end in `Refusal` or `Fault` and are not
    /// refusals: the client's own buffer, the errors of parsers, flavour rules.
    let private closedValues : Set<string> =
        Set.ofList
            [
                "BufferRefusal"
                "DirectorySourceRefusal"
                "Ipv4PrefixRefusal"
                "SignalMaskRefusal"
                "BindFault"
                "GetCwdDestinationFault"
            ]

    [<Test>]
    let ``no public type is named Refusal or Fault, but the closed values`` () : unit =
        let named =
            library.GetExportedTypes ()
            |> Array.filter (fun (t : Type) -> not t.IsNested)
            |> Array.map (fun (t : Type) -> LibraryNames.withoutArity t.Name)
            |> Array.filter (fun (name : string) -> name.EndsWith "Refusal" || name.EndsWith "Fault")
            |> Set.ofArray

        // Controls, so that this cannot pass by finding nothing.
        named |> shouldContain "BufferRefusal"
        named |> shouldContain "BindFault"

        let faults =
            [
                for name in Set.difference named closedValues do
                    yield
                        $"%s{name} is public. A refusal is internal, and a public entry point raises it (Refusal.orRaise); if %s{name} is a value a client holds rather than a refusal, add it to closedValues"
                for name in Set.difference closedValues named do
                    yield $"%s{name} is listed as a public closed value, and is not one. Remove it from the list"
            ]

        faults |> shouldEqual []

    /// The error type of a `Result`, if `t` is one.
    let private resultError (t : Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<Result<int, int>> then
            Some (t.GetGenericArguments ()).[1]
        else
            None

    /// The `Error`s a public function may answer, none of them a refusal: an
    /// errno, or the error of a parser or a classifier, which says what was
    /// wrong with the client's own input.
    let private publicErrors : Set<string> =
        Set.ofList
            [
                "UnixError"
                "AbsoluteUnixPathError"
                "FileNameError"
                "SimulatedUnixPlatformError"
                "SymlinkTargetError"
                "UnixByteStringDefect"
                "UnixPathError"
                "UnixPathTextDefect"
                "BufferRefusal"
                "Ipv4PrefixRefusal"
                "SignalMaskRefusal"
            ]

    [<Test>]
    let ``no public function answers a refusal`` () : unit =
        let answered =
            library.GetExportedTypes ()
            |> Array.filter (fun (t : Type) -> LibraryNames.kindOf t = Some SourceConstructFlags.Module)
            |> Array.collect (fun (t : Type) ->
                t.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
                |> Array.choose (fun (m : MethodInfo) ->
                    resultError m.ReturnType
                    |> Option.map (fun (error : Type) ->
                        $"%s{LibraryNames.dottedName t}.%s{LibraryNames.sourceName m}",
                        LibraryNames.withoutArity error.Name
                    )
                )
            )
            |> List.ofArray

        // A control: a function whose `Error` is an errno.
        answered |> shouldContain ("UnixSignal.sigaction", "UnixError")

        answered
        |> List.filter (fun (_, error : string) -> not (publicErrors.Contains error))
        |> List.map (fun (name : string, error : string) -> $"%s{name} answers Error of %s{error}")
        |> shouldEqual []

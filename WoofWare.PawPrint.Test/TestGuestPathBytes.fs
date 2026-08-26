namespace WoofWare.PawPrint.Test

open System
open System.Text
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// PawPrint's half of a path argument: the rules are `PathArgument.parse`'s and
/// are tested in `WoofWare.PosixKernel.Test.TestPathArgument`, so what is left
/// here is the message a refusal composes, which needs facts the library never
/// has — which entry point asked, what the bytes actually were, and which
/// managed caller could have produced them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGuestPathBytes =

    let private linux : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    [<Test>]
    let ``a path that cannot be represented names the caller and its bytes`` () : unit =
        // The library says why no answer exists; only PawPrint can say which
        // entry point was asked and what it was asked with, and a crash without
        // those is one nobody can act on.
        let invalid = [| 0x2Fuy ; 0x61uy ; 0xFFuy |]

        let exn =
            Assert.Throws<Exception> (fun () ->
                NativeSystemNative.parseGuestPathBytes "SystemNative_Open" linux invalid
                |> ignore<Result<UnixPath, UnixError>>
            )

        exn.Message |> shouldContainText "SystemNative_Open"
        // The bytes, in hex, so that the offending one is identifiable: it is by
        // construction not printable.
        exn.Message |> shouldContainText "2F 61 FF"
        // And the reachability, which is a fact about CoreLib rather than about
        // any kernel: its own callers encode from a string and so cannot produce
        // this.
        exn.Message |> shouldContainText "hand-rolled P/Invoke"

    [<Test>]
    let ``an answerable path argument comes back as an answer`` () : unit =
        // The other two arms are not refusals and must not crash: PawPrint hands
        // ENAMETOOLONG to the guest as an errno, and a good path through.
        let tooLong = Encoding.UTF8.GetBytes (String.replicate 5000 "a")

        NativeSystemNative.parseGuestPathBytes "SystemNative_Open" linux tooLong
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

        match NativeSystemNative.parseGuestPathBytes "SystemNative_Open" linux (Encoding.UTF8.GetBytes "/etc") with
        | Ok path -> UnixPath.toString path |> shouldEqual "/etc"
        | Error error -> failwith $"expected a path, got %O{error}"

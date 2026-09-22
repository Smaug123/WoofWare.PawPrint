namespace WoofWare.PawPrint.Test

open System
open System.Text
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// PawPrint's half of a path argument: the rules are `PathArgument.parse`'s and
/// are tested in `WoofWare.PosixKernel.Test.TestPathArgument`, so what is left
/// here is what PawPrint does with each outcome.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGuestPathBytes =

    let private linux : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    [<Test>]
    let ``a path that is not valid UTF-8 is a path, byte for byte`` () : unit =
        // A real Unix names a file with any NUL-free bytes, and a hand-rolled
        // P/Invoke can pass such bytes, so this is an answer rather than a crash.
        let bytes = [| 0x2Fuy ; 0x74uy ; 0x6Duy ; 0x70uy ; 0x2Fuy ; 0xFFuy |]

        match NativeSystemNative.parseGuestPathBytes "SystemNative_Open" linux bytes with
        | Ok path ->
            UnixPath.toByteString path
            |> UnixByteString.toBytes
            |> Seq.toArray
            |> shouldEqual bytes
        | Error error -> failwith $"expected a path, got %O{error}"

    [<Test>]
    let ``bytes holding a NUL crash, naming the caller and the offset`` () : unit =
        // PawPrint reads a guest's path up to its NUL, so bytes holding one mean
        // that read went wrong: an interpreter bug, and the crash must say where.
        let exn =
            Assert.Throws<Exception> (fun () ->
                NativeSystemNative.parseGuestPathBytes "SystemNative_Open" linux [| 0x2Fuy ; 0x00uy ; 0x61uy |]
                |> ignore<Result<UnixPath, UnixError>>
            )

        exn.Message |> shouldContainText "SystemNative_Open"
        exn.Message |> shouldContainText "offset 1"

    [<Test>]
    let ``an answerable path argument comes back as an answer`` () : unit =
        // The other two arms are not refusals and must not crash: PawPrint hands
        // ENAMETOOLONG to the guest as an errno, and a good path through.
        let tooLong = Encoding.UTF8.GetBytes (String.replicate 5000 "a")

        NativeSystemNative.parseGuestPathBytes "SystemNative_Open" linux tooLong
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

        match NativeSystemNative.parseGuestPathBytes "SystemNative_Open" linux (Encoding.UTF8.GetBytes "/etc") with
        | Ok path -> UnixPath.tryToString path |> shouldEqual (Some "/etc")
        | Error error -> failwith $"expected a path, got %O{error}"

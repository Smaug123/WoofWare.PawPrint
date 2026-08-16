namespace WoofWare.PawPrint.Test

open System
open System.Text
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `PATH_MAX` is the one length rule the resolver can never see: it binds the
/// pathname *as the guest passed it*, in bytes, before any parsing. `UnixPath`
/// has already lost that — it collapses repeated separators, records a trailing
/// one as a flag, and counts UTF-16 — so the rule lives at the syscall boundary,
/// in `NativeSystemNative.parseGuestPathBytes`, and is tested here against that
/// function directly rather than through a resolution.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGuestPathBytes =

    let private darwin : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64

    let private linux : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    /// The bytes a guest's `char*` would hold, without the NUL:
    /// `readNullTerminatedBytes` has already dropped it, which is why the limit
    /// (which counts it) is compared against `pathMaxBytes - 1`.
    let private bytesOf (s : string) : byte[] = Encoding.UTF8.GetBytes s

    /// A path of exactly `n` bytes that would resolve if it were short enough:
    /// "./" repeated, so every component is a "." and none can hit NAME_MAX.
    let private ofLength (n : int) : string =
        let unit = "./"
        let s = String.replicate ((n / 2) + 1) unit
        s.Substring (0, n)

    [<Test>]
    let ``the length probe really produces the length it claims`` () : unit =
        // Guards the guard: every assertion below is about a byte count, so a
        // helper that quietly produced 1022 bytes when asked for 1023 would make
        // the boundary tests pass against the wrong boundary.
        for n in [ 255 ; 1023 ; 1024 ; 4095 ; 4096 ] do
            (ofLength n |> bytesOf).Length |> shouldEqual n

    [<Test>]
    let ``PATH_MAX binds the argument at each platform's own boundary`` () : unit =
        // Measured: on macOS an argument of 1023 bytes resolves and 1024 is
        // ENAMETOOLONG; on Linux 4095 resolves and 4096 is ENAMETOOLONG. The
        // limit counts the NUL, which is why the usable length is one less than
        // the number in the header.
        let ok (limits : PathLimits) (n : int) : unit =
            match NativeSystemNative.parseGuestPathBytes "test" limits (bytesOf (ofLength n)) with
            | Ok _ -> ()
            | Error error -> failwith $"a %d{n}-byte path was refused with %O{error}"

        let tooLong (limits : PathLimits) (n : int) : unit =
            NativeSystemNative.parseGuestPathBytes "test" limits (bytesOf (ofLength n))
            |> shouldEqual (Error UnixError.ENAMETOOLONG)

        ok darwin 1023
        tooLong darwin 1024

        ok linux 4095
        tooLong linux 4096

        // ...and the platforms genuinely differ in between, which is the whole
        // reason this is derived from the flavour rather than a constant.
        ok linux 1024
        tooLong darwin 4095

    [<Test>]
    let ``the limit is bytes, not characters`` () : unit =
        // 512 two-byte characters are 1024 bytes but only 512 UTF-16 units, so a
        // `String.Length` implementation would accept this on Darwin.
        let multiByte = String.replicate 512 "é"
        (bytesOf multiByte).Length |> shouldEqual 1024

        NativeSystemNative.parseGuestPathBytes "test" darwin (bytesOf multiByte)
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

    [<Test>]
    let ``an over-long path is refused before its bytes are decoded`` () : unit =
        // Ordering: a real kernel checks the length in
        // `getname`/`copyinstr` as it copies the string in, long before anything
        // interprets it. PawPrint aborts the interpreter on a path that is not
        // valid UTF-8, because it cannot represent one — so if the decode ran
        // first, a path that a real kernel rejects cheaply would instead take
        // the whole run down.
        let invalid = Array.append (bytesOf (ofLength 2000)) [| 0xFFuy |]

        NativeSystemNative.parseGuestPathBytes "test" darwin invalid
        |> shouldEqual (Error UnixError.ENAMETOOLONG)

        // The same bytes under a kernel whose PATH_MAX they fit *do* reach the
        // decode, and that is the abort — which is what proves the check above
        // is doing the work rather than the input being harmless.
        let exn =
            Assert.Throws<Exception> (fun () ->
                NativeSystemNative.parseGuestPathBytes "test" linux invalid
                |> ignore<Result<UnixPath, UnixError>>
            )

        exn.Message |> shouldContainText "not valid UTF-8"

    [<Test>]
    let ``a path within the limit parses to what it says`` () : unit =
        match NativeSystemNative.parseGuestPathBytes "test" linux (bytesOf "/etc/hostname") with
        | Error error -> failwith $"expected success, got %O{error}"
        | Ok path -> UnixPath.toString path |> shouldEqual "/etc/hostname"
